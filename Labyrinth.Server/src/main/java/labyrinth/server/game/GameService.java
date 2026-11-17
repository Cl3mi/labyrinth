package labyrinth.server.game;

import labyrinth.server.exceptions.GameAlreadyStartedException;
import labyrinth.server.exceptions.UsernameTakenException;
import labyrinth.server.game.ai.SligthlyLessSimpleAiStrategy;
import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.events.*;
import labyrinth.server.game.factories.BoardFactory;
import labyrinth.server.game.factories.TreasureCardFactory;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import labyrinth.server.game.services.*;
import labyrinth.server.game.util.GameTimer;
import labyrinth.server.messaging.events.EventPublisher;
import labyrinth.server.messaging.events.abstractions.IEvent;
import labyrinth.server.persistence.GameRecorder;
import labyrinth.server.persistence.GameRecords;
import labyrinth.server.persistence.GameSnapshotMapper;
import labyrinth.server.persistence.SessionTokenSource;
import lombok.Getter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Function;

@Service
public class GameService {

    @Getter
    private final Game game;
    private final ReentrantReadWriteLock rwLock = new ReentrantReadWriteLock();

    private final TreasureCardFactory treasureCardFactory;
    private final BoardFactory boardFactory;
    private final EventPublisher eventPublisher;
    private final GameRecorder gameRecorder;
    private final SessionTokenSource sessionTokens;
    private final TaskScheduler scheduler;

    /** ID of the running game in the database, null while in the lobby. */
    private UUID currentGameId;
    private long snapshotVersion;

    private static final Logger log = LoggerFactory.getLogger(GameService.class);

    public GameService(TreasureCardFactory treasureCardFactory,
                       BoardFactory boardFactory,
                       EventPublisher eventPublisher,
                       TaskScheduler scheduler,
                       GameInitializerService gameInitializer,
                       GameRecorder gameRecorder,
                       SessionTokenSource sessionTokens) {

        this.treasureCardFactory = treasureCardFactory;
        this.boardFactory = boardFactory;
        this.eventPublisher = eventPublisher;
        this.gameRecorder = gameRecorder;
        this.sessionTokens = sessionTokens;
        this.scheduler = scheduler;

        var gameTimer = new GameTimer(scheduler);
        var gameLogger = new GameLogger();

        var aiStrategy = new SligthlyLessSimpleAiStrategy(this);
        var playerRegistry = new PlayerRegistry(4);
        var turnController = new TurnController(gameTimer, gameLogger);
        var movementManager = new MovementManager();
        var achievementService = new AchievementService();
        var bonusManager = new BonusManager(turnController, gameLogger);

        game = new Game(
                playerRegistry,
                turnController,
                movementManager,
                achievementService,
                bonusManager,
                gameTimer,
                aiStrategy,
                gameLogger,
                gameInitializer
        );

        turnController.setOnNextPlayer(player -> {
            publishEvent(new NextPlayerEvent(player));
            if (!rwLock.isWriteLockedByCurrentThread() && scheduler != null) {
                // turn timer fired outside the lock: snapshot after the new turn has started
                scheduler.schedule(this::recordSnapshotWithLock, Instant.now());
            }
        });
    }

    public Player join(String username) throws UsernameTakenException, GameAlreadyStartedException {
        rwLock.writeLock().lock();
        try {
            return game.join(username);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void leave(Player player) {
        rwLock.writeLock().lock();
        try {
            game.leave(player);

            if (!game.anyPlayerActive()) {
                log.info("No human players left, resetting game to LOBBY");
                returnToLobbyNoLock();
            }

            publishEvent(new PlayerLeftEvent());
            recordSnapshot();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public int getMaxPlayers() {
        return game.getMaxPlayers();
    }

    public List<Player> getPlayers() {
        rwLock.readLock().lock();
        try {
            return List.copyOf(game.getPlayers());
        } finally {
            rwLock.readLock().unlock();
        }
    }

    public RoomState getGameState() {
        rwLock.readLock().lock();
        try {
            return game.getRoomState();
        } finally {
            rwLock.readLock().unlock();
        }
    }

    /**
     * Alias for getGameState() for backward compatibility with tests.
     */
    public RoomState getRoomState() {
        return getGameState();
    }

    public Player getPlayer(UUID playerId) {
        rwLock.readLock().lock();
        try {
            return game.getPlayer(playerId);
        } finally {
            rwLock.readLock().unlock();
        }
    }

    public Board getBoard() {
        rwLock.readLock().lock();
        try {
            return game.getBoard();
        } finally {
            rwLock.readLock().unlock();
        }
    }

    public void startGame(GameConfig gameConfig) {
        rwLock.writeLock().lock();
        try {
            int playersCount = game.getPlayers().size();

            var board = boardFactory.createBoard(gameConfig.boardWidth(), gameConfig.boardHeight());

            int totalTreasures = gameConfig.treasureCardCount();
            var treasureCards = treasureCardFactory.createTreasureCards(totalTreasures, playersCount);

            game.startGame(gameConfig, treasureCards, board);
            recordGameStarted(gameConfig);
            recordSnapshot();

            publishEvent(new GameStartedEvent());

            for(var player : game.getPlayers()) {
                var treasureCardEvent = new NextTreasureCardEvent(player, player.getCurrentTreasureCard());
                publishEvent(treasureCardEvent);
            }
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    /** Continues a game from the database after a restart of this instance. */
    public void restore(UUID gameId, long snapshotVersion, GameSnapshotMapper.RestoredGame r) {
        rwLock.writeLock().lock();
        try {
            game.restore(r.config(), r.board(), r.players(), r.gameStartTime(), r.activeBonus(),
                    r.currentPlayerIndex(), r.moveState(), r.bonusUsedThisTurn(), r.lastShift());
            currentGameId = gameId;
            this.snapshotVersion = snapshotVersion;
            log.info("Restored game {} at snapshot version {} with {} players", gameId, snapshotVersion, r.players().size());
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public Player getCurrentPlayer() {
        rwLock.readLock().lock();
        try {
            return game.getCurrentPlayer();
        } finally {
            rwLock.readLock().unlock();
        }
    }

    public boolean movePlayerToTile(int row, int col, Player player) {
        rwLock.writeLock().lock();
        try {
            var result = game.movePlayerToTile(row, col, player);

            if (!result.moveSuccess()) {
                return false;
            }

            if (result.gameOver()) {
                for (var award : game.getEndGameAchievements()) {
                    var achievementEvent = new AchievementUnlockedEvent(award.player(), award.achievement());
                    publishEvent(achievementEvent);
                }

                var players = getPlayers();
                recordGameFinished(players, player);

                publishEvent(new GameOverEvent(players));
                returnToLobbyNoLock();
                return true;
            }

            publishEvent(new PlayerMovedEvent());
            recordSnapshot();

            if (result.treasureCollected()) {
                var treasureCardEvent = new NextTreasureCardEvent(player, player.getCurrentTreasureCard());
                publishEvent(treasureCardEvent);
            }


            return true;
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void rotateExtraTileClockwise(Player player) {
        rwLock.writeLock().lock();
        try {
            game.rotateExtraTileClockwise(player);
            publishEvent(new ExtraTileRotatedEvent());
            recordSnapshot();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public boolean shift(int index, Direction direction, Player player) {
        rwLock.writeLock().lock();
        try {
            var pushResult = game.shift(index, direction, player);

            if (pushResult.shiftSuccess()) {
                publishEvent(new BoardShiftedEvent());
                recordSnapshot();
            }

            return pushResult.shiftSuccess();

        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void enableAiAndMarkDisconnected(Player player) {
        rwLock.writeLock().lock();
        try {
            if(game.getRoomState() != RoomState.IN_GAME) {
                log.warn("Game is not in IN_GAME state, cannot enable AI for player {}", player.getUsername());
                return;
            }

            game.enableAiAndMarkDisconnected(player);

            if (!game.anyPlayerActive()) {
                log.info("No human players left, resetting game to LOBBY");
                returnToLobbyNoLock();
            }
            else {
                publishEvent(new PlayerUpdatedEvent(player));

                if(getCurrentPlayer() == player) {
                    game.performAiTurn(player);
                }
                recordSnapshot();
            }


        } finally {
            rwLock.writeLock().unlock();
        }
    }



    public void toggleAiForPlayer(Player player) {
        rwLock.writeLock().lock();
        try {
            game.toggleAiForPlayer(player);
            publishEvent(new PlayerUpdatedEvent(player));
            recordSnapshot();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void useBeamBonus(int row, int col, Player player) {
        rwLock.writeLock().lock();
        try {
            var success = game.useBonus(BonusTypes.BEAM, row, col);

            if (success) {
                player.getStatistics().increaseScore(PointRewards.REWARD_SHIFT_TILE);
                publishEvent(new BonusUsedEvent());
                recordSnapshot();
            }
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void useSwapBonus(Player targetPlayer) {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.SWAP, targetPlayer);
            publishEvent(new BonusUsedEvent());
            recordSnapshot();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void usePushTwiceBonus() {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.PUSH_TWICE);
            publishEvent(new BonusUsedEvent());
            recordSnapshot();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void usePushFixedBonus() {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.PUSH_FIXED);
            publishEvent(new BonusUsedEvent());
            recordSnapshot();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public Position getCurrentPositionOfPlayer(Player player) {
        rwLock.readLock().lock();
        try {
            return game.getCurrentPositionOfPlayer(player);
        } finally {
            rwLock.readLock().unlock();
        }
    }


    private void returnToLobbyNoLock() {
        if (currentGameId != null) {
            // Still running, so it ended without a result (e.g. all humans left).
            if (gameRecorder != null) {
                gameRecorder.gameAborted(currentGameId, OffsetDateTime.now());
            }
            currentGameId = null;
        }
        game.returnToLobby();
        publishEvent(new ReturnedToLobbyEvent());
    }


    // Generic helper: run a function under the Game read-lock. Keeps locking
    // centralized
    // while avoiding coupling the Game layer to messaging/contract DTO types.
    public <T> T withGameReadLock(Function<Game, T> action) {
        rwLock.readLock().lock();
        try {
            return action.apply(game);
        } finally {
            rwLock.readLock().unlock();
        }
    }


    private void recordGameStarted(GameConfig config) {
        currentGameId = UUID.randomUUID();
        snapshotVersion = 0;
        if (gameRecorder == null) {
            return;
        }
        gameRecorder.gameStarted(new GameRecords.GameStarted(
                currentGameId,
                OffsetDateTime.now(),
                config.boardWidth(),
                config.boardHeight(),
                config.treasureCardCount(),
                config.totalBonusCount(),
                config.turnTimeInSeconds(),
                config.gameDurationInSeconds(),
                game.getPlayers().size()));
    }

    private void recordGameFinished(List<Player> players, Player endedBy) {
        var gameId = currentGameId;
        currentGameId = null;
        if (gameRecorder == null || gameId == null) {
            return;
        }
        var results = PlayerRanking.rank(players).stream()
                .map(r -> {
                    var p = r.player();
                    var stats = p.getStatistics();
                    return new GameRecords.PlayerResult(
                            p.getId(),
                            p.getUsername(),
                            p.isBot(),
                            r.placement(),
                            stats.getScore(),
                            stats.getTreasuresCollected(),
                            stats.getStepsTaken(),
                            stats.getTilesPushed(),
                            stats.getCollectedAchievements().stream().map(Enum::name).toList(),
                            p == endedBy);
                })
                .toList();
        gameRecorder.gameFinished(new GameRecords.GameFinished(gameId, OffsetDateTime.now(), results));
    }

    private void recordSnapshot() {
        if (gameRecorder == null || currentGameId == null || game.getRoomState() != RoomState.IN_GAME) {
            return;
        }
        try {
            Map<UUID, UUID> tokens = sessionTokens == null ? Map.of() : sessionTokens.identifierTokensByPlayerId();
            gameRecorder.gameSnapshot(GameSnapshotMapper.capture(
                    currentGameId, ++snapshotVersion, game, tokens, OffsetDateTime.now()));
        } catch (Exception ex) {
            log.error("Could not take snapshot of game {}: {}", currentGameId, ex.getMessage(), ex);
        }
    }

    private void recordSnapshotWithLock() {
        rwLock.writeLock().lock();
        try {
            recordSnapshot();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    private void publishEvent(IEvent event) {
        if (eventPublisher == null) {
            log.warn("EventPublisher is not initialized. Cannot publish event: {}", event.getClass().getSimpleName());
            return;
        }

        eventPublisher.publish(event);
    }

    public MoveState getCurrentMoveState() {
        return game.getCurrentMoveState();
    }

}

