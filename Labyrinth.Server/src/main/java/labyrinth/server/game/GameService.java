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
import lombok.Getter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Function;

@Service
public class GameService {

    @Getter
    private final Game game;
    private final ReadWriteLock rwLock = new ReentrantReadWriteLock();

    private final TreasureCardFactory treasureCardFactory;
    private final BoardFactory boardFactory;
    private final EventPublisher eventPublisher;

    private static final Logger log = LoggerFactory.getLogger(GameService.class);

    public GameService(TreasureCardFactory treasureCardFactory,
                       BoardFactory boardFactory,
                       EventPublisher eventPublisher,
                       TaskScheduler scheduler,
                       GameInitializerService gameInitializer) {

        this.treasureCardFactory = treasureCardFactory;
        this.boardFactory = boardFactory;
        this.eventPublisher = eventPublisher;

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

        turnController.setOnNextPlayer(player ->
                publishEvent(new NextPlayerEvent(player))
        );
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
                game.returnToLobby();
            }

            publishEvent(new PlayerLeftEvent());
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

            publishEvent(new GameStartedEvent());

            for(var player : game.getPlayers()) {
                var treasureCardEvent = new NextTreasureCardEvent(player, player.getCurrentTreasureCard());
                publishEvent(treasureCardEvent);
            }
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

                publishEvent(new GameOverEvent(players));
                game.returnToLobby();
                return true;
            }

            publishEvent(new PlayerMovedEvent());

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
                game.returnToLobby();
            }
            else {
                publishEvent(new PlayerUpdatedEvent(player));

                if(getCurrentPlayer() == player) {
                    game.performAiTurn(player);
                }
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
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void usePushTwiceBonus() {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.PUSH_TWICE);
            publishEvent(new BonusUsedEvent());
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void usePushFixedBonus() {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.PUSH_FIXED);
            publishEvent(new BonusUsedEvent());
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