package labyrinth.server.game;

import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.events.AchievementUnlockedEvent;
import labyrinth.server.game.events.GameOverEvent;
import labyrinth.server.game.events.NextTreasureCardEvent;
import labyrinth.server.game.factories.BoardFactory;
import labyrinth.server.game.factories.TreasureCardFactory;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.services.GameInitializerService;
import labyrinth.server.game.util.GameTimer;
import labyrinth.server.messaging.events.EventPublisher;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Function;

@Service
public class GameService {

    private final Game game;
    private final ReadWriteLock rwLock = new ReentrantReadWriteLock();

    private final TreasureCardFactory treasureCardFactory;
    private final BoardFactory boardFactory;
    private final EventPublisher eventPublisher;

    private final MessageService messageService;
    private final GameMapper gameMapper;

    private final labyrinth.server.game.ai.SligthlyLessSimpleAiStrategy aiStrategy;

    public GameService(TreasureCardFactory treasureCardFactory,
                       BoardFactory boardFactory,
                       EventPublisher eventPublisher,
                       TaskScheduler scheduler,
                       MessageService messageService,
                       GameMapper gameMapper,
                       GameInitializerService gameInitializer) {

        this.treasureCardFactory = treasureCardFactory;
        this.boardFactory = boardFactory;
        this.eventPublisher = eventPublisher;
        this.messageService = messageService;
        this.gameMapper = gameMapper;

        var gameTimer = new GameTimer(scheduler);
        var gameLogger = new labyrinth.server.game.services.GameLogger();

        this.aiStrategy = new labyrinth.server.game.ai.SligthlyLessSimpleAiStrategy();

        var playerRegistry = new labyrinth.server.game.services.PlayerRegistry(4);
        var turnController = new labyrinth.server.game.services.TurnController(gameTimer, gameLogger);
        var movementManager = new labyrinth.server.game.services.MovementManager();
        var achievementService = new labyrinth.server.game.services.AchievementService();
        var bonusManager = new labyrinth.server.game.services.BonusManager(turnController, gameLogger);

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

        aiStrategy.setBroadcastCallback(new Runnable() {
            @Override
            public void run() {
                broadcastGameStateInternal();
            }
        });

        aiStrategy.setMoveResultCallback(moveResult -> {
            handleAiMoveResult(moveResult);
        });
    }

    /**
     * Handles the result of an AI move, publishing appropriate events.
     */
    private void handleAiMoveResult(labyrinth.server.game.results.MovePlayerToTileResult result) {
        try {
            if (result.treasureCollected()) {
                System.out.println("[GameService] AI collected a treasure!");
            }

            if (result.gameOver()) {
                System.out.println("[GameService] AI triggered game over! Publishing GameOverEvent...");
                rwLock.readLock().lock();
                try {
                    // Publish end-game achievements
                    for (var award : game.getEndGameAchievements()) {
                        var achievementEvent = new AchievementUnlockedEvent(award.player(), award.achievement());
                        eventPublisher.publishAsync(achievementEvent);
                        System.out.println("[GameService] Achievement awarded: " + award.player().getUsername() + " - " + award.achievement());
                    }

                    var gameOverEvent = new GameOverEvent(game.getPlayers());
                    eventPublisher.publishAsync(gameOverEvent);
                } finally {
                    rwLock.readLock().unlock();
                }
            }
        } catch (Exception e) {
            System.err.println("[GameService] Error handling AI move result: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * Broadcasts the current game state to all connected players.
     * Used internally by AI after making moves.
     */
    private void broadcastGameStateInternal() {
        try {
            rwLock.readLock().lock();
            try {
                var gameState = gameMapper.toGameStateDto(game);
                messageService.broadcastToPlayers(gameState);
            } finally {
                rwLock.readLock().unlock();
            }
        } catch (Exception e) {
            System.err.println("Failed to broadcast game state: " + e.getMessage());
            e.printStackTrace();
        }
    }

    public Player join(String username) {
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
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    /**
     * Resets the game to LOBBY state, allowing a new game to be started.
     * Should be called when a finished game needs to be cleaned up.
     */
    public void resetForNewGame() {
        rwLock.writeLock().lock();
        try {
            game.resetForNewGame();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    /**
     * Checks if the game is in FINISHED state.
     */
    public boolean isGameFinished() {
        rwLock.readLock().lock();
        try {
            return game.getRoomState() == RoomState.FINISHED;
        } finally {
            rwLock.readLock().unlock();
        }
    }

    /**
     * Checks if the game is in IN_GAME state (actively playing).
     */
    public boolean isGameInProgress() {
        rwLock.readLock().lock();
        try {
            return game.getRoomState() == RoomState.IN_GAME;
        } finally {
            rwLock.readLock().unlock();
        }
    }

    public int getMaxPlayers() {
        return game.getMAX_PLAYERS();
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
            // If game is finished, reset to lobby first
            if (game.getRoomState() == RoomState.FINISHED) {
                game.returnToLobby();
            }

            int playersCount = 4;

            var board = boardFactory.createBoard(gameConfig.boardWidth(), gameConfig.boardHeight());
            // Multiply treasures per player by actual player count (after AI fill)
            int totalTreasures = gameConfig.treasureCardCount() * playersCount;
            var treasureCards = treasureCardFactory.createTreasureCards(totalTreasures, playersCount);

            game.startGame(gameConfig, treasureCards, board);
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
            boolean moveSuccess = result.moveSuccess();

            if (result.treasureCollected()) {
                var treasureCardEvent = new NextTreasureCardEvent(player, player.getCurrentTreasureCard());
                eventPublisher.publishAsync(treasureCardEvent);
            }

            if (result.gameOver()) {
                // Publish end-game achievements
                for (var award : game.getEndGameAchievements()) {
                    var achievementEvent = new AchievementUnlockedEvent(award.player(), award.achievement());
                    eventPublisher.publishAsync(achievementEvent);
                }

                var gameOverEvent = new GameOverEvent(game.getPlayers());
                eventPublisher.publishAsync(gameOverEvent);
            }

            return moveSuccess;
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void rotateExtraTileClockwise(Player player) {
        rwLock.writeLock().lock();
        try {
            game.rotateExtraTileClockwise(player);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public boolean shift(int index, Direction direction, Player player) {
        rwLock.writeLock().lock();
        try {
            var pushResult = game.shift(index, direction, player);
            return pushResult.shiftSuccess();
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void toggleAiForPlayer(Player player) {
        rwLock.writeLock().lock();
        try {
            game.toggleAiForPlayer(player);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void useBeamBonus(int row, int col, Player player) {
        rwLock.writeLock().lock();
        try {
            var useSuccessfull = game.useBonus(BonusTypes.BEAM, row, col);

            if (useSuccessfull) {
                player.getStatistics().increaseScore(PointRewards.REWARD_SHIFT_TILE);
            }

        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void useSwapBonus(Player currentPlayer, Player targetPlayer) {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.SWAP, targetPlayer);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void usePushTwiceBonus(Player player) {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.PUSH_TWICE);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void usePushFixedBonus(Player player) {
        rwLock.writeLock().lock();
        try {
            game.useBonus(BonusTypes.PUSH_FIXED);
        } finally {
            rwLock.writeLock().unlock();
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
}