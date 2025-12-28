package labyrinth.server.game;

import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.Achievement;
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
import labyrinth.server.game.util.GameTimer;
import labyrinth.server.messaging.events.EventPublisher;
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

    public GameService(TreasureCardFactory treasureCardFactory,
                       BoardFactory boardFactory,
                       EventPublisher eventPublisher,
                       TaskScheduler scheduler) {

        this.treasureCardFactory = treasureCardFactory;
        this.boardFactory = boardFactory;
        this.eventPublisher = eventPublisher;

        var gameTimer = new GameTimer(scheduler);
        game = new Game(gameTimer);
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

            var board = boardFactory.createBoard(gameConfig.boardWidth(), gameConfig.boardHeight(), gameConfig.totalBonusCount());
            var treasureCards = treasureCardFactory.createTreasureCards(gameConfig.treasureCardCount(), playersCount);

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

            if (result.runnerAchieved()) {
                var achievementEvent = new AchievementUnlockedEvent(player, Achievement.RUNNER);
                eventPublisher.publishAsync(achievementEvent);
            }

            if(result.treasureCollected()){
                var treasureCardEvent = new NextTreasureCardEvent(player, player.getCurrentTreasureCard());
                eventPublisher.publishAsync(treasureCardEvent);
            }

            if(result.gameOver()){
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

            if (pushResult.pusherAchieved()) {
                var achievementEvent = new AchievementUnlockedEvent(player, Achievement.PUSHER);
                eventPublisher.publishAsync(achievementEvent);
            }

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
            var useSuccessfull = game.useBeamBonus(row, col, player);

            if(useSuccessfull){
                player.getStatistics().increaseScore(PointRewards.REWARD_SHIFT_TILE);
            }

        } finally {
            rwLock.writeLock().unlock();
        }
    }


    public void useSwapBonus(Player currentPlayer, Player targetPlayer) {
        rwLock.writeLock().lock();
        try {
            game.useSwapBonus(currentPlayer, targetPlayer);
        } finally {
            rwLock.writeLock().unlock();
        }
    }


    public void usePushTwiceBonus(Player player) {
        rwLock.writeLock().lock();
        try {
            game.usePushTwiceBonus(player);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void usePushFixedBonus(Player player) {
        rwLock.writeLock().lock();
        try {
            game.usePushFixedBonus(player);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    // Generic helper: run a function under the Game read-lock. Keeps locking centralized
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
