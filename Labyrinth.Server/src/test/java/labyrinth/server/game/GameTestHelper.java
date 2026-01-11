package labyrinth.server.game;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.AiStrategy;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.services.*;

/**
 * Helper class for creating Game instances in tests.
 * Provides factory methods that handle the new interface-based constructor.
 */
public class GameTestHelper {

    /**
     * Creates a Game instance with all required dependencies.
     * Uses concrete service implementations.
     *
     * @param gameTimer the game timer
     * @param aiStrategy the AI strategy
     * @param gameLogger the game logger
     * @param gameInitializer the game initializer
     * @return a fully configured Game instance
     */
    public static Game createGame(
            IGameTimer gameTimer,
            AiStrategy aiStrategy,
            GameLogger gameLogger,
            GameInitializerService gameInitializer
    ) {
        // Create concrete service instances
        var playerRegistry = new PlayerRegistry(4);
        var turnController = new TurnController(gameTimer, gameLogger);
        var movementManager = new MovementManager();
        var achievementService = new AchievementService();
        var bonusManager = new BonusManager(turnController, gameLogger);

        // Create and return Game with all dependencies
        return new Game(
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
    }

    /**
     * Creates a Game instance with minimal dependencies for simple tests.
     * Uses default gameInitializer if not provided.
     *
     * @param gameTimer the game timer
     * @param aiStrategy the AI strategy
     * @param gameLogger the game logger
     * @return a fully configured Game instance
     */
    public static Game createGame(
            IGameTimer gameTimer,
            AiStrategy aiStrategy,
            GameLogger gameLogger
    ) {
        // Create default game initializer
        var bonusFactory = new labyrinth.server.game.factories.BonusFactory();
        var distributionService = new TreasureBonusDistributionService(bonusFactory);
        var gameInitializer = new GameInitializerService(distributionService);

        return createGame(gameTimer, aiStrategy, gameLogger, gameInitializer);
    }
}
