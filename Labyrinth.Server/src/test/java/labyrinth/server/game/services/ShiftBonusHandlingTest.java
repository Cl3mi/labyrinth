package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.SimpleAiStrategy;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.factories.BonusFactory;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static labyrinth.server.game.GameTestHelper.createGame;
import static org.mockito.Mockito.mock;

/**
 * Tests for bonus handling logic during shift operations.
 * Validates that the Game.shift() method correctly handles bonus state transitions.
 */
class ShiftBonusHandlingTest {

    private Game game;
    private Player player;

    @BeforeEach
    void setUp() {
        var bonusFactory = new BonusFactory();
        var distributionService = new TreasureBonusDistributionService(bonusFactory);
        var gameInitializer = new GameInitializerService(distributionService);

        game = createGame(mock(IGameTimer.class), new SimpleAiStrategy(), new GameLogger(), gameInitializer);
        player = game.join("TestPlayer");
        game.join("Player2");

        List<TreasureCard> cards = new ArrayList<>();
        IntStream.range(0, 24).forEach(i -> cards.add(new TreasureCard(i, "Card" + i, "img")));
        Board board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7);

        game.startGame(GameConfig.getDefault(), cards, board);
        player = game.getCurrentPlayer(); // Get the actual current player after game start
    }

    @Test
    void shift_shouldTransitionToMoveState_afterSuccessfulShift() {
        // Arrange - initial state is PLACE_TILE

        // Act
        game.shift(1, Direction.RIGHT, player);

        // Assert
        assertEquals(MoveState.MOVE, game.getCurrentMoveState(),
                "After a successful shift, move state should transition to MOVE");
    }

    @Test
    void shift_withPushTwiceBonus_shouldAllowAnotherPush() {
        // Arrange
        player.getBonuses().add(BonusTypes.PUSH_TWICE);
        game.useBonus(BonusTypes.PUSH_TWICE);

        // Act - first shift with PUSH_TWICE active
        var result = game.shift(1, Direction.RIGHT, player);

        // Assert
        assertTrue(result.shiftSuccess(), "Shift should succeed");
        assertEquals(MoveState.PLACE_TILE, game.getCurrentMoveState(),
                "PUSH_TWICE bonus should allow staying in PLACE_TILE state for another push");
    }

    @Test
    void shift_withPushTwiceBonus_shouldConsumeBonus() {
        // Arrange
        player.getBonuses().add(BonusTypes.PUSH_TWICE);
        game.useBonus(BonusTypes.PUSH_TWICE);
        BonusTypes activeBonusBeforeShift = game.getActiveBonus();

        // Act
        game.shift(1, Direction.RIGHT, player);

        // Assert
        assertEquals(BonusTypes.PUSH_TWICE, activeBonusBeforeShift, "PUSH_TWICE was active before shift");
        assertNull(game.getActiveBonus(), "PUSH_TWICE bonus should be consumed after shift");
    }

    @Test
    void shift_withPushFixedBonus_shouldAllowShiftingFixedTiles() {
        // Arrange
        player.getBonuses().add(BonusTypes.PUSH_FIXED);
        game.useBonus(BonusTypes.PUSH_FIXED);

        // Act
        var result = game.shift(0, Direction.RIGHT, player); // Row 0 contains fixed tiles

        // Assert
        assertTrue(result.shiftSuccess(), "Should be able to shift row with fixed tiles when PUSH_FIXED is active");
    }

    @Test
    void shift_withPushFixedBonus_shouldConsumeBonus() {
        // Arrange
        player.getBonuses().add(BonusTypes.PUSH_FIXED);
        game.useBonus(BonusTypes.PUSH_FIXED);
        BonusTypes activeBonusBeforeShift = game.getActiveBonus();

        // Act
        game.shift(0, Direction.RIGHT, player);

        // Assert
        assertEquals(BonusTypes.PUSH_FIXED, activeBonusBeforeShift, "PUSH_FIXED was active before shift");
        assertNull(game.getActiveBonus(), "PUSH_FIXED bonus should be consumed after shift");
        assertEquals(MoveState.MOVE, game.getCurrentMoveState(),
                "Should transition to MOVE state after consuming PUSH_FIXED");
    }

    @Test
    void shift_normalShift_shouldNotAffectBonusState() {
        // Arrange - no bonus active
        assertNull(game.getActiveBonus(), "No bonus should be active initially");

        // Act
        game.shift(1, Direction.RIGHT, player);

        // Assert
        assertNull(game.getActiveBonus(), "No bonus should be active after normal shift");
        assertEquals(MoveState.MOVE, game.getCurrentMoveState(), "Should transition to MOVE state");
    }

    @Test
    void shift_shouldIncreaseTilesPushedStatistic() {
        // Arrange
        int tilesPushedBefore = player.getStatistics().getTilesPushed();

        // Act
        game.shift(1, Direction.RIGHT, player);

        // Assert
        assertEquals(tilesPushedBefore + 1, player.getStatistics().getTilesPushed(),
                "Tiles pushed statistic should increase by 1");
    }

    @Test
    void shift_shouldIncreasePlayerScore() {
        // Arrange
        int scoreBefore = player.getStatistics().getScore();

        // Act
        game.shift(1, Direction.RIGHT, player);

        // Assert
        assertTrue(player.getStatistics().getScore() > scoreBefore,
                "Player score should increase after successful shift");
    }

    @Test
    void shift_failedShift_shouldNotAwardPoints() {
        // Arrange
        int scoreBefore = player.getStatistics().getScore();
        int tilesPushedBefore = player.getStatistics().getTilesPushed();

        // Act - try to shift a row with fixed tiles (should fail)
        var result = game.shift(0, Direction.RIGHT, player);

        // Assert
        assertFalse(result.shiftSuccess(), "Shift should fail on row with fixed tiles");
        assertEquals(scoreBefore, player.getStatistics().getScore(),
                "Score should not increase on failed shift");
        assertEquals(tilesPushedBefore, player.getStatistics().getTilesPushed(),
                "Tiles pushed should not increase on failed shift");
    }
}
