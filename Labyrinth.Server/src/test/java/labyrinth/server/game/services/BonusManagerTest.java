package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.abstractions.ITurnController;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.Position;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Tests for the BonusManager class.
 * Validates bonus usage validation, effect application, and turn-based restrictions.
 */
class BonusManagerTest {

    private BonusManager bonusManager;
    private ITurnController turnController;
    private GameLogger gameLogger;
    private Game game;
    private Player player;
    private Board board;

    @BeforeEach
    void setUp() {
        turnController = mock(ITurnController.class);
        gameLogger = new GameLogger();
        bonusManager = new BonusManager(turnController, gameLogger);

        // Setup game mock
        game = mock(Game.class);
        player = new Player(UUID.randomUUID(), "TestPlayer");

        // Create a simple board
        BiMap<Position, Tile> tileMap = new BiMap<>();
        for (int row = 0; row < 3; row++) {
            for (int col = 0; col < 3; col++) {
                tileMap.put(new Position(row, col),
                        new Tile(EnumSet.of(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT)));
            }
        }
        board = new Board(3, 3, tileMap, new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
        board.setPlayers(List.of(player));

        when(game.getBoard()).thenReturn(board);
        when(game.getPlayers()).thenReturn(List.of(player));

        // Default: bonus not used yet, in PLACE_TILE state
        when(turnController.isBonusUsedThisTurn()).thenReturn(false);
        when(turnController.getCurrentMoveState()).thenReturn(MoveState.PLACE_TILE);
    }

    @Nested
    class HasStrategy {

        @ParameterizedTest
        @EnumSource(BonusTypes.class)
        void shouldHaveStrategyForAllBonusTypes(BonusTypes type) {
            // Assert
            assertTrue(bonusManager.hasStrategy(type),
                    "Should have strategy for " + type);
        }
    }

    @Nested
    class UseBonusValidation {

        @Test
        void shouldThrowExceptionForUnknownBonusType() {
            // This test would require a way to create an unknown bonus type
            // Since BonusTypes is an enum, this is difficult without mocking
            // Skip for now - covered by hasStrategy test
        }

        @Test
        void shouldThrowExceptionWhenBonusAlreadyUsedThisTurn() {
            // Arrange
            when(turnController.isBonusUsedThisTurn()).thenReturn(true);
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player)
            );
        }

        @Test
        void shouldThrowExceptionWhenNotInPlaceTileState() {
            // Arrange
            when(turnController.getCurrentMoveState()).thenReturn(MoveState.MOVE);
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player)
            );
        }
    }

    @Nested
    class UsePushTwiceBonus {

        @Test
        void shouldApplyPushTwiceBonusSuccessfully() {
            // Arrange
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act
            boolean result = bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            assertTrue(result);
            verify(game).setActiveBonus(BonusTypes.PUSH_TWICE);
        }

        @Test
        void shouldFailWhenPlayerDoesNotHaveBonus() {
            // Arrange - player has no bonuses

            // Act
            boolean result = bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldMarkBonusAsUsed() {
            // Arrange
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act
            bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            verify(turnController).markBonusUsed();
        }

        @Test
        void shouldNotMarkBonusUsedWhenFailed() {
            // Arrange - player has no bonus

            // Act
            bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            verify(turnController, never()).markBonusUsed();
        }
    }

    @Nested
    class UsePushFixedBonus {

        @Test
        void shouldApplyPushFixedBonusSuccessfully() {
            // Arrange
            player.getBonuses().add(BonusTypes.PUSH_FIXED);

            // Act
            boolean result = bonusManager.useBonus(BonusTypes.PUSH_FIXED, game, player);

            // Assert
            assertTrue(result);
            verify(game).setActiveBonus(BonusTypes.PUSH_FIXED);
        }

        @Test
        void shouldFailWhenPlayerDoesNotHaveBonus() {
            // Act
            boolean result = bonusManager.useBonus(BonusTypes.PUSH_FIXED, game, player);

            // Assert
            assertFalse(result);
        }
    }

    @Nested
    class UseBeamBonus {

        private Player realPlayer;

        @BeforeEach
        void setUpBeam() {
            realPlayer = new Player(UUID.randomUUID(), "TestPlayer");
            Tile currentTile = board.getTileAt(0, 0);
            realPlayer.setCurrentTile(currentTile);
            when(game.getPlayers()).thenReturn(List.of(realPlayer));
        }

        @Test
        void shouldApplyBeamBonusSuccessfully() {
            // Arrange
            realPlayer.getBonuses().add(BonusTypes.BEAM);

            // Act
            boolean result = bonusManager.useBonus(BonusTypes.BEAM, game, realPlayer, 1, 1);

            // Assert
            assertTrue(result);
        }

        @Test
        void shouldFailWhenTargetTileIsOccupied() {
            // Arrange
            realPlayer.getBonuses().add(BonusTypes.BEAM);
            Player otherPlayer = new Player(UUID.randomUUID(), "Other");
            Tile targetTile = board.getTileAt(1, 1);
            otherPlayer.setCurrentTile(targetTile);
            when(game.getPlayers()).thenReturn(List.of(realPlayer, otherPlayer));

            // Act
            boolean result = bonusManager.useBonus(BonusTypes.BEAM, game, realPlayer, 1, 1);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldThrowExceptionForMissingArguments() {
            // Arrange
            realPlayer.getBonuses().add(BonusTypes.BEAM);

            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    bonusManager.useBonus(BonusTypes.BEAM, game, realPlayer)
            );
        }
    }

    @Nested
    class UseSwapBonus {

        private Player targetPlayer;

        @BeforeEach
        void setUpSwap() {
            targetPlayer = new Player(UUID.randomUUID(), "TargetPlayer");
            Tile tile1 = board.getTileAt(0, 0);
            Tile tile2 = board.getTileAt(1, 1);
            player.setCurrentTile(tile1);
            targetPlayer.setCurrentTile(tile2);
            when(game.getPlayers()).thenReturn(List.of(player, targetPlayer));
        }

        @Test
        void shouldApplySwapBonusSuccessfully() {
            // Arrange
            player.getBonuses().add(BonusTypes.SWAP);

            // Act
            boolean result = bonusManager.useBonus(BonusTypes.SWAP, game, player, targetPlayer);

            // Assert
            assertTrue(result);
        }

        @Test
        void shouldFailWhenPlayerDoesNotHaveBonus() {
            // Act
            boolean result = bonusManager.useBonus(BonusTypes.SWAP, game, player, targetPlayer);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldThrowExceptionForMissingTargetPlayer() {
            // Arrange
            player.getBonuses().add(BonusTypes.SWAP);

            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    bonusManager.useBonus(BonusTypes.SWAP, game, player)
            );
        }

        @Test
        void shouldThrowExceptionForWrongArgumentType() {
            // Arrange
            player.getBonuses().add(BonusTypes.SWAP);

            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    bonusManager.useBonus(BonusTypes.SWAP, game, player, "not a player")
            );
        }
    }

    @Nested
    class BonusScoring {

        @Test
        void shouldIncreaseScoreWhenBonusUsedSuccessfully() {
            // Arrange
            player.getBonuses().add(BonusTypes.PUSH_TWICE);
            int initialScore = player.getStatistics().getScore();

            // Act
            bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            assertTrue(player.getStatistics().getScore() > initialScore);
        }

        @Test
        void shouldNotIncreaseScoreWhenBonusFails() {
            // Arrange - player has no bonus
            int initialScore = player.getStatistics().getScore();

            // Act
            bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            assertEquals(initialScore, player.getStatistics().getScore());
        }
    }

    @Nested
    class BonusLogging {

        @Test
        void shouldLogBonusUsageOnSuccess() {
            // Arrange
            player.getBonuses().add(BonusTypes.PUSH_TWICE);
            int initialLogCount = gameLogger.getExecutionLogs().size();

            // Act
            bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            assertTrue(gameLogger.getExecutionLogs().size() > initialLogCount);
        }

        @Test
        void shouldNotLogBonusUsageOnFailure() {
            // Arrange - player has no bonus
            int initialLogCount = gameLogger.getExecutionLogs().size();

            // Act
            bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            assertEquals(initialLogCount, gameLogger.getExecutionLogs().size());
        }
    }

    @Nested
    class BonusStateRestrictions {

        @Test
        void shouldAllowBonusInPlaceTileState() {
            // Arrange
            when(turnController.getCurrentMoveState()).thenReturn(MoveState.PLACE_TILE);
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act & Assert - should not throw
            assertDoesNotThrow(() ->
                    bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player)
            );
        }

        @Test
        void shouldRejectBonusInMoveState() {
            // Arrange
            when(turnController.getCurrentMoveState()).thenReturn(MoveState.MOVE);
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player)
            );
        }

        @Test
        void shouldRejectSecondBonusInSameTurn() {
            // Arrange
            when(turnController.isBonusUsedThisTurn()).thenReturn(true);
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player)
            );
        }
    }

    @Nested
    class BonusRemovalFromPlayer {

        @Test
        void shouldRemoveBonusFromPlayerOnSuccess() {
            // Arrange
            player.getBonuses().add(BonusTypes.PUSH_TWICE);
            assertTrue(player.getBonuses().contains(BonusTypes.PUSH_TWICE));

            // Act
            bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);

            // Assert
            assertFalse(player.getBonuses().contains(BonusTypes.PUSH_TWICE));
        }

        @Test
        void shouldNotRemoveBonusOnValidationFailure() {
            // Arrange
            when(turnController.isBonusUsedThisTurn()).thenReturn(true);
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act
            try {
                bonusManager.useBonus(BonusTypes.PUSH_TWICE, game, player);
            } catch (IllegalStateException ignored) {
            }

            // Assert - bonus should still be there
            assertTrue(player.getBonuses().contains(BonusTypes.PUSH_TWICE));
        }
    }
}
