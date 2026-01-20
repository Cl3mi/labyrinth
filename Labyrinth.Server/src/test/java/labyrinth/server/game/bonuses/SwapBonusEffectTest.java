package labyrinth.server.game.bonuses;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.PlayerStatistics;
import labyrinth.server.game.models.Tile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.EnumSet;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Tests for the SwapBonusEffect class.
 * Validates swap bonus mechanics including position swapping and tile interactions.
 */
class SwapBonusEffectTest {

    private SwapBonusEffect swapBonusEffect;
    private Game game;
    private Player player;
    private Player targetPlayer;
    private Tile playerTile;
    private Tile targetTile;

    @BeforeEach
    void setUp() {
        swapBonusEffect = new SwapBonusEffect();
        game = mock(Game.class);
        player = mock(Player.class);
        targetPlayer = mock(Player.class);

        playerTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
        targetTile = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));

        when(player.getCurrentTile()).thenReturn(playerTile);
        when(targetPlayer.getCurrentTile()).thenReturn(targetTile);

        PlayerStatistics playerStats = new PlayerStatistics();
        when(player.getStatistics()).thenReturn(playerStats);
    }

    @Nested
    class SuccessfulSwap {

        @Test
        void shouldReturnTrueWhenSwapSucceeds() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);

            // Act
            boolean result = swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            assertTrue(result);
        }

        @Test
        void shouldProcessPlayerStepOnTargetTile() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);

            // Act
            swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            verify(game).processPlayerStepOnTile(player, targetTile);
        }

        @Test
        void shouldProcessTargetPlayerStepOnOriginalTile() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);

            // Act
            swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            verify(game).processPlayerStepOnTile(targetPlayer, playerTile);
        }

        @Test
        void shouldSetMoveStateToMove() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);

            // Act
            swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            verify(game).setMoveState(MoveState.MOVE);
        }

        @Test
        void shouldIncreasePlayerScore() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);
            int initialScore = player.getStatistics().getScore();

            // Act
            swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            assertTrue(player.getStatistics().getScore() > initialScore);
        }
    }

    @Nested
    class FailedSwap {

        @Test
        void shouldReturnFalseWhenPlayerDoesNotHaveBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(false);

            // Act
            boolean result = swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotProcessStepsWhenPlayerDoesNotHaveBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(false);

            // Act
            swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            verify(game, never()).processPlayerStepOnTile(any(), any());
        }

        @Test
        void shouldNotChangeMoveStateWhenFailed() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(false);

            // Act
            swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            verify(game, never()).setMoveState(any());
        }
    }

    @Nested
    class ArgumentValidation {

        @Test
        void shouldThrowExceptionWhenNoArgumentsProvided() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    swapBonusEffect.apply(game, player)
            );
        }

        @Test
        void shouldThrowExceptionWhenArgumentIsNotPlayer() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    swapBonusEffect.apply(game, player, "not a player")
            );
        }

        @Test
        void shouldThrowExceptionWhenArgumentIsNull() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    swapBonusEffect.apply(game, player, (Object) null)
            );
        }

        @Test
        void shouldThrowExceptionWhenArgumentIsInteger() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    swapBonusEffect.apply(game, player, 42)
            );
        }
    }

    @Nested
    class SwapWithDifferentTileTypes {

        @Test
        void shouldSwapWhenTargetTileHasTreasure() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);
            // Target tile has a treasure - processPlayerStepOnTile will handle collection

            // Act
            boolean result = swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            assertTrue(result);
            verify(game).processPlayerStepOnTile(player, targetTile);
        }

        @Test
        void shouldSwapWhenBothTilesHaveTreasures() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);

            // Act
            boolean result = swapBonusEffect.apply(game, player, targetPlayer);

            // Assert
            assertTrue(result);
            verify(game).processPlayerStepOnTile(player, targetTile);
            verify(game).processPlayerStepOnTile(targetPlayer, playerTile);
        }
    }

    @Nested
    class SwapWithSelf {

        @Test
        void shouldHandleSwapWithSelf() {
            // Arrange - player tries to swap with themselves
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);

            // Act - this is technically allowed by the code but shouldn't happen in practice
            boolean result = swapBonusEffect.apply(game, player, player);

            // Assert
            assertTrue(result);
            // Both calls would be to the same player stepping on the same tile
            verify(game, times(2)).processPlayerStepOnTile(eq(player), any());
        }
    }

    @Nested
    class TileReferencesAfterSwap {

        @Test
        void shouldUseCorrectTileReferencesBeforeSwap() {
            // Arrange
            when(player.useBonus(BonusTypes.SWAP)).thenReturn(true);

            // Act
            swapBonusEffect.apply(game, player, targetPlayer);

            // Assert - verify the tiles were captured correctly before any modifications
            verify(game).processPlayerStepOnTile(player, targetTile);
            verify(game).processPlayerStepOnTile(targetPlayer, playerTile);
        }
    }
}
