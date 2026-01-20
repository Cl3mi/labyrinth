package labyrinth.server.game.bonuses;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Tests for the PushTwiceBonusEffect class.
 * Validates push twice bonus mechanics including activation and state management.
 */
class PushTwiceBonusEffectTest {

    private PushTwiceBonusEffect pushTwiceBonusEffect;
    private Game game;
    private Player player;

    @BeforeEach
    void setUp() {
        pushTwiceBonusEffect = new PushTwiceBonusEffect();
        game = mock(Game.class);
        player = mock(Player.class);
    }

    @Nested
    class SuccessfulApplication {

        @Test
        void shouldReturnTrueWhenPlayerHasBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            boolean result = pushTwiceBonusEffect.apply(game, player);

            // Assert
            assertTrue(result);
        }

        @Test
        void shouldSetActiveBonusToPushTwice() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            pushTwiceBonusEffect.apply(game, player);

            // Assert
            verify(game).setActiveBonus(BonusTypes.PUSH_TWICE);
        }

        @Test
        void shouldConsumePlayerBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            pushTwiceBonusEffect.apply(game, player);

            // Assert
            verify(player).useBonus(BonusTypes.PUSH_TWICE);
        }
    }

    @Nested
    class FailedApplication {

        @Test
        void shouldReturnFalseWhenPlayerDoesNotHaveBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(false);

            // Act
            boolean result = pushTwiceBonusEffect.apply(game, player);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotSetActiveBonusWhenFailed() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(false);

            // Act
            pushTwiceBonusEffect.apply(game, player);

            // Assert
            verify(game, never()).setActiveBonus(any());
        }
    }

    @Nested
    class ArgumentHandling {

        @Test
        void shouldIgnoreExtraArguments() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act & Assert - should not throw
            assertDoesNotThrow(() ->
                    pushTwiceBonusEffect.apply(game, player, "extra", "arguments", 123)
            );
        }

        @Test
        void shouldWorkWithNoExtraArguments() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            boolean result = pushTwiceBonusEffect.apply(game, player);

            // Assert
            assertTrue(result);
        }

        @Test
        void shouldWorkWithEmptyVarargs() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            boolean result = pushTwiceBonusEffect.apply(game, player, new Object[]{});

            // Assert
            assertTrue(result);
        }
    }

    @Nested
    class NoSideEffects {

        @Test
        void shouldNotModifyMoveState() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            pushTwiceBonusEffect.apply(game, player);

            // Assert - PUSH_TWICE doesn't change move state, it allows another push
            verify(game, never()).setMoveState(any());
        }

        @Test
        void shouldNotProcessPlayerStep() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            pushTwiceBonusEffect.apply(game, player);

            // Assert
            verify(game, never()).processPlayerStepOnTile(any(), any());
        }

        @Test
        void shouldNotInteractWithBoard() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true);

            // Act
            pushTwiceBonusEffect.apply(game, player);

            // Assert
            verify(game, never()).getBoard();
        }
    }

    @Nested
    class MultipleApplications {

        @Test
        void shouldAllowMultipleApplicationsIfPlayerHasMultipleBonuses() {
            // Arrange - simulate having the bonus twice
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true, true);

            // Act
            boolean result1 = pushTwiceBonusEffect.apply(game, player);
            boolean result2 = pushTwiceBonusEffect.apply(game, player);

            // Assert
            assertTrue(result1);
            assertTrue(result2);
            verify(game, times(2)).setActiveBonus(BonusTypes.PUSH_TWICE);
        }

        @Test
        void shouldFailSecondApplicationIfOnlyOneBonus() {
            // Arrange - only one bonus available
            when(player.useBonus(BonusTypes.PUSH_TWICE)).thenReturn(true, false);

            // Act
            boolean result1 = pushTwiceBonusEffect.apply(game, player);
            boolean result2 = pushTwiceBonusEffect.apply(game, player);

            // Assert
            assertTrue(result1);
            assertFalse(result2);
        }
    }
}
