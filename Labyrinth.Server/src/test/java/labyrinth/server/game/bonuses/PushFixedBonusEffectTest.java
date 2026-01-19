package labyrinth.server.game.bonuses;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.PlayerStatistics;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Tests for the PushFixedBonusEffect class.
 * Validates push fixed bonus mechanics including activation and scoring.
 */
class PushFixedBonusEffectTest {

    private PushFixedBonusEffect pushFixedBonusEffect;
    private Game game;
    private Player player;
    private PlayerStatistics playerStats;

    @BeforeEach
    void setUp() {
        pushFixedBonusEffect = new PushFixedBonusEffect();
        game = mock(Game.class);
        player = mock(Player.class);
        playerStats = new PlayerStatistics();
        when(player.getStatistics()).thenReturn(playerStats);
    }

    @Nested
    class SuccessfulApplication {

        @Test
        void shouldReturnTrueWhenPlayerHasBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            boolean result = pushFixedBonusEffect.apply(game, player);

            // Assert
            assertTrue(result);
        }

        @Test
        void shouldSetActiveBonusToPushFixed() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            verify(game).setActiveBonus(BonusTypes.PUSH_FIXED);
        }

        @Test
        void shouldConsumePlayerBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            verify(player).useBonus(BonusTypes.PUSH_FIXED);
        }

        @Test
        void shouldIncreasePlayerScore() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);
            int initialScore = playerStats.getScore();

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            assertTrue(playerStats.getScore() > initialScore);
        }
    }

    @Nested
    class FailedApplication {

        @Test
        void shouldReturnFalseWhenPlayerDoesNotHaveBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(false);

            // Act
            boolean result = pushFixedBonusEffect.apply(game, player);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotSetActiveBonusWhenFailed() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(false);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            verify(game, never()).setActiveBonus(any());
        }

        @Test
        void shouldNotIncreaseScoreWhenFailed() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(false);
            int initialScore = playerStats.getScore();

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            assertEquals(initialScore, playerStats.getScore());
        }
    }

    @Nested
    class ArgumentHandling {

        @Test
        void shouldIgnoreExtraArguments() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act & Assert - should not throw
            assertDoesNotThrow(() ->
                    pushFixedBonusEffect.apply(game, player, "extra", 123, true)
            );
        }

        @Test
        void shouldWorkWithNoExtraArguments() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            boolean result = pushFixedBonusEffect.apply(game, player);

            // Assert
            assertTrue(result);
        }
    }

    @Nested
    class NoSideEffects {

        @Test
        void shouldNotModifyMoveState() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert - PUSH_FIXED doesn't change move state
            verify(game, never()).setMoveState(any());
        }

        @Test
        void shouldNotProcessPlayerStep() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            verify(game, never()).processPlayerStepOnTile(any(), any());
        }

        @Test
        void shouldNotInteractWithBoard() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            verify(game, never()).getBoard();
        }
    }

    @Nested
    class ScoreCalculation {

        @Test
        void shouldAddBonusPoints() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert - score should have increased by the bonus amount
            assertTrue(playerStats.getScore() > 0);
        }

        @Test
        void shouldAccumulateScoreAcrossMultipleApplications() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true, true);

            // Act
            pushFixedBonusEffect.apply(game, player);
            int scoreAfterFirst = playerStats.getScore();
            pushFixedBonusEffect.apply(game, player);
            int scoreAfterSecond = playerStats.getScore();

            // Assert
            assertTrue(scoreAfterSecond > scoreAfterFirst);
        }
    }

    @Nested
    class ComparisonWithOtherBonuses {

        @Test
        void shouldOnlyUsePushFixedBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert - should only try to use PUSH_FIXED, not other types
            verify(player).useBonus(BonusTypes.PUSH_FIXED);
            verify(player, never()).useBonus(BonusTypes.PUSH_TWICE);
            verify(player, never()).useBonus(BonusTypes.BEAM);
            verify(player, never()).useBonus(BonusTypes.SWAP);
        }

        @Test
        void shouldOnlySetPushFixedAsActiveBonus() {
            // Arrange
            when(player.useBonus(BonusTypes.PUSH_FIXED)).thenReturn(true);

            // Act
            pushFixedBonusEffect.apply(game, player);

            // Assert
            verify(game).setActiveBonus(BonusTypes.PUSH_FIXED);
            verify(game, never()).setActiveBonus(BonusTypes.PUSH_TWICE);
            verify(game, never()).setActiveBonus(BonusTypes.BEAM);
            verify(game, never()).setActiveBonus(BonusTypes.SWAP);
        }
    }
}
