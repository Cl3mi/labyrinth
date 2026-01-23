package labyrinth.client.unit.models;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.assertj.core.api.Assertions.*;

/**
 * Tests for game state transition logic.
 * Verifies that the flags used to control game over -> new game transitions
 * are properly managed.
 *
 * This tests the logic used in LabyrinthApplication for handling game state transitions.
 */
@DisplayName("Game State Transition Logic")
class GameStateTransitionTest {

    /**
     * Simulates the game state flags from LabyrinthApplication.
     */
    private static class GameStateFlags {
        boolean isGameOver = false;
        boolean isGameOverCleanup = false;
        boolean exitedToLobby = false;

        /**
         * Resets all flags for a new game.
         * This is the fix applied to onGameStarted handler.
         */
        void resetForNewGame() {
            exitedToLobby = false;
            isGameOver = false;
            isGameOverCleanup = false;
        }

        /**
         * Sets flags when game ends.
         */
        void onGameOver() {
            isGameOver = true;
            isGameOverCleanup = true;
        }

        /**
         * Determines if showGame() should proceed.
         * This is the condition checked in showGame().
         */
        boolean shouldShowGame() {
            return !isGameOver && !isGameOverCleanup;
        }

        /**
         * Determines if game state update should be processed.
         * This is the condition checked in onGameStateUpdate().
         */
        boolean shouldProcessGameStateUpdate() {
            return !exitedToLobby;
        }
    }

    private GameStateFlags flags;

    @BeforeEach
    void setUp() {
        flags = new GameStateFlags();
    }

    @Nested
    @DisplayName("Initial State")
    class InitialStateTests {

        @Test
        @DisplayName("initialState_allFlagsAreFalse")
        void initialState_allFlagsAreFalse() {
            assertThat(flags.isGameOver).isFalse();
            assertThat(flags.isGameOverCleanup).isFalse();
            assertThat(flags.exitedToLobby).isFalse();
        }

        @Test
        @DisplayName("initialState_shouldShowGame")
        void initialState_shouldShowGame() {
            assertThat(flags.shouldShowGame()).isTrue();
        }

        @Test
        @DisplayName("initialState_shouldProcessGameStateUpdate")
        void initialState_shouldProcessGameStateUpdate() {
            assertThat(flags.shouldProcessGameStateUpdate()).isTrue();
        }
    }

    @Nested
    @DisplayName("Game Over State")
    class GameOverStateTests {

        @Test
        @DisplayName("onGameOver_setsFlags")
        void onGameOver_setsFlags() {
            // When: Game ends
            flags.onGameOver();

            // Then: Flags should be set
            assertThat(flags.isGameOver).isTrue();
            assertThat(flags.isGameOverCleanup).isTrue();
        }

        @Test
        @DisplayName("onGameOver_shouldNotShowGame")
        void onGameOver_shouldNotShowGame() {
            // Given: Game has ended
            flags.onGameOver();

            // Then: showGame should return false
            assertThat(flags.shouldShowGame()).isFalse();
        }

        @Test
        @DisplayName("onGameOver_shouldStillProcessGameStateUpdate")
        void onGameOver_shouldStillProcessGameStateUpdate() {
            // Given: Game has ended but player hasn't exited to lobby
            flags.onGameOver();

            // Then: Should still process game state updates
            // (This allows seeing the final game state)
            assertThat(flags.shouldProcessGameStateUpdate()).isTrue();
        }
    }

    @Nested
    @DisplayName("New Game Started After Game Over - THE FIX")
    class NewGameAfterGameOverTests {

        @Test
        @DisplayName("newGameStarted_resetsFlagsCorrectly")
        void newGameStarted_resetsFlagsCorrectly() {
            // Given: Previous game has ended
            flags.onGameOver();
            assertThat(flags.isGameOver).isTrue();
            assertThat(flags.isGameOverCleanup).isTrue();

            // When: New game starts (this is the fix)
            flags.resetForNewGame();

            // Then: All flags should be reset
            assertThat(flags.isGameOver).isFalse();
            assertThat(flags.isGameOverCleanup).isFalse();
            assertThat(flags.exitedToLobby).isFalse();
        }

        @Test
        @DisplayName("newGameStarted_shouldShowGame")
        void newGameStarted_shouldShowGame() {
            // Given: Previous game has ended
            flags.onGameOver();
            assertThat(flags.shouldShowGame()).isFalse();

            // When: New game starts
            flags.resetForNewGame();

            // Then: showGame should work
            assertThat(flags.shouldShowGame()).isTrue();
        }

        @Test
        @DisplayName("newGameStarted_shouldProcessGameStateUpdate")
        void newGameStarted_shouldProcessGameStateUpdate() {
            // Given: Previous game has ended
            flags.onGameOver();

            // When: New game starts
            flags.resetForNewGame();

            // Then: Should process game state updates
            assertThat(flags.shouldProcessGameStateUpdate()).isTrue();
        }
    }

    @Nested
    @DisplayName("Exited To Lobby")
    class ExitedToLobbyTests {

        @Test
        @DisplayName("exitedToLobby_shouldNotProcessGameStateUpdate")
        void exitedToLobby_shouldNotProcessGameStateUpdate() {
            // Given: Player has exited to lobby
            flags.exitedToLobby = true;

            // Then: Should not process game state updates
            assertThat(flags.shouldProcessGameStateUpdate()).isFalse();
        }

        @Test
        @DisplayName("newGameStarted_resetsExitedToLobby")
        void newGameStarted_resetsExitedToLobby() {
            // Given: Player had exited to lobby
            flags.exitedToLobby = true;

            // When: New game starts
            flags.resetForNewGame();

            // Then: exitedToLobby should be reset
            assertThat(flags.exitedToLobby).isFalse();
            assertThat(flags.shouldProcessGameStateUpdate()).isTrue();
        }
    }

    @Nested
    @DisplayName("Bug Scenario - The Original Issue")
    class BugScenarioTests {

        @Test
        @DisplayName("originalBug_gameOverThenNewGame_failsWithoutFix")
        void originalBug_gameOverThenNewGame_failsWithoutFix() {
            // This test demonstrates the bug that was fixed.
            // Before the fix, isGameOver and isGameOverCleanup were not reset.

            // Given: Game ends
            flags.onGameOver();

            // Then: Without the fix (not calling resetForNewGame),
            // showGame would return false even for a new game
            assertThat(flags.shouldShowGame()).isFalse();

            // The bug: If onGameStarted didn't reset these flags,
            // the new game wouldn't be shown
        }

        @Test
        @DisplayName("fixedBehavior_gameOverThenNewGame_worksCorrectly")
        void fixedBehavior_gameOverThenNewGame_worksCorrectly() {
            // This test demonstrates the fixed behavior.

            // Given: Game ends
            flags.onGameOver();
            assertThat(flags.shouldShowGame()).isFalse();

            // When: New game starts (with the fix applied)
            flags.resetForNewGame();

            // Then: showGame should work for the new game
            assertThat(flags.shouldShowGame()).isTrue();
        }

        @Test
        @DisplayName("multipleGamesInSequence_workCorrectly")
        void multipleGamesInSequence_workCorrectly() {
            // Simulate multiple games in sequence

            // Game 1 starts
            flags.resetForNewGame();
            assertThat(flags.shouldShowGame()).isTrue();

            // Game 1 ends
            flags.onGameOver();
            assertThat(flags.shouldShowGame()).isFalse();

            // Game 2 starts
            flags.resetForNewGame();
            assertThat(flags.shouldShowGame()).isTrue();

            // Game 2 ends
            flags.onGameOver();
            assertThat(flags.shouldShowGame()).isFalse();

            // Game 3 starts
            flags.resetForNewGame();
            assertThat(flags.shouldShowGame()).isTrue();
        }
    }

    @Nested
    @DisplayName("Edge Cases")
    class EdgeCaseTests {

        @Test
        @DisplayName("resetForNewGame_whenNotInGameOverState_stillWorks")
        void resetForNewGame_whenNotInGameOverState_stillWorks() {
            // Given: Flags are in initial state
            assertThat(flags.isGameOver).isFalse();

            // When: Reset is called (even though not needed)
            flags.resetForNewGame();

            // Then: Should still be in valid state
            assertThat(flags.shouldShowGame()).isTrue();
            assertThat(flags.shouldProcessGameStateUpdate()).isTrue();
        }

        @Test
        @DisplayName("gameOverWithExitToLobby_newGameResetsAll")
        void gameOverWithExitToLobby_newGameResetsAll() {
            // Given: Game ended and player exited to lobby
            flags.onGameOver();
            flags.exitedToLobby = true;

            // When: New game starts
            flags.resetForNewGame();

            // Then: All flags should be reset
            assertThat(flags.isGameOver).isFalse();
            assertThat(flags.isGameOverCleanup).isFalse();
            assertThat(flags.exitedToLobby).isFalse();
            assertThat(flags.shouldShowGame()).isTrue();
            assertThat(flags.shouldProcessGameStateUpdate()).isTrue();
        }
    }
}
