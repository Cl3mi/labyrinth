package labyrinth.client.unit.ai;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.*;

/**
 * Tests for AI mode toggle logic.
 * Tests the state management aspects that don't require WebSocket interaction.
 *
 * Note: The actual AiController class is excluded from coverage since it requires
 * WebSocket connections. These tests verify the state logic patterns used.
 */
@DisplayName("AI Toggle State Logic")
class AiControllerToggleTest {

    /**
     * Simulates the AI toggle state logic from AiController.
     * This avoids the need to mock the WebSocketClient which is problematic.
     */
    private static class AiToggleState {
        private final AtomicBoolean aiModeEnabled = new AtomicBoolean(false);
        private final AtomicBoolean stopped = new AtomicBoolean(false);
        private Runnable onAiModeChanged;

        // Track commands sent for verification
        private Boolean lastSentEnabled = null;
        private int commandsSent = 0;
        private boolean isConnected = false;

        void setOnAiModeChanged(Runnable callback) {
            this.onAiModeChanged = callback;
        }

        void setConnected(boolean connected) {
            this.isConnected = connected;
        }

        void toggleAiMode() {
            boolean newState = !aiModeEnabled.get();
            aiModeEnabled.set(newState);

            // Notify server about AI status change
            if (isConnected) {
                sendToggleAi(newState);
            }

            if (onAiModeChanged != null) {
                onAiModeChanged.run();
            }
        }

        void setAiModeEnabled(boolean enabled) {
            boolean changed = aiModeEnabled.getAndSet(enabled) != enabled;

            // Notify server about AI status change if the state actually changed
            if (changed && isConnected) {
                sendToggleAi(enabled);
            }

            if (onAiModeChanged != null) {
                onAiModeChanged.run();
            }
        }

        boolean isAiModeEnabled() {
            return aiModeEnabled.get();
        }

        void stop() {
            stopped.set(true);
            aiModeEnabled.set(false);

            if (onAiModeChanged != null) {
                onAiModeChanged.run();
            }
        }

        void reset() {
            stopped.set(false);
        }

        private void sendToggleAi(boolean enabled) {
            lastSentEnabled = enabled;
            commandsSent++;
        }

        Boolean getLastSentEnabled() {
            return lastSentEnabled;
        }

        int getCommandsSent() {
            return commandsSent;
        }

        void resetCommandTracking() {
            lastSentEnabled = null;
            commandsSent = 0;
        }
    }

    private AiToggleState state;

    @BeforeEach
    void setUp() {
        state = new AiToggleState();
    }

    @Nested
    @DisplayName("Toggle AI Mode")
    class ToggleAiModeTests {

        @Test
        @DisplayName("toggleAiMode_initiallyOff_turnsOn")
        void toggleAiMode_initiallyOff_turnsOn() {
            // Given: AI mode is initially off
            assertThat(state.isAiModeEnabled()).isFalse();

            // When: Toggle AI mode
            state.toggleAiMode();

            // Then: AI mode should be on
            assertThat(state.isAiModeEnabled()).isTrue();
        }

        @Test
        @DisplayName("toggleAiMode_currentlyOn_turnsOff")
        void toggleAiMode_currentlyOn_turnsOff() {
            // Given: AI mode is on
            state.setAiModeEnabled(true);
            assertThat(state.isAiModeEnabled()).isTrue();

            // When: Toggle AI mode
            state.toggleAiMode();

            // Then: AI mode should be off
            assertThat(state.isAiModeEnabled()).isFalse();
        }

        @Test
        @DisplayName("toggleAiMode_sendsCommandToServerWhenConnected")
        void toggleAiMode_sendsCommandToServerWhenConnected() {
            // Given: Client is connected
            state.setConnected(true);

            // When: Toggle AI mode (turning it on)
            state.toggleAiMode();

            // Then: Should send toggle command with enabled=true
            assertThat(state.getLastSentEnabled()).isTrue();
            assertThat(state.getCommandsSent()).isEqualTo(1);
        }

        @Test
        @DisplayName("toggleAiMode_doesNotSendCommandWhenDisconnected")
        void toggleAiMode_doesNotSendCommandWhenDisconnected() {
            // Given: Client is not connected
            state.setConnected(false);

            // When: Toggle AI mode
            state.toggleAiMode();

            // Then: Should not send any command
            assertThat(state.getCommandsSent()).isZero();
        }

        @Test
        @DisplayName("multipleToggles_sendCorrectValues")
        void multipleToggles_sendCorrectValues() {
            // Given: Client is connected
            state.setConnected(true);

            // When: Toggle multiple times
            state.toggleAiMode(); // off -> on (sends true)
            assertThat(state.getLastSentEnabled()).isTrue();

            state.toggleAiMode(); // on -> off (sends false)
            assertThat(state.getLastSentEnabled()).isFalse();

            state.toggleAiMode(); // off -> on (sends true)
            assertThat(state.getLastSentEnabled()).isTrue();

            // Then: Should have sent 3 commands total
            assertThat(state.getCommandsSent()).isEqualTo(3);
        }
    }

    @Nested
    @DisplayName("Set AI Mode Enabled")
    class SetAiModeEnabledTests {

        @Test
        @DisplayName("setAiModeEnabled_true_enablesAiMode")
        void setAiModeEnabled_true_enablesAiMode() {
            // When: Set AI mode enabled
            state.setAiModeEnabled(true);

            // Then: AI mode should be enabled
            assertThat(state.isAiModeEnabled()).isTrue();
        }

        @Test
        @DisplayName("setAiModeEnabled_false_disablesAiMode")
        void setAiModeEnabled_false_disablesAiMode() {
            // Given: AI mode is on
            state.setAiModeEnabled(true);

            // When: Set AI mode disabled
            state.setAiModeEnabled(false);

            // Then: AI mode should be disabled
            assertThat(state.isAiModeEnabled()).isFalse();
        }

        @Test
        @DisplayName("setAiModeEnabled_changedState_sendsCommand")
        void setAiModeEnabled_changedState_sendsCommand() {
            // Given: Client is connected, AI mode is off
            state.setConnected(true);
            assertThat(state.isAiModeEnabled()).isFalse();

            // When: Set AI mode enabled
            state.setAiModeEnabled(true);

            // Then: Should send command
            assertThat(state.getLastSentEnabled()).isTrue();
            assertThat(state.getCommandsSent()).isEqualTo(1);
        }

        @Test
        @DisplayName("setAiModeEnabled_sameState_doesNotSendCommand")
        void setAiModeEnabled_sameState_doesNotSendCommand() {
            // Given: Client is connected, AI mode is off
            state.setConnected(true);

            // When: Set AI mode to the same state (false)
            state.setAiModeEnabled(false);

            // Then: Should not send command (state didn't change)
            assertThat(state.getCommandsSent()).isZero();
        }

        @Test
        @DisplayName("setAiModeEnabled_alreadyEnabled_doesNotSendCommand")
        void setAiModeEnabled_alreadyEnabled_doesNotSendCommand() {
            // Given: Client is connected, set AI mode on first
            state.setConnected(true);
            state.setAiModeEnabled(true);
            state.resetCommandTracking(); // Reset to verify next call

            // When: Set AI mode to the same state (true)
            state.setAiModeEnabled(true);

            // Then: Should not send command (state didn't change)
            assertThat(state.getCommandsSent()).isZero();
        }
    }

    @Nested
    @DisplayName("AI Mode Changed Callback")
    class AiModeChangedCallbackTests {

        @Test
        @DisplayName("toggleAiMode_callsCallback")
        void toggleAiMode_callsCallback() {
            // Given: Callback is registered
            AtomicBoolean callbackCalled = new AtomicBoolean(false);
            state.setOnAiModeChanged(() -> callbackCalled.set(true));

            // When: Toggle AI mode
            state.toggleAiMode();

            // Then: Callback should be called
            assertThat(callbackCalled.get()).isTrue();
        }

        @Test
        @DisplayName("setAiModeEnabled_callsCallback")
        void setAiModeEnabled_callsCallback() {
            // Given: Callback is registered
            AtomicBoolean callbackCalled = new AtomicBoolean(false);
            state.setOnAiModeChanged(() -> callbackCalled.set(true));

            // When: Set AI mode enabled
            state.setAiModeEnabled(true);

            // Then: Callback should be called
            assertThat(callbackCalled.get()).isTrue();
        }

        @Test
        @DisplayName("multipleToggles_callbackCalledEachTime")
        void multipleToggles_callbackCalledEachTime() {
            // Given: Callback is registered
            AtomicInteger callbackCount = new AtomicInteger(0);
            state.setOnAiModeChanged(() -> callbackCount.incrementAndGet());

            // When: Toggle multiple times
            state.toggleAiMode();
            state.toggleAiMode();
            state.toggleAiMode();

            // Then: Callback should be called 3 times
            assertThat(callbackCount.get()).isEqualTo(3);
        }
    }

    @Nested
    @DisplayName("Stop AI Controller")
    class StopAiControllerTests {

        @Test
        @DisplayName("stop_disablesAiMode")
        void stop_disablesAiMode() {
            // Given: AI mode is enabled
            state.setAiModeEnabled(true);
            assertThat(state.isAiModeEnabled()).isTrue();

            // When: Stop the controller
            state.stop();

            // Then: AI mode should be disabled
            assertThat(state.isAiModeEnabled()).isFalse();
        }

        @Test
        @DisplayName("stop_callsCallback")
        void stop_callsCallback() {
            // Given: Callback is registered
            AtomicBoolean callbackCalled = new AtomicBoolean(false);
            state.setOnAiModeChanged(() -> callbackCalled.set(true));

            // When: Stop the controller
            state.stop();

            // Then: Callback should be called
            assertThat(callbackCalled.get()).isTrue();
        }
    }

    @Nested
    @DisplayName("Reset AI Controller")
    class ResetAiControllerTests {

        @Test
        @DisplayName("reset_allowsNewToggle")
        void reset_allowsNewToggle() {
            // Given: Controller was stopped
            state.stop();

            // When: Reset and toggle
            state.reset();
            state.toggleAiMode();

            // Then: AI mode should be enabled
            assertThat(state.isAiModeEnabled()).isTrue();
        }
    }
}
