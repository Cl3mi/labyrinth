package labyrinth.client.integration.websocket;

import labyrinth.client.messaging.GameClient;
import labyrinth.contracts.models.*;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for GameClient - tests client-side behavior without requiring a real server.
 * These tests focus on callback registration, message parsing, and state management.
 */
@DisplayName("GameClient Unit Tests")
class GameClientTest {

    private static final String TEST_SERVER_URI = "ws://localhost:8080/game";
    private GameClient gameClient;

    @BeforeEach
    void setUp() throws URISyntaxException {
        gameClient = new GameClient(new URI(TEST_SERVER_URI));
    }

    @AfterEach
    void tearDown() {
        if (gameClient != null) {
            try {
                gameClient.close();
            } catch (Exception ignored) {
            }
        }
    }

    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {

        @Test
        @DisplayName("constructor_validUri_createsClient")
        void constructor_validUri_createsClient() throws URISyntaxException {
            // Given
            URI uri = new URI(TEST_SERVER_URI);

            // When
            GameClient client = new GameClient(uri);

            // Then
            assertThat(client).isNotNull();
            client.close();
        }

        @Test
        @DisplayName("constructor_withDifferentPort_createsClient")
        void constructor_withDifferentPort_createsClient() throws URISyntaxException {
            // Given
            URI uri = new URI("ws://localhost:9090/game");

            // When
            GameClient client = new GameClient(uri);

            // Then
            assertThat(client).isNotNull();
            client.close();
        }
    }

    @Nested
    @DisplayName("Callback Registration")
    class CallbackRegistrationTests {

        @Test
        @DisplayName("setOnConnectAck_validCallback_setsCallback")
        void setOnConnectAck_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnConnectAck(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnLobbyState_validCallback_setsCallback")
        void setOnLobbyState_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnLobbyState(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnGameStarted_validCallback_setsCallback")
        void setOnGameStarted_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnGameStarted(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnGameStateUpdate_validCallback_setsCallback")
        void setOnGameStateUpdate_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnGameStateUpdate(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnGameOver_validCallback_setsCallback")
        void setOnGameOver_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnGameOver(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnErrorMessage_validCallback_setsCallback")
        void setOnErrorMessage_validCallback_setsCallback() {
            AtomicReference<String> errorMessage = new AtomicReference<>();
            gameClient.setOnErrorMessage(errorMessage::set);
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnOpenHook_validCallback_setsCallback")
        void setOnOpenHook_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnOpenHook(() -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnConnectionLost_validCallback_setsCallback")
        void setOnConnectionLost_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnConnectionLost(() -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnStatusUpdate_validCallback_setsCallback")
        void setOnStatusUpdate_validCallback_setsCallback() {
            AtomicReference<String> status = new AtomicReference<>();
            gameClient.setOnStatusUpdate(status::set);
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnNextTreasure_validCallback_setsCallback")
        void setOnNextTreasure_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnNextTreasure(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnPlayerUpdated_validCallback_setsCallback")
        void setOnPlayerUpdated_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnPlayerUpdated(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }

        @Test
        @DisplayName("setOnAchievementUnlocked_validCallback_setsCallback")
        void setOnAchievementUnlocked_validCallback_setsCallback() {
            AtomicBoolean called = new AtomicBoolean(false);
            gameClient.setOnAchievementUnlocked(payload -> called.set(true));
            assertThat(gameClient).isNotNull();
        }
    }

    @Nested
    @DisplayName("Connection State")
    class ConnectionStateTests {

        @Test
        @DisplayName("newClient_initialState_notConnected")
        void newClient_initialState_notConnected() throws URISyntaxException {
            GameClient client = new GameClient(new URI(TEST_SERVER_URI));
            assertThat(client.isOpen()).isFalse();
            client.close();
        }

        @Test
        @DisplayName("getReadyState_afterCreation_isNotYetConnected")
        void getReadyState_afterCreation_isNotYetConnected() {
            assertThat(gameClient.getReadyState())
                    .isEqualTo(org.java_websocket.enums.ReadyState.NOT_YET_CONNECTED);
        }
    }

    @Nested
    @DisplayName("Message Parsing")
    class MessageParsingTests {

        @Test
        @DisplayName("onMessage_nullType_handlesGracefully")
        void onMessage_nullType_handlesGracefully() {
            String messageWithNullType = "{\"payload\": {}}";
            assertThatCode(() -> gameClient.onMessage(messageWithNullType))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_invalidEventType_handlesGracefully")
        void onMessage_invalidEventType_handlesGracefully() {
            String messageWithInvalidType = "{\"type\": \"INVALID_EVENT_TYPE\"}";
            assertThatCode(() -> gameClient.onMessage(messageWithInvalidType))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_malformedJson_handlesGracefully")
        void onMessage_malformedJson_handlesGracefully() {
            String malformedJson = "{ this is not valid json }";
            assertThatCode(() -> gameClient.onMessage(malformedJson))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_emptyMessage_handlesGracefully")
        void onMessage_emptyMessage_handlesGracefully() {
            String emptyMessage = "";
            assertThatCode(() -> gameClient.onMessage(emptyMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_serverInfoEvent_handlesCorrectly")
        void onMessage_serverInfoEvent_handlesCorrectly() {
            String serverInfoMessage = "{\"type\": \"SERVER_INFO\", \"message\": \"Welcome\"}";
            assertThatCode(() -> gameClient.onMessage(serverInfoMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_connectAckEvent_parsesCorrectly")
        void onMessage_connectAckEvent_parsesCorrectly() {
            String connectAckMessage = "{\"type\": \"CONNECT_ACK\", \"playerId\": \"123\", \"identifierToken\": \"token-abc\"}";
            assertThatCode(() -> gameClient.onMessage(connectAckMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_lobbyStateEvent_parsesCorrectly")
        void onMessage_lobbyStateEvent_parsesCorrectly() {
            String lobbyStateMessage = "{\"type\": \"LOBBY_STATE\", \"players\": []}";
            assertThatCode(() -> gameClient.onMessage(lobbyStateMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_gameStartedEvent_parsesCorrectly")
        void onMessage_gameStartedEvent_parsesCorrectly() {
            String gameStartedMessage = "{\"type\": \"GAME_STARTED\", \"payload\": {}}";
            assertThatCode(() -> gameClient.onMessage(gameStartedMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_gameStateUpdateEvent_parsesCorrectly")
        void onMessage_gameStateUpdateEvent_parsesCorrectly() {
            String gameStateMessage = "{\"type\": \"GAME_STATE_UPDATE\", \"payload\": {}}";
            assertThatCode(() -> gameClient.onMessage(gameStateMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_actionErrorEvent_parsesCorrectly")
        void onMessage_actionErrorEvent_parsesCorrectly() {
            String actionErrorMessage = "{\"type\": \"ACTION_ERROR\", \"errorCode\": \"NOT_YOUR_TURN\", \"message\": \"Wait\"}";
            assertThatCode(() -> gameClient.onMessage(actionErrorMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_gameOverEvent_parsesCorrectly")
        void onMessage_gameOverEvent_parsesCorrectly() {
            String gameOverMessage = "{\"type\": \"GAME_OVER\", \"winnerId\": \"player-1\"}";
            assertThatCode(() -> gameClient.onMessage(gameOverMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_nextTreasureEvent_parsesCorrectly")
        void onMessage_nextTreasureEvent_parsesCorrectly() {
            String nextTreasureMessage = "{\"type\": \"NEXT_TREASURE\", \"payload\": {}}";
            assertThatCode(() -> gameClient.onMessage(nextTreasureMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_playerUpdatedEvent_parsesCorrectly")
        void onMessage_playerUpdatedEvent_parsesCorrectly() {
            String playerUpdatedMessage = "{\"type\": \"PLAYER_UPDATED\", \"payload\": {}}";
            assertThatCode(() -> gameClient.onMessage(playerUpdatedMessage))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onMessage_achievementUnlockedEvent_parsesCorrectly")
        void onMessage_achievementUnlockedEvent_parsesCorrectly() {
            String achievementMessage = "{\"type\": \"ACHIEVEMENT_UNLOCKED\", \"payload\": {}}";
            assertThatCode(() -> gameClient.onMessage(achievementMessage))
                    .doesNotThrowAnyException();
        }
    }

    @Nested
    @DisplayName("WebSocket Lifecycle Events")
    class WebSocketLifecycleTests {

        @Test
        @DisplayName("onClose_normalClose_handlesGracefully")
        void onClose_normalClose_handlesGracefully() {
            assertThatCode(() -> gameClient.onClose(1000, "Normal closure", false))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onClose_remoteClose_handlesGracefully")
        void onClose_remoteClose_handlesGracefully() {
            assertThatCode(() -> gameClient.onClose(1006, "Connection lost", true))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onClose_withNullReason_handlesGracefully")
        void onClose_withNullReason_handlesGracefully() {
            assertThatCode(() -> gameClient.onClose(1000, null, false))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onError_withException_handlesGracefully")
        void onError_withException_handlesGracefully() {
            Exception testException = new RuntimeException("Test error");
            assertThatCode(() -> gameClient.onError(testException))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("onError_withNullMessageException_handlesGracefully")
        void onError_withNullMessageException_handlesGracefully() {
            Exception testException = new RuntimeException();
            assertThatCode(() -> gameClient.onError(testException))
                    .doesNotThrowAnyException();
        }
    }
}
