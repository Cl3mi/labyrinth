package labyrinth.client.messaging;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import labyrinth.contracts.models.*;
import lombok.Setter;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.enums.ReadyState;
import org.java_websocket.handshake.ServerHandshake;

import javax.swing.*;
import java.net.URI;
import java.util.function.Consumer;

public class GameClient extends WebSocketClient {

    private final ObjectMapper mapper;

    private volatile ConnectionState connectionState = ConnectionState.DISCONNECTED;
    private volatile boolean intentionalDisconnect = false;


    @Setter private Consumer<ConnectAckEventPayload> onConnectAck;
    @Setter private Consumer<LobbyStateEventPayload> onLobbyState;
    @Setter private Consumer<GameStateEventPayload> onGameStarted;
    @Setter private Consumer<AchievementUnlockedEventPayload> onAchievementUnlocked;
    @Setter private Consumer<GameOverEventPayload> onGameOver;
    @Setter private Consumer<NextTreasureCardEventPayload> onNextTreasure;
    @Setter private Consumer<PlayerUpdatedEventPayload> onPlayerUpdated;

    @Setter private Consumer<GameStateEventPayload> onGameStateUpdate;

    @Setter private Consumer<String> onErrorMessage;
    @Setter private Runnable onOpenHook;

    @Setter private Runnable onConnectionLost;        // Triggered on unintentional disconnect
    @Setter private Consumer<String> onStatusUpdate;  // Status messages for UI

    public GameClient(URI serverUri) {
        super(serverUri);

        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
    }

    @Override
    public void onOpen(ServerHandshake handShake) {
        System.out.println("WebSocket connected");

        connectionState = ConnectionState.CONNECTED;
        intentionalDisconnect = false; // Reset flag on successful connection

        if (onStatusUpdate != null) {
            runOnUiThread(() -> onStatusUpdate.accept("Verbunden mit Server"));
        }

        if (onOpenHook != null) {
            runOnUiThread(onOpenHook);
        }
    }

    @Override
    public void onMessage(String message) {
        try {
            System.out.println("WS IN: " + message);

            JsonNode root = mapper.readTree(message);
            JsonNode typeNode = root.get("type");
            if (typeNode == null || typeNode.isNull()) {
                runOnUiThread(() -> {
                    if (onErrorMessage != null) onErrorMessage.accept("Unknown message (no type): " + message);
                });
                return;
            }

            final EventType type;
            try {
                type = EventType.valueOf(typeNode.asText());
            } catch (IllegalArgumentException iae) {
                runOnUiThread(() -> {
                    if (onErrorMessage != null) onErrorMessage.accept("Unknown EventType: " + typeNode.asText());
                });
                return;
            }

            JsonNode payloadNode = root.get("payload");
            if (payloadNode == null || payloadNode.isNull()) payloadNode = root;

            switch (type) {
                case CONNECT_ACK -> {
                    ConnectAckEventPayload payload = mapper.treeToValue(payloadNode, ConnectAckEventPayload.class);
                    if (onConnectAck != null) runOnUiThread(() -> onConnectAck.accept(payload));
                }
                case LOBBY_STATE -> {
                    LobbyStateEventPayload payload = mapper.treeToValue(payloadNode, LobbyStateEventPayload.class);
                    if (onLobbyState != null) runOnUiThread(() -> onLobbyState.accept(payload));
                }
                case GAME_STARTED -> {
                    GameStateEventPayload payload = mapper.treeToValue(payloadNode, GameStateEventPayload.class);
                    if (onGameStarted != null) runOnUiThread(() -> onGameStarted.accept(payload));
                }
                case GAME_STATE_UPDATE -> {
                    GameStateEventPayload payload = mapper.treeToValue(payloadNode, GameStateEventPayload.class);
                    if (onGameStateUpdate != null) runOnUiThread(() -> onGameStateUpdate.accept(payload));
                }
                case ACTION_ERROR -> {
                    ActionErrorEventPayload payload = mapper.treeToValue(payloadNode, ActionErrorEventPayload.class);
                    // Include error code in the message so handlers can detect specific error types
                    String errorCode = payload.getErrorCode() != null ? payload.getErrorCode().toString() : "UNKNOWN";
                    String msg = errorCode + ": " + (payload.getMessage() != null ? payload.getMessage() : "No details");
                    if (onErrorMessage != null) runOnUiThread(() -> onErrorMessage.accept(msg));
                }

                case ACHIEVEMENT_UNLOCKED -> {
                    AchievementUnlockedEventPayload payload = mapper.treeToValue(payloadNode, AchievementUnlockedEventPayload.class);
                    if (onAchievementUnlocked != null) runOnUiThread(() -> onAchievementUnlocked.accept(payload));
                }

                case GAME_OVER -> {
                    System.out.println("[GameClient] GAME_OVER message received, parsing payload...");
                    GameOverEventPayload payload = mapper.treeToValue(payloadNode, GameOverEventPayload.class);
                    System.out.println("[GameClient] GAME_OVER payload parsed, winner: " + payload.getWinnerId() + ", callback registered: " + (onGameOver != null));
                    if (onGameOver != null) {
                        System.out.println("[GameClient] Calling onGameOver callback...");
                        runOnUiThread(() -> onGameOver.accept(payload));
                    } else {
                        System.err.println("[GameClient] WARNING: onGameOver callback is NULL!");
                    }
                }

                case NEXT_TREASURE -> {
                    NextTreasureCardEventPayload payload = mapper.treeToValue(payloadNode, NextTreasureCardEventPayload.class);
                    if (onNextTreasure != null) runOnUiThread(() -> onNextTreasure.accept(payload));
                }

                case PLAYER_UPDATED -> {
                    PlayerUpdatedEventPayload payload = mapper.treeToValue(payloadNode, PlayerUpdatedEventPayload.class);
                    if (onPlayerUpdated != null) runOnUiThread(() -> onPlayerUpdated.accept(payload));
                }

                case SERVER_INFO -> {
                    // ServerInfoEventPayload payload = mapper.treeToValue(payloadNode, ServerInfoEventPayload.class);
                    System.out.println("SERVER_INFO received: " + payloadNode);
                }

                default -> System.out.println("Unhandled event type: " + type + " raw=" + message);
            }
        } catch (Exception e) {
            e.printStackTrace();
            runOnUiThread(() -> {
                if (onErrorMessage != null) onErrorMessage.accept("Failed to parse message: " + e.getMessage());
            });
        }
    }

    @Override
    public void onClose(int code, String reason, boolean remote) {
        System.out.println("WebSocket closed. code=" + code + " remote=" + remote + " reason=" + reason);

        ConnectionState previousState = connectionState;

        // Update state based on whether disconnect was intentional
        if (connectionState == ConnectionState.DISCONNECTING) {
            connectionState = ConnectionState.DISCONNECTED;
            System.out.println("Intentional disconnect completed");
            return; // Don't trigger reconnection
        }

        // Unintentional disconnect - prepare for reconnection
        connectionState = ConnectionState.DISCONNECTED;

        // Determine if this was an unexpected disconnect
        boolean shouldReconnect = !intentionalDisconnect &&
                                  previousState == ConnectionState.CONNECTED;

        if (shouldReconnect) {
            System.out.println("Unintentional disconnect detected - triggering reconnection");
            if (onConnectionLost != null) {
                runOnUiThread(onConnectionLost);
            }
        } else {
            // Normal error message for expected disconnects
            runOnUiThread(() -> {
                if (onErrorMessage != null) {
                    onErrorMessage.accept("Disconnected (code=" + code + "): " +
                                        (reason == null ? "" : reason));
                }
            });
        }
    }

    @Override
    public void onError(Exception ex) {
        ex.printStackTrace();

        // If we're connected or connecting, this is an unexpected error
        boolean shouldReconnect = (connectionState == ConnectionState.CONNECTED ||
                                   connectionState == ConnectionState.CONNECTING) &&
                                  !intentionalDisconnect;

        if (shouldReconnect) {
            System.out.println("WebSocket error during active connection - may trigger reconnection");
            // Note: onClose() will be called after this, which handles reconnection
        }

        runOnUiThread(() -> {
            if (onErrorMessage != null) {
                onErrorMessage.accept("WebSocket error: " +
                                    ex.getClass().getSimpleName() + ": " + ex.getMessage());
            }
        });
    }

    // ===================== COMMANDS =====================

    public void sendConnect(String username) {
        try {
            ConnectCommandPayload payload = new ConnectCommandPayload();
            payload.setType(CommandType.CONNECT);
            payload.setUsername(username);
            send(mapper.writeValueAsString(payload));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendReconnect(String identifierToken) {
        try {
            ConnectCommandPayload payload = new ConnectCommandPayload();
            payload.setType(CommandType.CONNECT);
            payload.setIdentifierToken(identifierToken);
            send(mapper.writeValueAsString(payload));
            System.out.println("sendReconnect -> " + mapper.writeValueAsString(payload));
        } catch (Exception e) {
            e.printStackTrace();
            runOnUiThread(() -> {
                if (onErrorMessage != null) onErrorMessage.accept("Failed to send reconnect: " + e.getMessage());
            });
        }
    }

    public void sendDisconnect() {
        try {
            DisconnectCommandPayload payload = new DisconnectCommandPayload();
            payload.setType(CommandType.DISCONNECT);
            send(mapper.writeValueAsString(payload));
            System.out.println("sendDisconnect -> " + mapper.writeValueAsString(payload));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendStartGame(BoardSize boardSize,
                              int totalTreasureCardCount,
                              int totalBonusCount,
                              Integer gameDurationInSeconds,
                              Integer turnTimeInSeconds) {
        try {
            StartGameCommandPayload payload = new StartGameCommandPayload();
            payload.setType(CommandType.START_GAME);
            payload.setBoardSize(boardSize);
            payload.setTreasureCardCount(totalTreasureCardCount);
            payload.setTotalBonusCount(totalBonusCount);
            payload.setGameDurationInSeconds(gameDurationInSeconds != null ? gameDurationInSeconds : 3600);

            // Set turnTimeInSeconds in additionalProperties (not in standard Contracts)
            var additionalProps = new java.util.HashMap<String, Object>();
            additionalProps.put("turnTimeInSeconds", turnTimeInSeconds != null ? turnTimeInSeconds : 30);
            payload.setAdditionalProperties(additionalProps);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendStartGame isOpen=" + isOpen() + " isClosing=" + isClosing() + " isClosed=" + isClosed());
            System.out.println("sendStartGame -> " + json);
            send(json);
            System.out.println(">>> START_GAME sent, waiting for response...");

            new Thread(() -> {
                try {
                    Thread.sleep(3000);
                    System.out.println(">>> 3 seconds passed - no GAME_STARTED received");
                    System.out.println(">>> Connection still open: " + isOpen());
                } catch (InterruptedException ignored) {}
            }).start();
        } catch (Exception e) {
            e.printStackTrace();
            runOnUiThread(() -> {
                if (onErrorMessage != null) onErrorMessage.accept("Failed to send start game: " + e.getMessage());
            });
        }
    }

    public void sendRotateTile() {
        try {
            RotateTileCommandPayload payload = new RotateTileCommandPayload();
            payload.setType(CommandType.ROTATE_TILE);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendRotateTile -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            runOnUiThread(() -> {
                if (onErrorMessage != null) {
                    onErrorMessage.accept("Failed to send rotate tile: " + e.getMessage());
                }
            });
        }
    }

    public void sendMovePawn(int targetRow, int targetCol) {
        try {
            MovePawnCommandPayload payload = new MovePawnCommandPayload();
            payload.setType(CommandType.MOVE_PAWN);

            Coordinates coords = new Coordinates();
            coords.setRow(targetRow);
            coords.setColumn(targetCol);
            payload.setTargetCoordinates(coords);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendMovePawn -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            runOnUiThread(() -> {
                if (onErrorMessage != null) {
                    onErrorMessage.accept("Failed to send move pawn: " + e.getMessage());
                }
            });
        }
    }

    public void sendPushTile(int rowOrColIndex, Direction direction) {
        try {
            PushTileCommandPayload payload = new PushTileCommandPayload();
            payload.setType(CommandType.PUSH_TILE);
            payload.setRowOrColIndex(rowOrColIndex);
            payload.setDirection(direction);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendPushTile -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                runOnUiThread(() -> onErrorMessage.accept("Failed to send push tile: " + e.getMessage()));
            }
        }
    }

    // ===================== BONUS COMMANDS =====================

    /**
     * Send USE_BEAM command to teleport avatar to target position.
     * After using this bonus, the player can still make their normal move.
     */
    public void sendUseBeam(int targetRow, int targetCol) {
        try {
            UseBeamCommandPayload payload = new UseBeamCommandPayload();
            payload.setType(CommandType.USE_BEAM);

            Coordinates coords = new Coordinates();
            coords.setRow(targetRow);
            coords.setColumn(targetCol);
            payload.setTargetCoordinates(coords);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendUseBeam -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                runOnUiThread(() -> onErrorMessage.accept("Failed to send use beam: " + e.getMessage()));
            }
        }
    }

    /**
     * Send USE_SWAP command to swap positions with another player.
     * After using this bonus, the player can still make their normal move.
     */
    public void sendUseSwap(String targetPlayerId) {
        try {
            UseSwapCommandPayload payload = new UseSwapCommandPayload();
            payload.setType(CommandType.USE_SWAP);
            payload.setTargetPlayerId(targetPlayerId);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendUseSwap -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                runOnUiThread(() -> onErrorMessage.accept("Failed to send use swap: " + e.getMessage()));
            }
        }
    }

    /**
     * Send USE_PUSH_FIXED command to push a normally fixed row/column.
     * Cannot push the outermost tiles (avatar start positions).
     * After using this bonus, the player can still make their normal move.
     */
    public void sendUsePushFixed(int rowOrColIndex, Direction direction) {
        try {
            UsePushFixedCommandPayload payload = new UsePushFixedCommandPayload();
            payload.setType(CommandType.USE_PUSH_FIXED);
            payload.setRowOrColIndex(rowOrColIndex);
            payload.setDirection(direction);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendUsePushFixed -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                runOnUiThread(() -> onErrorMessage.accept("Failed to send use push fixed: " + e.getMessage()));
            }
        }
    }

    /**
     * Send USE_PUSH_TWICE command to activate double push mode.
     * After activation, the player can push tiles twice before making their move.
     */
    public void sendUsePushTwice() {
        try {
            UsePushTwiceCommandPayload payload = new UsePushTwiceCommandPayload();
            payload.setType(CommandType.USE_PUSH_TWICE);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendUsePushTwice -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                runOnUiThread(() -> onErrorMessage.accept("Failed to send use push twice: " + e.getMessage()));
            }
        }
    }

    // ===================== CONNECTION MANAGEMENT =====================

    /**
     * Initiates a clean disconnect. Sets intentionalDisconnect flag to prevent reconnection.
     * Uses CloseCode.NORMAL (1000) to signal intentional disconnect to server.
     */
    public void disconnectCleanly() {
        intentionalDisconnect = true;
        connectionState = ConnectionState.DISCONNECTING;

        try {
            sendDisconnect();
            Thread.sleep(50);
            closeConnection(1000, "Client disconnecting normally");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            closeConnection(1000, "Client disconnecting normally");
        }
    }


    /**
     * Attempts to reconnect to the server using stored credentials.
     * @param identifierToken The reconnection token
     * @return true if reconnect attempt was initiated, false if connection state doesn't allow it
     */
    public boolean attemptReconnect(String identifierToken) {
        if (connectionState == ConnectionState.CONNECTED ||
            connectionState == ConnectionState.CONNECTING ||
            connectionState == ConnectionState.RECONNECTING) {
            System.out.println("Cannot reconnect - already connected or connecting");
            return false;
        }

        try {
            connectionState = ConnectionState.RECONNECTING;
            intentionalDisconnect = false;

            if (!isClosed()) {
                closeBlocking();
            }

            reconnectBlocking();
            sendReconnect(identifierToken);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            connectionState = ConnectionState.DISCONNECTED;
            if (onErrorMessage != null) {
                runOnUiThread(() -> onErrorMessage.accept("Reconnect failed: " + e.getMessage()));
            }
            return false;
        }
    }

    /**
     * Initiates a fresh connection (not a reconnection).
     * @param username The username for new connection
     * @return true if connect attempt was initiated
     */
    public boolean attemptConnect(String username) {
        if (connectionState == ConnectionState.CONNECTED ||
            connectionState == ConnectionState.CONNECTING) {
            System.out.println("Cannot connect - already connected or connecting");
            return false;
        }

        try {
            connectionState = ConnectionState.CONNECTING;
            intentionalDisconnect = false;

            if (!isClosed()) {
                closeBlocking();
            }

            reconnectBlocking();
            sendConnect(username);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            connectionState = ConnectionState.DISCONNECTED;
            if (onErrorMessage != null) {
                runOnUiThread(() -> onErrorMessage.accept("Connect failed: " + e.getMessage()));
            }
            return false;
        }
    }

    public synchronized void ensureTransportConnected() {
        ReadyState rs = getReadyState();

        if (rs == ReadyState.OPEN) {
            return;
        }

        if (rs == ReadyState.NOT_YET_CONNECTED) {
            connect();
            return;
        }

        if (rs == ReadyState.CLOSED) {
            reconnect();
            return;
        }

        try {
            closeBlocking();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            close();
        }
        reconnect();
    }

    private void runOnUiThread(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) r.run();
        else SwingUtilities.invokeLater(r);
    }
}
