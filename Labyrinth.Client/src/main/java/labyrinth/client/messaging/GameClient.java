package labyrinth.client.messaging;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import labyrinth.contracts.models.*;
import lombok.Setter;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import javax.swing.*;
import java.net.URI;
import java.util.function.Consumer;

public class GameClient extends WebSocketClient {

    private final ObjectMapper mapper;

    @Setter private Consumer<ConnectAckEventPayload> onConnectAck;
    @Setter private Consumer<LobbyStateEventPayload> onLobbyState;
    @Setter private Consumer<GameStateEventPayload> onGameStarted;

    // ✅ einzig vorhandenes State-Payload
    @Setter private Consumer<GameStateEventPayload> onGameStateUpdate;

    @Setter private Consumer<String> onErrorMessage;
    @Setter private Runnable onOpenHook;

    public GameClient(URI serverUri) {
        super(serverUri);

        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
    }

    @Override
    public void onOpen(ServerHandshake handshakedata) {
        System.out.println("WebSocket connected");
        if (onOpenHook != null) onOpenHook.run();
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

            // Envelope support {type, payload}, fallback root
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
                    // ✅ korrektes Payload
                    GameStateEventPayload payload = mapper.treeToValue(payloadNode, GameStateEventPayload.class);
                    if (onGameStateUpdate != null) runOnUiThread(() -> onGameStateUpdate.accept(payload));
                }
                case ACTION_ERROR -> {
                    ActionErrorEventPayload payload = mapper.treeToValue(payloadNode, ActionErrorEventPayload.class);
                    String msg = payload.getMessage() != null ? payload.getMessage() : payload.toString();
                    if (onErrorMessage != null) runOnUiThread(() -> onErrorMessage.accept(msg));
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
        runOnUiThread(() -> {
            if (onErrorMessage != null) {
                onErrorMessage.accept("Disconnected (code=" + code + "): " + (reason == null ? "" : reason));
            }
        });
    }

    @Override
    public void onError(Exception ex) {
        ex.printStackTrace();
        runOnUiThread(() -> {
            if (onErrorMessage != null) {
                onErrorMessage.accept("WebSocket error: " + ex.getClass().getSimpleName() + ": " + ex.getMessage());
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
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendStartGame(BoardSize boardSize,
                              int treasureCardCount,
                              int totalBonusCount,
                              Integer gameDurationInSeconds) {
        try {
            StartGameCommandPayload payload = new StartGameCommandPayload();
            payload.setType(CommandType.START_GAME);
            payload.setBoardSize(boardSize);
            payload.setTreasureCardCount(treasureCardCount);
            payload.setTotalBonusCount(totalBonusCount);
            payload.setGameDurationInSeconds(gameDurationInSeconds != null ? gameDurationInSeconds : 0);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendStartGame isOpen=" + isOpen() + " isClosing=" + isClosing() + " isClosed=" + isClosed());
            System.out.println("sendStartGame -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            runOnUiThread(() -> {
                if (onErrorMessage != null) onErrorMessage.accept("Failed to send start game: " + e.getMessage());
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

    // ✅ Korrekt gemäß Contract: nur direction, kein extraTileEntrances
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

    private void runOnUiThread(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) r.run();
        else SwingUtilities.invokeLater(r);
    }
}
