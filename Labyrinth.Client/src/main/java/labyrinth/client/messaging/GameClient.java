package labyrinth.client.messaging;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.*;
import lombok.Setter;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import javax.swing.*;
import java.net.URI;
import java.util.Set;
import java.util.function.Consumer;

public class GameClient extends WebSocketClient {

    private final ObjectMapper mapper = new ObjectMapper();

    // Callback-Hooks fürs UI
    @Setter
    private Consumer<ConnectAckEventPayload> onConnectAck;
    @Setter
    private Consumer<LobbyStateEventPayload> onLobbyState;
    @Setter
    private Consumer<GameStartedEventPayload> onGameStarted;
    @Setter
    private Consumer<GameStateUpdateEventPayload> onGameStateUpdate;
    @Setter
    private Consumer<String> onErrorMessage;
    @Setter
    private Runnable onOpenHook;

    public GameClient(URI serverUri) {
        super(serverUri);
    }

    @Override
    public void onOpen(ServerHandshake handshakedata) {
        System.out.println("WebSocket connected");
        if (onOpenHook != null) onOpenHook.run();
    }


    @Override
    public void onMessage(String message) {
        try {
            JsonNode root = mapper.readTree(message);

            System.out.println("WS IN: " + message);

            JsonNode typeNode = root.get("type");
            if (typeNode == null) {
                runOnUiThread(() -> {
                    if (onErrorMessage != null) {
                        onErrorMessage.accept("Unknown message (no type): " + message);
                    }
                });
                return;
            }

            EventType type = EventType.valueOf(typeNode.asText());

            // If the server uses an envelope: { "type": "...", "payload": { ... } }
            // deserialize from "payload". If there's no payload field, fall back to root.
            JsonNode payloadNode = root.get("payload");
            if (payloadNode == null || payloadNode.isNull()) {
                payloadNode = root;
            }

            switch (type) {
                case CONNECT_ACK -> {
                    ConnectAckEventPayload payload =
                            mapper.treeToValue(payloadNode, ConnectAckEventPayload.class);

                    if (onConnectAck != null) {
                        runOnUiThread(() -> onConnectAck.accept(payload));
                    }
                }
                case LOBBY_STATE -> {
                    LobbyStateEventPayload payload =
                            mapper.treeToValue(payloadNode, LobbyStateEventPayload.class);

                    if (onLobbyState != null) {
                        runOnUiThread(() -> onLobbyState.accept(payload));
                    }
                }
                case GAME_STARTED -> {
                    System.out.println("WS EVENT: GAME_STARTED");
                    GameStartedEventPayload payload =
                            mapper.treeToValue(payloadNode, GameStartedEventPayload.class);

                    if (onGameStarted != null) {
                        runOnUiThread(() -> onGameStarted.accept(payload));
                    }
                }
                case GAME_STATE_UPDATE -> {
                    System.out.println("WS EVENT: GAME_STATE_UPDATE");
                    GameStateUpdateEventPayload payload =
                            mapper.treeToValue(payloadNode, GameStateUpdateEventPayload.class);

                    if (onGameStateUpdate != null) {
                        runOnUiThread(() -> onGameStateUpdate.accept(payload));
                    }
                }
                case ACTION_ERROR -> {
                    System.out.println("WS EVENT: ACTION_ERROR " + message);
                    ActionErrorEventPayload payload =
                            mapper.treeToValue(payloadNode, ActionErrorEventPayload.class);

                    if (onErrorMessage != null) {
                        String msg = payload.getMessage() != null ? payload.getMessage() : payload.toString();
                        runOnUiThread(() -> onErrorMessage.accept(msg));
                    }
                }
                default -> System.out.println("Unhandled event type: " + type + " message=" + message);
            }
        } catch (Exception e) {
            e.printStackTrace();
            runOnUiThread(() -> {
                if (onErrorMessage != null) {
                    onErrorMessage.accept("Failed to parse message: " + e.getMessage());
                }
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

    // =================================================================================
    // COMMANDS SENDEN
    // =================================================================================

    public void sendConnect(String username) {
        try {
            ConnectCommandPayload payload = new ConnectCommandPayload();
            payload.setType(CommandType.CONNECT);
            payload.setUsername(username);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendReconnect(String playerId) {
        try {
            ConnectCommandPayload payload = new ConnectCommandPayload();
            payload.setType(CommandType.CONNECT);
            payload.setIdentifierToken(playerId);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                onErrorMessage.accept("Failed to send reconnect: " + e.getMessage());
            }
        }
    }

    public void sendDisconnect() {
        try {
            DisconnectCommandPayload payload = new DisconnectCommandPayload();
            payload.setType(CommandType.DISCONNECT);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendStartGame(BoardSize boardSize,
                              int treasureCardCountPerPlayer,
                              int totalBonusCount,
                              Integer gameDurationInSeconds) {
        try {
            StartGameCommandPayload payload = new StartGameCommandPayload();
            payload.setType(CommandType.START_GAME);
            payload.setBoardSize(boardSize);
            payload.setTreasureCardCount(treasureCardCountPerPlayer);
            payload.setTotalBonusCount(totalBonusCount);
            payload.setGameDurationInSeconds(gameDurationInSeconds);

            String json = mapper.writeValueAsString(payload);
            System.out.println("sendStartGame isOpen=" + isOpen() + " isClosing=" + isClosing() + " isClosed=" + isClosed());
            System.out.println("sendStartGame -> " + json);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                onErrorMessage.accept("Failed to send start game: " + e.getMessage());
            }
        }
    }

    public void sendMovePawn(int targetRow, int targetCol) {
        try {
            MovePawnCommandPayload payload = new MovePawnCommandPayload();
            payload.setType(CommandType.MOVE_PAWN);

            Coordinates coords = new Coordinates();
            coords.setX(targetCol);
            coords.setY(targetRow);
            payload.setTargetCoordinates(coords);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendPushTile(
            int rowOrColIndex,
            labyrinth.contracts.models.Direction direction,
            Set<labyrinth.contracts.models.Direction> extraTileEntrances
    ) {
        try {
            PushTileCommandPayload payload = new PushTileCommandPayload();
            payload.setType(CommandType.PUSH_TILE);
            payload.setRowOrColIndex(rowOrColIndex);
            payload.setDirection(direction);

            Direction[] entrances = extraTileEntrances.toArray(new Direction[0]);
            payload.setTileEntrances(entrances);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void runOnUiThread(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) {
            r.run();
        } else {
            SwingUtilities.invokeLater(r);
        }
    }
}
