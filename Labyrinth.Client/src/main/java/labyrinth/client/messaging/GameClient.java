package labyrinth.client.messaging;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.*;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.util.Set;
import java.util.function.Consumer;

public class GameClient extends WebSocketClient {

    private final ObjectMapper mapper = new ObjectMapper();

    // Callback-Hooks fürs UI
    private Consumer<ConnectAckEventPayload> onConnectAck;
    private Consumer<LobbyStateEventPayload> onLobbyState;
    private Consumer<GameStateUpdateEventPayload> onGameStateUpdate;
    private Consumer<String> onErrorMessage;

    public GameClient(URI serverUri) {
        super(serverUri);
    }

    // ====== Callback-Setter ======

    public void setOnConnectAck(Consumer<ConnectAckEventPayload> onConnectAck) {
        this.onConnectAck = onConnectAck;
    }

    public void setOnLobbyState(Consumer<LobbyStateEventPayload> onLobbyState) {
        this.onLobbyState = onLobbyState;
    }

    public void setOnGameStateUpdate(Consumer<GameStateUpdateEventPayload> onGameStateUpdate) {
        this.onGameStateUpdate = onGameStateUpdate;
    }

    public void setOnErrorMessage(Consumer<String> onErrorMessage) {
        this.onErrorMessage = onErrorMessage;
    }

    // ====== Lifecycle ======

    @Override
    public void onOpen(ServerHandshake handshakedata) {
        System.out.println("WebSocket connected");
    }

    @Override
    public void onMessage(String message) {
        try {
            JsonNode root = mapper.readTree(message);
            JsonNode typeNode = root.get("type");
            if (typeNode == null) {
                System.err.println("Unknown message (no type): " + message);
                return;
            }

            EventType type = EventType.valueOf(typeNode.asText());

            switch (type) {
                case CONNECT_ACK -> {
                    var payload = mapper.treeToValue(root, ConnectAckEventPayload.class);
                    if (onConnectAck != null) onConnectAck.accept(payload);
                }
                case LOBBY_STATE -> {
                    var payload = mapper.treeToValue(root, LobbyStateEventPayload.class);
                    if (onLobbyState != null) onLobbyState.accept(payload);
                }
                case GAME_STATE_UPDATE -> {
                    var payload = mapper.treeToValue(root, GameStateUpdateEventPayload.class);
                    if (onGameStateUpdate != null) onGameStateUpdate.accept(payload);
                }
                case ACTION_ERROR -> {
                    var payload = mapper.treeToValue(root, ActionErrorEventPayload.class);
                    if (onErrorMessage != null && payload.getMessage() != null) {
                        onErrorMessage.accept(payload.getMessage());
                    }
                }
                default -> System.out.println("Unhandled event type: " + type + " message=" + message);
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                onErrorMessage.accept("Failed to parse message: " + e.getMessage());
            }
        }
    }

    @Override
    public void onClose(int code, String reason, boolean remote) {
        System.out.println("WebSocket closed: " + reason);
    }

    @Override
    public void onError(Exception ex) {
        ex.printStackTrace();
        if (onErrorMessage != null) {
            onErrorMessage.accept("WebSocket error: " + ex.getMessage());
        }
    }

    // ====== Commands senden ======

    /** Neues Verbinden mit Username (entspricht dem Teil ohne playerId im ConnectCommandHandler) */
    public void sendConnect(String username) {
        try {
            var payload = new ConnectCommandPayload();
            payload.setType(CommandType.CONNECT);
            payload.setUsername(username); // playerId bleibt null → neuer Spieler

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /** Reconnect mit bestehender PlayerId */
    public void sendReconnect(String playerId) {
        try {
            var payload = new ConnectCommandPayload();
            payload.setType(CommandType.CONNECT);
            payload.setPlayerId(playerId);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /** Beispiel für einen Spielzug: Pawn bewegen */
    public void sendMovePawn(int targetRow, int targetCol) {
        try {
            MovePawnCommandPayload payload = new MovePawnCommandPayload();
            payload.setType(CommandType.MOVE_PAWN);

            Coordinates coords = new Coordinates();

            // MAPPING: BoardPanel gibt row/col → Contracts brauchen x/y
            coords.setX(targetCol);
            coords.setY(targetRow);

            payload.setTargetCoordinates(coords);

            String json = mapper.writeValueAsString(payload);
            send(json);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /** Beispiel für einen Spielzug: Tile schieben */
    public void sendPushTile(
            int rowOrColIndex,
            labyrinth.client.enums.Direction clientDirection,
            Set<labyrinth.client.enums.Direction> extraTileEntrances
    ) {
        try {
            PushTileCommandPayload payload = new PushTileCommandPayload();
            payload.setType(CommandType.PUSH_TILE);

            // Index setzen
            payload.setRowOrColIndex(rowOrColIndex);

            // Richtung: Client-Enum -> Contracts-Enum
            labyrinth.contracts.models.Direction dir =
                    labyrinth.contracts.models.Direction.valueOf(clientDirection.name());
            payload.setDirection(dir);

            // Entrances der Extra-Tile mappen: Client-Enum -> Contracts-Enum[]
            labyrinth.contracts.models.Direction[] entrances = extraTileEntrances.stream()
                    .map(d -> labyrinth.contracts.models.Direction.valueOf(d.name()))
                    .toArray(labyrinth.contracts.models.Direction[]::new);

            payload.setTileEntrances(entrances);

            String json = mapper.writeValueAsString(payload);
            send(json);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
