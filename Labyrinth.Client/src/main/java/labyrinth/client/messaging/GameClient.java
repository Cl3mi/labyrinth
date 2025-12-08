package labyrinth.client.messaging;

import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.*;

import lombok.Setter;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Dünner Wrapper um Java-WebSocket, der direkt mit den Contracts-Payloads
 * (labyrinth.contracts.models.*) spricht.
 */
public class GameClient extends WebSocketClient {

    private final ObjectMapper mapper = new ObjectMapper();

    // Callback-Hooks fürs UI
    @Setter
    private Consumer<ConnectAckEventPayload> onConnectAck;
    @Setter
    private Consumer<LobbyStateEventPayload> onLobbyState;
    @Setter
    private Consumer<GameStateUpdateEventPayload> onGameStateUpdate;
    @Setter
    private Consumer<String> onErrorMessage;

    public GameClient(URI serverUri) {
        super(serverUri);
    }

    // ====== Callback-Setter ======

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
                    ConnectAckEventPayload payload =
                            mapper.treeToValue(root, ConnectAckEventPayload.class);
                    if (onConnectAck != null) onConnectAck.accept(payload);
                }
                case LOBBY_STATE -> {
                    LobbyStateEventPayload payload =
                            mapper.treeToValue(root, LobbyStateEventPayload.class);
                    if (onLobbyState != null) onLobbyState.accept(payload);
                }
                case GAME_STATE_UPDATE -> {
                    GameStateUpdateEventPayload payload =
                            mapper.treeToValue(root, GameStateUpdateEventPayload.class);
                    if (onGameStateUpdate != null) onGameStateUpdate.accept(payload);
                }
                case ACTION_ERROR -> {
                    ActionErrorEventPayload payload =
                            mapper.treeToValue(root, ActionErrorEventPayload.class);
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

    // =================================================================================
    // COMMANDS SENDEN
    // =================================================================================

    /** Neues Verbinden mit Username (entspricht dem Teil ohne playerId im ConnectCommandHandler). */
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

    /** Reconnect mit bestehender PlayerId. */
    public void sendReconnect(String playerId) {
        try {
            ConnectCommandPayload payload = new ConnectCommandPayload();
            payload.setType(CommandType.CONNECT);
            payload.setPlayerId(playerId);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /** Beispiel: Pawn bewegen (BoardPanel liefert row/col, Contracts wollen x/y). */
    public void sendMovePawn(int targetRow, int targetCol) {
        try {
            MovePawnCommandPayload payload = new MovePawnCommandPayload();
            payload.setType(CommandType.MOVE_PAWN);

            Coordinates coords = new Coordinates();
            // Mapping: x = column, y = row (so hast du es geschrieben)
            coords.setX(targetCol);
            coords.setY(targetRow);

            payload.setTargetCoordinates(coords);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /** Beispiel: Tile schieben. */
    public void sendPushTile(
            int rowOrColIndex,
            labyrinth.client.enums.Direction clientDirection,
            Set<labyrinth.client.enums.Direction> extraTileEntrances
    ) {
        try {
            PushTileCommandPayload payload = new PushTileCommandPayload();
            payload.setType(CommandType.PUSH_TILE);

            payload.setRowOrColIndex(rowOrColIndex);

            // Richtung: Client-Enum -> Contracts-Enum
            Direction dir = Direction.valueOf(clientDirection.name());
            payload.setDirection(dir);

            // Entrances der Extra-Tile: Client-Enum -> Contracts-Enum[]
            Direction[] entrances = extraTileEntrances.stream()
                    .map(d -> Direction.valueOf(d.name()))
                    .toArray(Direction[]::new);

            payload.setTileEntrances(entrances);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /** StartGame-Kommando entsprechend deiner Contracts-StartGameCommandPayload. */
    public void sendStartGame(BoardSize boardSize, int treasureCardCountPerPlayer) {
        try {
            StartGameCommandPayload payload = new StartGameCommandPayload();
            payload.setType(CommandType.START_GAME);
            payload.setBoardSize(boardSize);

            // Dein Contract-Feld heißt treasureCardCount (int)
            payload.setTreasureCardCount(treasureCardCountPerPlayer);

            // Optional:
            // payload.setTotalBonusCount(0);
            // payload.setGameDurationInSeconds(1800);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                onErrorMessage.accept("Failed to send start game: " + e.getMessage());
            }
        }
    }
}
