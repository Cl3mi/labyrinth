package labyrinth.client.messaging;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.*;
import lombok.Setter;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

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

    public GameClient(URI serverUri) {
        super(serverUri);
    }

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
                case GAME_STARTED -> {
                    GameStartedEventPayload payload =
                            mapper.treeToValue(root, GameStartedEventPayload.class);
                    if (onGameStarted != null) onGameStarted.accept(payload);
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
            payload.setPlayerId(playerId);   // <- hier playerId statt identifierToken

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

    public void sendStartGame(BoardSize boardSize, int treasureCardCountPerPlayer) {
        try {
            StartGameCommandPayload payload = new StartGameCommandPayload();
            payload.setType(CommandType.START_GAME);
            payload.setBoardSize(boardSize);
            payload.setTreasureCardCount(treasureCardCountPerPlayer);

            String json = mapper.writeValueAsString(payload);
            send(json);
        } catch (Exception e) {
            e.printStackTrace();
            if (onErrorMessage != null) {
                onErrorMessage.accept("Failed to send start game: " + e.getMessage());
            }
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
}
