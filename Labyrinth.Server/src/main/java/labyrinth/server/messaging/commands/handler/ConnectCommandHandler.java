package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;


@Component
public class ConnectCommandHandler extends AbstractCommandHandler<ConnectCommandPayload> {

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;
    private final GameMapper gameMapper;


    public ConnectCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry,
                                 MessageService messageService,
                                 PlayerInfoMapper playerInfoMapper,
                                 GameMapper gameMapper) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.playerInfoMapper = playerInfoMapper;
        this.gameMapper = gameMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.CONNECT;
    }

    @Override
    public void handle(WebSocketSession session, ConnectCommandPayload payload) throws Exception {
        if (playerSessionRegistry.isSessionRegistered(session)) {
            throw new ActionErrorException("Session is already connected", ErrorCode.GENERAL);
        }

        if (payload.getIdentifierToken() != null) {
            UUID identifierToken;
            try {
                identifierToken = UUID.fromString(payload.getIdentifierToken());
            } catch (IllegalArgumentException ex) {
                identifierToken = null;
            }

            if (identifierToken != null) {
                var playerId = playerSessionRegistry.getPlayerIdByIdentifierToken(identifierToken);

                if (playerId == null) {
                    if (payload.getUsername() == null || payload.getUsername().isBlank()) {
                        throw new ActionErrorException("Reconnection token is invalid or expired", ErrorCode.PLAYER_NOT_FOUND);
                    }

                    connectNewPlayer(session, payload.getUsername());
                    return;
                }

                var player = gameService.getPlayer(playerId);
                if (player == null) {
                    // Player was removed from game but token still exists in registry - clean it up
                    playerSessionRegistry.removePlayer(playerId);
                    throw new ActionErrorException("Player session has expired", ErrorCode.PLAYER_NOT_FOUND);
                }

                registerExistingPlayer(playerId, identifierToken, session);
            } else {
                // identifierToken couldn't be parsed -> treat like unknown token above
                if (payload.getUsername() == null || payload.getUsername().isBlank()) {
                    throw new ActionErrorException("Reconnection token is invalid or expired", ErrorCode.PLAYER_NOT_FOUND);
                }

                connectNewPlayer(session, payload.getUsername());
            }
            return;
        }

        connectNewPlayer(session, payload.getUsername());
    }


    private void connectNewPlayer(WebSocketSession session, String username) throws Exception {
        var newPlayer = gameService.join(username);
        var newIdentifier = UUID.randomUUID();
        playerSessionRegistry.registerPlayer(newPlayer.getId(), newIdentifier, session);

        registerAndSendAck(newPlayer.getId(), newIdentifier);
        sendStateForPlayer(newPlayer.getId());
    }

    private void registerExistingPlayer(UUID playerId, UUID identifierToken, WebSocketSession session) throws Exception {
        playerSessionRegistry.registerPlayer(playerId, identifierToken, session);
        registerAndSendAck(playerId, identifierToken);
        sendStateForPlayer(playerId);
    }

    private void registerAndSendAck(UUID playerId, UUID identifierToken) {
        var ackPayload = createAckPayload(playerId, identifierToken);
        messageService.sendToPlayer(playerId, ackPayload);
    }

    private void sendStateForPlayer(UUID playerId) {
        RoomState roomState = gameService.getGameState();
        if (roomState == RoomState.IN_GAME) {
            var gameStateDto = gameService.withGameReadLock(gameMapper::toGameStateDto);
            gameStateDto.setType(EventType.GAME_STATE_UPDATE);
            messageService.sendToPlayer(playerId, gameStateDto);
        } else {
            var lobbyStatePayload = createLobbyStatePayload();
            messageService.broadcastToPlayers(lobbyStatePayload);
        }
    }

    private ConnectAckEventPayload createAckPayload(UUID playerId, UUID identifierToken) {
        var ackPayload = new ConnectAckEventPayload();
        ackPayload.setType(EventType.CONNECT_ACK);
        ackPayload.setPlayerId(playerId.toString());
        ackPayload.setIdentifierToken(identifierToken.toString());
        return ackPayload;
    }

    private LobbyStateEventPayload createLobbyStatePayload() {
        var players = gameService.getPlayers()
                .stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);

        var lobbyStateUpdated = new LobbyStateEventPayload();
        lobbyStateUpdated.setType(EventType.LOBBY_STATE);
        lobbyStateUpdated.setPlayers(players);

        return lobbyStateUpdated;
    }
}
