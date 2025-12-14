package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;


@Component
public class ConnectCommandHandler extends AbstractCommandHandler<ConnectCommandPayload> {

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;


    public ConnectCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry,
                                 MessageService messageService,
                                 PlayerInfoMapper playerInfoMapper) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.playerInfoMapper = playerInfoMapper;
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
            var identifierToken = UUID.fromString(payload.getIdentifierToken());

            var playerId = playerSessionRegistry.getPlayerIdByIdentifierToken(identifierToken);

            var player = gameService.getPlayer(playerId);
            if (player == null) {
                throw new ActionErrorException("Player with ID " + playerId + " not found", ErrorCode.PLAYER_NOT_FOUND);
            }

            playerSessionRegistry.registerPlayer(playerId, identifierToken, session);

            var ackPayload = new ConnectAckEventPayload();
            ackPayload.setType(EventType.CONNECT_ACK);
            ackPayload.setIdentifierToken(identifierToken.toString());
            messageService.sendToPlayer(playerId, ackPayload);
            return;
        }

        var player = gameService.join(payload.getUsername());

        var identifierToken = UUID.randomUUID();
        playerSessionRegistry.registerPlayer(player.getId(), identifierToken, session);

        var ackPayload = new ConnectAckEventPayload();
        ackPayload.setType(EventType.CONNECT_ACK);
        ackPayload.setIdentifierToken(identifierToken.toString());

        messageService.sendToPlayer(player.getId(), ackPayload);

        var players = gameService.getPlayers()
                .stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);

        var lobbyStateUpdated = new LobbyStateEventPayload();
        lobbyStateUpdated.setType(EventType.LOBBY_STATE);
        lobbyStateUpdated.setPlayers(players);

        messageService.broadcastToPlayers(lobbyStateUpdated);
    }
}
