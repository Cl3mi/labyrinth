package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.abstractions.IMessageService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;


@Component
@RequiredArgsConstructor
public class ConnectCommandHandler implements ICommandHandler<ConnectCommandPayload> {

    private final IGame game;
    private final IPlayerSessionRegistry IPlayerSessionRegistry;
    private final IMessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;


    @Override
    public CommandType type() {
        return CommandType.CONNECT;
    }

    @Override
    public void handle(WebSocketSession session, ConnectCommandPayload payload) throws Exception {

        if (IPlayerSessionRegistry.isSessionRegistered(session)) {
            throw new ActionErrorException("Session is already connected", ErrorCode.GENERAL); //TODO: error code?
        }

        if (payload.getIdentifierToken() != null) {
            var identifierToken = UUID.fromString(payload.getIdentifierToken());

            var playerId = IPlayerSessionRegistry.getPlayerIdByIdentifierToken(identifierToken);
            var player = game.getPlayer(playerId);

            if(player == null) {
                throw new ActionErrorException("Player with given identifier token not found", ErrorCode.GENERAL);
            }

            IPlayerSessionRegistry.registerPlayer(playerId, identifierToken, session);

            var ackPayload = new ConnectAckEventPayload();
            ackPayload.setType(EventType.CONNECT_ACK);
            ackPayload.setIdentifierToken(identifierToken.toString());
            messageService.sendToPlayer(playerId, ackPayload);
            return;
        }

        var player = game.join(payload.getUsername());

        var identifierToken = UUID.randomUUID();
        IPlayerSessionRegistry.registerPlayer(player.getId(), identifierToken, session);

        var ackPayload = new ConnectAckEventPayload();
        ackPayload.setType(EventType.CONNECT_ACK);
        ackPayload.setIdentifierToken(identifierToken.toString());

        messageService.sendToPlayer(player.getId(), ackPayload);

        var players = game.getPlayers()
                .stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);

        var lobbyStateUpdated = new LobbyStateEventPayload();
        lobbyStateUpdated.setType(EventType.LOBBY_STATE);
        lobbyStateUpdated.setPlayers(players);

        messageService.broadcastToPlayers(lobbyStateUpdated);
    }
}
