package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
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
    private final PlayerSessionRegistry playerSessionRegistry;
    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;


    @Override
    public CommandType type() {
        return CommandType.CONNECT;
    }

    @Override
    public void handle(WebSocketSession session, ConnectCommandPayload payload) throws Exception {

        if (playerSessionRegistry.isSessionRegistered(session)) {
            throw new ActionErrorException("Session is already connected", ErrorCode.GENERAL); //TODO: error code?
        }

        if (payload.getPlayerId() != null) {
            var playerId = UUID.fromString(payload.getPlayerId());
            var player = game.getPlayer(playerId);

            playerSessionRegistry.registerPlayer(player, session);

            var ackPayload = new ConnectAckEventPayload();
            ackPayload.setType(EventType.CONNECT_ACK);
            ackPayload.setPlayerId(player.getId().toString());
            messageService.sendToPlayer(player, ackPayload);
            return;
        }

        var player = game.join(payload.getUsername());

        playerSessionRegistry.registerPlayer(player, session);

        var ackPayload = new ConnectAckEventPayload();
        ackPayload.setType(EventType.CONNECT_ACK);
        ackPayload.setPlayerId(player.getId().toString());

        messageService.sendToPlayer(player, ackPayload);

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
