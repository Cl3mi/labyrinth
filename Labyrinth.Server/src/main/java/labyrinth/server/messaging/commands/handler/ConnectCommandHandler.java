package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
public class ConnectCommandHandler implements ICommandHandler<ConnectCommandPayload> {
    private final GameService gameService;
    private final PlayerSessionRegistry playerSessionRegistry;
    private final MessageService messageService;

    public ConnectCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry,
                                 MessageService messageService) {
        this.gameService = gameService;
        this.playerSessionRegistry = playerSessionRegistry;
        this.messageService = messageService;
    }

    @Override
    public CommandType type() {
        return CommandType.CONNECT;
    }

    @Override
    public void handle(WebSocketSession session, ConnectCommandPayload payload) throws Exception {

        if(playerSessionRegistry.isSessionRegistered(session.getId())) {
            throw new ActionErrorException("Session is already connected", ErrorCode.GENERAL); //TODO: error code?
        }

        var player =  gameService.connectPlayer(payload.getUsername());

        playerSessionRegistry.registerPlayer(player, session);

        var ackPayload = new ConnectAckEventPayload();
        ackPayload.setType(EventType.CONNECT_ACK);
        ackPayload.setPlayerId(player.getId().toString());

        messageService.sendToPlayer(player, ackPayload);

        var players = gameService.getPlayersInLobby()
                .stream()
                .map(p -> {
                    var playerInfo = new PlayerInfo();
                    playerInfo.setId(p.getId().toString());
                    playerInfo.setIsAdmin(p.isAdmin());
                    playerInfo.setColor(p.getColor());
                    playerInfo.setName(p.getUsername());
                    return playerInfo;
                })
                .toArray(PlayerInfo[]::new);

        var lobbyStateUpdated = new LobbyStateEventPayload();
        lobbyStateUpdated.setType(EventType.LOBBY_STATE);
        lobbyStateUpdated.setPlayers(players);

        messageService.broadcastToPlayers(lobbyStateUpdated);
    }
}
