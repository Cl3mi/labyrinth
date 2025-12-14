package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.PlayerUpdatedEventPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
public class DisconnectCommandHandler extends AbstractCommandHandler<DisconnectCommandPayload> {

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;

    public DisconnectCommandHandler(GameService gameService,
                                    PlayerSessionRegistry playerSessionRegistry,
                                    MessageService messageService,
                                    PlayerInfoMapper playerInfoMapper) {
        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.playerInfoMapper = playerInfoMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.DISCONNECT;
    }

    @Override
    public void handle(WebSocketSession session, DisconnectCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);

        gameService.leave(player);

        var playerUpdatedEventPayload = new PlayerUpdatedEventPayload();
        playerUpdatedEventPayload.setType(EventType.PLAYER_UPDATED);
        playerUpdatedEventPayload.setPlayer(playerInfoMapper.toDto(player));

        messageService.broadcastToPlayers(playerUpdatedEventPayload);
    }
}
