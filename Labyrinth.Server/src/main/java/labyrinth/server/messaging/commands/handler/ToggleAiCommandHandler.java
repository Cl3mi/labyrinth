package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.PlayerUpdatedEventPayload;
import labyrinth.contracts.models.ToggleAiCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class ToggleAiCommandHandler extends AbstractCommandHandler<ToggleAiCommandPayload> {

    private final PlayerInfoMapper playerInfoMapper;
    private final MessageService messageService;

    public ToggleAiCommandHandler(GameService gameService,
                                  PlayerSessionRegistry playerSessionRegistry,
                                  PlayerInfoMapper playerInfoMapper,
                                  MessageService messageService) {
        super(gameService, playerSessionRegistry);
        this.playerInfoMapper = playerInfoMapper;
        this.messageService = messageService;
    }

    @Override
    public CommandType type() {
        return CommandType.TOGGLE_AI;
    }

    @Override
    public void handle(WebSocketSession session, ToggleAiCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        gameService.toggleAiForPlayer(player);

        var playerUpdatedEventPayload = new PlayerUpdatedEventPayload();
        playerUpdatedEventPayload.setType(EventType.PLAYER_UPDATED);
        playerUpdatedEventPayload.setPlayer(playerInfoMapper.toDto(player));

        messageService.broadcastToPlayers(playerUpdatedEventPayload);
    }
}
