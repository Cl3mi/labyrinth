package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.PlayerDisconnectedEventPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
public class DisconnectCommandHandler extends AbstractCommandHandler<DisconnectCommandPayload> {

    private final MessageService messageService;
    private final GameMapper gameMapper;

    public DisconnectCommandHandler(GameService gameService,
                                    PlayerSessionRegistry playerSessionRegistry,
                                    MessageService messageService,
                                    GameMapper gameMapper) {
        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.gameMapper = gameMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.DISCONNECT;
    }

    @Override
    public void handle(WebSocketSession session, DisconnectCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);

        gameService.leave(player);

        var disconnectedPayload = new PlayerDisconnectedEventPayload();
        disconnectedPayload.setType(EventType.PLAYER_DISCONNECTED);
        disconnectedPayload.setPlayerId(player.getId().toString());
        messageService.broadcastToPlayers(disconnectedPayload);

        var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
        messageService.broadcastToPlayers(gameState);
    }
}
