package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.PlayerDisconnectedEventPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.abstractions.IMessageService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
public class DisconnectCommandHandler extends AbstractCommandHandler<DisconnectCommandPayload> {

    private final IMessageService messageService;
    private final GameMapper gameMapper;

    public DisconnectCommandHandler(IGame game, IPlayerSessionRegistry playerSessionRegistry, IMessageService messageService, GameMapper gameMapper) {
        super(game, playerSessionRegistry);

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

        game.leave(player);

        var disconnectedPayload = new PlayerDisconnectedEventPayload();
        disconnectedPayload.setType(EventType.PLAYER_DISCONNECTED);
        disconnectedPayload.setPlayerId(player.getId().toString());
        messageService.broadcastToPlayers(disconnectedPayload);

        var gameState = gameMapper.toGameStateDto(game);
        messageService.broadcastToPlayers(gameState);
    }
}
