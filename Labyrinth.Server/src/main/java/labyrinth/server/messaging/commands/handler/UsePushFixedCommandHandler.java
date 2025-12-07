package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UsePushFixedCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class UsePushFixedCommandHandler extends AbstractCommandHandler<UsePushFixedCommandPayload> {

    public UsePushFixedCommandHandler(GameService gameService,
                                      IPlayerSessionRegistry playerSessionRegistry) {
        super(gameService, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_FIXED;
    }

    @Override
    public void handle(WebSocketSession session, UsePushFixedCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        gameService.usePushFixedBonus(player);
    }
}
