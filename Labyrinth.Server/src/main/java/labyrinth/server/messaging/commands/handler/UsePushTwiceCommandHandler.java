package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UsePushTwiceCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class UsePushTwiceCommandHandler extends AbstractCommandHandler<UsePushTwiceCommandPayload> {

    public UsePushTwiceCommandHandler(IGame game, IPlayerSessionRegistry playerSessionRegistry) {
        super(game, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_TWICE;
    }

    @Override
    public void handle(WebSocketSession session, UsePushTwiceCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        game.usePushTwiceBonus(player);
    }
}
