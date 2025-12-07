package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ToggleAiCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class ToggleAiCommandHandler extends AbstractCommandHandler<ToggleAiCommandPayload> {

    public ToggleAiCommandHandler(IGame game, IPlayerSessionRegistry playerSessionRegistry) {
        super(game, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.TOGGLE_AI;
    }

    @Override
    public void handle(WebSocketSession session, ToggleAiCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        game.toggleAiForPlayer(player);
    }
}
