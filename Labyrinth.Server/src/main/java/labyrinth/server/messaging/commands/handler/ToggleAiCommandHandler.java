package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ToggleAiCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class ToggleAiCommandHandler implements ICommandHandler<ToggleAiCommandPayload> {

    private final IGame game;

    @Override
    public CommandType type() {
        return CommandType.TOGGLE_AI;
    }

    @Override
    public void handle(WebSocketSession session, ToggleAiCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
