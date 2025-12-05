package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ToggleAiCommandPayload;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

public class ToggleAiCommandHandler implements ICommandHandler<ToggleAiCommandPayload> {
    @Override
    public CommandType type() {
        return CommandType.TOGGLE_AI;
    }

    @Override
    public void handle(WebSocketSession session, ToggleAiCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
