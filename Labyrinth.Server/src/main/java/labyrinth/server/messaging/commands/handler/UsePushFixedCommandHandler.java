package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UsePushFixedCommandPayload;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

public class UsePushFixedCommandHandler implements ICommandHandler<UsePushFixedCommandPayload> {
    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_FIXED;
    }

    @Override
    public void handle(WebSocketSession session, UsePushFixedCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
