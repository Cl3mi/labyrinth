package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UsePushTwiceCommandPayload;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

public class UsePushTwiceCommandHandler implements ICommandHandler<UsePushTwiceCommandPayload> {
    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_TWICE;
    }

    @Override
    public void handle(WebSocketSession session, UsePushTwiceCommandPayload payload) throws Exception {
        //TODO: implement
    }
}

