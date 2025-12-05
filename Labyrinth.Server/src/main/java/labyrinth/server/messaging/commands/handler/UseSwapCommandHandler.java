package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UseSwapCommandPayload;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

public class UseSwapCommandHandler implements ICommandHandler<UseSwapCommandPayload> {
    @Override
    public CommandType type() {
        return CommandType.USE_SWAP;
    }

    @Override
    public void handle(WebSocketSession session, UseSwapCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
