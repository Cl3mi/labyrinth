package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

public class MovePawnCommandHandler implements ICommandHandler<MovePawnCommandHandler> {
    @Override
    public CommandType type() {
        return CommandType.MOVE_PAWN;
    }

    @Override
    public void handle(WebSocketSession session, MovePawnCommandHandler payload) throws Exception {
        //TODO: implement
    }
}
