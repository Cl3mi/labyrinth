package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
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
