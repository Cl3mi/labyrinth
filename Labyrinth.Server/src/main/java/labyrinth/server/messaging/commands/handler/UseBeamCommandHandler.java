package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UseBeamCommandPayload;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

public class UseBeamCommandHandler implements ICommandHandler<UseBeamCommandPayload> {

    @Override
    public CommandType type() {
        return CommandType.USE_BEAM;
    }

    @Override
    public void handle(WebSocketSession session, UseBeamCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
