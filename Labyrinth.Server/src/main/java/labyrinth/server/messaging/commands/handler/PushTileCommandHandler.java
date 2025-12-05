package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.PushTileCommandPayload;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

public class PushTileCommandHandler implements ICommandHandler<PushTileCommandPayload> {
    @Override
    public CommandType type() {
        return CommandType.PUSH_TILE;
    }

    @Override
    public void handle(WebSocketSession session, PushTileCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
