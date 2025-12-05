package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.PushTileCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class PushTileCommandHandler implements ICommandHandler<PushTileCommandPayload> {

    private final IGame game;

    @Override
    public CommandType type() {
        return CommandType.PUSH_TILE;
    }

    @Override
    public void handle(WebSocketSession session, PushTileCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
