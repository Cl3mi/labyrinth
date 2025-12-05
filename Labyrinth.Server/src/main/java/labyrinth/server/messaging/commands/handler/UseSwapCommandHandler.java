package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UseSwapCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class UseSwapCommandHandler implements ICommandHandler<UseSwapCommandPayload> {

    private final IGame game;

    @Override
    public CommandType type() {
        return CommandType.USE_SWAP;
    }

    @Override
    public void handle(WebSocketSession session, UseSwapCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
