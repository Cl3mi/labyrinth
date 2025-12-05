package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UsePushFixedCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class UsePushFixedCommandHandler implements ICommandHandler<UsePushFixedCommandPayload> {

    private final IGame game;

    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_FIXED;
    }

    @Override
    public void handle(WebSocketSession session, UsePushFixedCommandPayload payload) throws Exception {
        //TODO: implement
    }
}
