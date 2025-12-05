package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UsePushTwiceCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class UsePushTwiceCommandHandler implements ICommandHandler<UsePushTwiceCommandPayload> {

    private final IGame game;

    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_TWICE;
    }

    @Override
    public void handle(WebSocketSession session, UsePushTwiceCommandPayload payload) throws Exception {
        //TODO: implement
    }
}

