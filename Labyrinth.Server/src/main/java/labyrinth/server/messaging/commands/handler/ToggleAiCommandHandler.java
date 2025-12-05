package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.ToggleAiCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class ToggleAiCommandHandler implements ICommandHandler<ToggleAiCommandPayload> {

    private final IGame game;
    private final PlayerSessionRegistry playerSessionRegistry;

    @Override
    public CommandType type() {
        return CommandType.TOGGLE_AI;
    }

    @Override
    public void handle(WebSocketSession session, ToggleAiCommandPayload payload) throws Exception {
        var playerId = playerSessionRegistry.getPlayerId(session);

        if (playerId == null) {
            throw new ActionErrorException("Session is not connected to a player", ErrorCode.GENERAL); //TODO: error code?
        }

        var player = game.getPlayer(playerId);
        if(player == null) {
            throw new ActionErrorException("Player with ID " + playerId + " not found", ErrorCode.GENERAL); //TODO: error code?
        }

        game.toggleAiForPlayer(player);
    }
}
