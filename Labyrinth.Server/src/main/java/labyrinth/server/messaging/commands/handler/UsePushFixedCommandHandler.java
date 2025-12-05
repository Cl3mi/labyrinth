package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.UsePushFixedCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class UsePushFixedCommandHandler implements ICommandHandler<UsePushFixedCommandPayload> {

    private final IGame game;
    private final PlayerSessionRegistry playerSessionRegistry;

    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_FIXED;
    }

    @Override
    public void handle(WebSocketSession session, UsePushFixedCommandPayload payload) throws Exception {
        var playerId = playerSessionRegistry.getPlayerId(session);

        if (playerId == null) {
            throw new ActionErrorException("Session is not connected to a player", ErrorCode.GENERAL); //TODO: error code?
        }

        var player = game.getPlayer(playerId);
        if(player == null) {
            throw new ActionErrorException("Player with ID " + playerId + " not found", ErrorCode.GENERAL); //TODO: error code?
        }

        var currentPlayer = game.getCurrentPlayer();

        if(player != currentPlayer) {
            throw new ActionErrorException("It's not your turn.", ErrorCode.NOT_YOUR_TURN);
        }

        game.usePushFixedBonus(player);
        //TODO: implement
    }
}
