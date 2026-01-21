package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.MovePawnCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class MovePawnCommandHandler extends AbstractCommandHandler<MovePawnCommandPayload> {


    public MovePawnCommandHandler(GameService gameService,
                                  PlayerSessionRegistry playerSessionRegistry) {

        super(gameService, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.MOVE_PAWN;
    }

    @Override
    public void handle(WebSocketSession session, MovePawnCommandPayload payload) throws ActionErrorException {
        var player = requireExistingPlayer(session);

        requirePlayerIsCurrent(player);

        var coordinates = payload.getTargetCoordinates();
        var success = gameService.movePlayerToTile(coordinates.getRow(), coordinates.getColumn(), player);

        if (!success) {
            throw new ActionErrorException("Cannot move pawn to the specified coordinates.", ErrorCode.INVALID_MOVE);
        }
    }
}
