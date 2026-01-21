package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.RotateTileCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class RotateTileCommandHandler extends AbstractCommandHandler<RotateTileCommandPayload> {


    protected RotateTileCommandHandler(GameService gameService,
                                       PlayerSessionRegistry playerSessionRegistry) {
        super(gameService, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.ROTATE_TILE;
    }

    @Override
    public void handle(WebSocketSession session, RotateTileCommandPayload payload) throws ActionErrorException {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        gameService.rotateExtraTileClockwise(player);
    }
}
