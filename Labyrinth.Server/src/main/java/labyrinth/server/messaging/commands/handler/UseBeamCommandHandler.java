package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UseBeamCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class UseBeamCommandHandler extends AbstractCommandHandler<UseBeamCommandPayload> {

    public UseBeamCommandHandler(GameService gameService,
                                 IPlayerSessionRegistry playerSessionRegistry) {
        super(gameService, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.USE_BEAM;
    }

    @Override
    public void handle(WebSocketSession session, UseBeamCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        var targetCoordinates = payload.getTargetCoordinates();
        gameService.useBeamBonus(targetCoordinates.getY(), targetCoordinates.getX(), player);
    }
}
