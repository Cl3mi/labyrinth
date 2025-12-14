package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.UseSwapCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;

@Component
public class UseSwapCommandHandler extends AbstractCommandHandler<UseSwapCommandPayload> {

    public UseSwapCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry) {
        super(gameService, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.USE_SWAP;
    }

    @Override
    public void handle(WebSocketSession session, UseSwapCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        var playerToSwap = gameService.getPlayer(UUID.fromString(payload.getTargetPlayerId()));

        if (playerToSwap == null) {
            throw new ActionErrorException("Player with ID " + payload.getTargetPlayerId() + " not found", ErrorCode.PLAYER_NOT_FOUND);
        }

        gameService.useSwapBonus(player, playerToSwap);
    }
}
