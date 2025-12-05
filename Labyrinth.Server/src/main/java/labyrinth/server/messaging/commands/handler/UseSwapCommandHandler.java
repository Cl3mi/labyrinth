package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.UseSwapCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class UseSwapCommandHandler implements ICommandHandler<UseSwapCommandPayload> {

    private final IGame game;
    private final PlayerSessionRegistry playerSessionRegistry;

    @Override
    public CommandType type() {
        return CommandType.USE_SWAP;
    }

    @Override
    public void handle(WebSocketSession session, UseSwapCommandPayload payload) throws Exception {
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

        var playerToSwap = game.getPlayer(UUID.fromString(payload.getTargetPlayerId()));

        if(playerToSwap == null) {
            throw new ActionErrorException("Player with ID " + payload.getTargetPlayerId() + " not found", ErrorCode.GENERAL); //TODO: error code?
        }

        game.useSwapBonus(player, playerToSwap);
    }
}
