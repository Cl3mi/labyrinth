package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.ErrorCode;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;


public abstract class AbstractCommandHandler<T> implements ICommandHandler<T> {

    protected final IGame game;
    protected final IPlayerSessionRegistry playerSessionRegistry;

    protected AbstractCommandHandler(IGame game, IPlayerSessionRegistry playerSessionRegistry) {
        if (playerSessionRegistry == null) throw new IllegalArgumentException("playerSessionRegistry must not be null");
        this.game = game;
        this.playerSessionRegistry = playerSessionRegistry;
    }


    protected UUID requireRegisteredSession(WebSocketSession session) throws ActionErrorException {
        if (!playerSessionRegistry.isSessionRegistered(session)) {
            throw new ActionErrorException("Session is not connected to a player", ErrorCode.GENERAL);
        }

        return playerSessionRegistry.getPlayerId(session);
    }

    protected Player requireExistingPlayer(WebSocketSession session) throws ActionErrorException {
        var playerId = requireRegisteredSession(session);
        var player = game.getPlayer(playerId);
        if (player == null) {
            throw new ActionErrorException("Player with ID " + playerId + " not found", ErrorCode.GENERAL);
        }

        return player;
    }

    protected void requirePlayerIsCurrent(Player player) throws ActionErrorException {
        var current = game.getCurrentPlayer();
        if (current == null || current != player) {
            throw new ActionErrorException("It's not your turn.", ErrorCode.NOT_YOUR_TURN);
        }
    }

    protected void requireAdmin(Player player) throws ActionErrorException {
        if (player == null || !player.isAdmin()) {
            throw new ActionErrorException("Only admin players can perform this action", ErrorCode.NOT_ADMIN);
        }
    }
}
