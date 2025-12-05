package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.abstractions.IMessageService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import labyrinth.server.messaging.mapper.GameMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
@RequiredArgsConstructor
public class DisconnectCommandHandler implements ICommandHandler<DisconnectCommandPayload> {

    private final IGame game;
    private final IPlayerSessionRegistry playerSessionRegistry;
    private final IMessageService messageService;
    private final GameMapper gameMapper;

    @Override
    public CommandType type() {
        return CommandType.DISCONNECT;
    }

    @Override
    public void handle(WebSocketSession session, DisconnectCommandPayload payload) throws Exception {
        var playerId = playerSessionRegistry.getPlayerId(session);

        if (playerId == null) {
            throw new ActionErrorException("Session is not connected to a player", ErrorCode.GENERAL); //TODO: error code?
        }

        var player = game.getPlayer(playerId);
        if(player == null) {
            throw new ActionErrorException("Player with ID " + playerId + " not found", ErrorCode.GENERAL); //TODO: error code?
        }

        game.leave(player);

        var gameState = gameMapper.toGameStateDto(game);
        messageService.broadcastToPlayers(gameState);
    }
}
