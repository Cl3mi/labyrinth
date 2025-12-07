package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.MovePawnCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.abstractions.IMessageService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class MovePawnCommandHandler extends AbstractCommandHandler<MovePawnCommandPayload> {

    private final IMessageService messageService;
    private final GameMapper gameMapper;

    public MovePawnCommandHandler(GameService gameService,
                                  IPlayerSessionRegistry playerSessionRegistry,
                                  IMessageService messageService,
                                  GameMapper gameMapper) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.gameMapper = gameMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.MOVE_PAWN;
    }

    @Override
    public void handle(WebSocketSession session, MovePawnCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);

        requirePlayerIsCurrent(player);

        var coordinates = payload.getTargetCoordinates();
        var moveSuccessful = gameService.movePlayerToTile(coordinates.getY(), coordinates.getX(), player);

        if (!moveSuccessful) {
            throw new ActionErrorException("Cannot move pawn to the specified coordinates.", ErrorCode.INVALID_MOVE);
        }

        var gameState = gameMapper.toGameStateDto(gameService.getGame());
        messageService.broadcastToPlayers(gameState);
    }
}
