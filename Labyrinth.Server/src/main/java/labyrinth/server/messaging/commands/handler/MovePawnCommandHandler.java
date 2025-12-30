package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.MovePawnCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class MovePawnCommandHandler extends AbstractCommandHandler<MovePawnCommandPayload> {

    private final MessageService messageService;
    private final GameMapper gameMapper;

    public MovePawnCommandHandler(GameService gameService,
                                  PlayerSessionRegistry playerSessionRegistry,
                                  MessageService messageService,
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
        var moveSuccessful = gameService.movePlayerToTile(coordinates.getRow(), coordinates.getColumn(), player);

        if (!moveSuccessful) {
            throw new ActionErrorException("Cannot move pawn to the specified coordinates.", ErrorCode.INVALID_MOVE);
        }

        // Only broadcast game state if game is still in progress
        // (if player won, GameOverEvent listener handles the broadcast)
        gameService.withGameReadLock(game -> {
            if (game.getRoomState() == labyrinth.server.game.enums.RoomState.IN_GAME) {
                var gameState = gameMapper.toGameStateDto(game);
                messageService.broadcastToPlayers(gameState);
            }
            return null;
        });
    }
}
