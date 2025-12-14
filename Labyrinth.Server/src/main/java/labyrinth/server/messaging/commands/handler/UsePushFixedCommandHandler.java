package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.UsePushFixedCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.DirectionMapper;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class UsePushFixedCommandHandler extends AbstractCommandHandler<UsePushFixedCommandPayload> {

    private final DirectionMapper directionMapper;
    private final GameMapper gameMapper;
    private final MessageService messageService;

    public UsePushFixedCommandHandler(GameService gameService,
                                      PlayerSessionRegistry playerSessionRegistry,
                                      DirectionMapper directionMapper,
                                      GameMapper gameMapper,
                                      MessageService messageService) {
        super(gameService, playerSessionRegistry);
        this.directionMapper = directionMapper;
        this.gameMapper = gameMapper;
        this.messageService = messageService;
    }

    @Override
    public CommandType type() {
        return CommandType.USE_PUSH_FIXED;
    }

    @Override
    public void handle(WebSocketSession session, UsePushFixedCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        gameService.usePushFixedBonus(player);

        var direction = directionMapper.toModel(payload.getDirection());
        var shiftSuccessful = gameService.shift(payload.getRowOrColIndex(), direction, player);

        if (!shiftSuccessful) {
            throw new ActionErrorException("Cannot push tile with the specified parameters.", ErrorCode.INVALID_PUSH);
        }

        var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
        messageService.broadcastToPlayers(gameState);

    }
}
