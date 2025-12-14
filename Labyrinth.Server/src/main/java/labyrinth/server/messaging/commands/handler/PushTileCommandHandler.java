package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.PushTileCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.DirectionMapper;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class PushTileCommandHandler extends AbstractCommandHandler<PushTileCommandPayload> {

    private final MessageService messageService;
    private final DirectionMapper directionMapper;
    private final GameMapper gameMapper;

    public PushTileCommandHandler(GameService gameService,
                                  PlayerSessionRegistry playerSessionRegistry,
                                  MessageService messageService,
                                  DirectionMapper directionMapper,
                                  GameMapper gameMapper) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.directionMapper = directionMapper;
        this.gameMapper = gameMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.PUSH_TILE;
    }

    @Override
    public void handle(WebSocketSession session, PushTileCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        var direction = directionMapper.toModel(payload.getDirection());
        var shiftSuccessful = gameService.shift(payload.getRowOrColIndex(), direction, player);

        if (!shiftSuccessful) {
            throw new ActionErrorException("Cannot push tile with the specified parameters.", ErrorCode.INVALID_PUSH);
        }

        var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
        messageService.broadcastToPlayers(gameState);
    }
}
