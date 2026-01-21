package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.PushTileCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.DirectionMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class PushTileCommandHandler extends AbstractCommandHandler<PushTileCommandPayload> {

    private final DirectionMapper directionMapper;

    public PushTileCommandHandler(GameService gameService,
                                  PlayerSessionRegistry playerSessionRegistry,
                                  DirectionMapper directionMapper) {

        super(gameService, playerSessionRegistry);
        this.directionMapper = directionMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.PUSH_TILE;
    }

    @Override
    public void handle(WebSocketSession session, PushTileCommandPayload payload) throws ActionErrorException {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        var direction = directionMapper.toModel(payload.getDirection());
        var shiftSuccessful = gameService.shift(payload.getRowOrColIndex(), direction, player);

        if (!shiftSuccessful) {
            throw new ActionErrorException("Cannot push tile with the specified parameters.", ErrorCode.INVALID_PUSH);
        }
    }
}
