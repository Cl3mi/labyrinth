package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.PushTileCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.abstractions.IMessageService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import labyrinth.server.messaging.mapper.DirectionMapper;
import labyrinth.server.messaging.mapper.GameMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.Arrays;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class PushTileCommandHandler implements ICommandHandler<PushTileCommandPayload> {

    private final IGame game;
    private final IPlayerSessionRegistry playerSessionRegistry;
    private final IMessageService messageService;
    private final GameMapper gameMapper;
    private final DirectionMapper directionMapper;

    @Override
    public CommandType type() {
        return CommandType.PUSH_TILE;
    }

    @Override
    public void handle(WebSocketSession session, PushTileCommandPayload payload) throws Exception {
        var playerId = playerSessionRegistry.getPlayerId(session);
        var player = game.getPlayer(playerId);

        //TODO: error handling

        var direction = directionMapper.toModel(payload.getDirection());
        var entrances = Arrays.stream(payload.getTileEntrances())
                .map(directionMapper::toModel)
                .collect(Collectors.toSet());


        try {
            game.shift(payload.getRowOrColIndex(), direction, entrances, player);

            var gameState = gameMapper.toGameStateDto(game);
            messageService.broadcastToPlayers(gameState);
        } catch (Exception ex) {
            throw new ActionErrorException("Cannot push tile with the specified parameters.", ErrorCode.GENERAL); //TODO: error code
        }
    }
}
