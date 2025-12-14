package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.RotateTileCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class RotateTileCommandHandler extends AbstractCommandHandler<RotateTileCommandPayload> {

    private final GameMapper gameMapper;
    private final MessageService messageService;

    protected RotateTileCommandHandler(GameService gameService,
                                       PlayerSessionRegistry playerSessionRegistry,
                                       GameMapper gameMapper,
                                       MessageService messageService) {
        super(gameService, playerSessionRegistry);

        this.gameMapper = gameMapper;
        this.messageService = messageService;
    }

    @Override
    public CommandType type() {
        return CommandType.ROTATE_TILE;
    }

    @Override
    public void handle(WebSocketSession session, RotateTileCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        gameService.rotateExtraTileClockwise(player);

        var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
        messageService.broadcastToPlayers(gameState);
    }
}
