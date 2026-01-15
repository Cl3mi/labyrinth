package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.UseBeamCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class UseBeamCommandHandler extends AbstractCommandHandler<UseBeamCommandPayload> {

    private final MessageService messageService;
    private final GameMapper gameMapper;

    public UseBeamCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry,
                                 MessageService messageService,
                                 GameMapper gameMapper) {
        super(gameService, playerSessionRegistry);
        this.messageService = messageService;
        this.gameMapper = gameMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.USE_BEAM;
    }

    @Override
    public void handle(WebSocketSession session, UseBeamCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requirePlayerIsCurrent(player);

        var targetCoordinates = payload.getTargetCoordinates();
        gameService.useBeamBonus(targetCoordinates.getRow(), targetCoordinates.getColumn(), player);

        // Broadcast updated game state to all players
        gameService.withGameReadLock(game -> {
            if (game.getRoomState() == labyrinth.server.game.enums.RoomState.IN_GAME) {
                var gameState = gameMapper.toGameStateDto(game);
                messageService.broadcastToPlayers(gameState);
            }
            return null;
        });
    }
}
