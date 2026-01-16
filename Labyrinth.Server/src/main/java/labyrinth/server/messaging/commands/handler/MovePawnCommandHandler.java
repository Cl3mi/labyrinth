package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.MovePawnCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.game.events.AchievementUnlockedEvent;
import labyrinth.server.game.events.GameOverEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.events.EventPublisher;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class MovePawnCommandHandler extends AbstractCommandHandler<MovePawnCommandPayload> {

    private final MessageService messageService;
    private final GameMapper gameMapper;
    private final EventPublisher eventPublisher;

    public MovePawnCommandHandler(GameService gameService,
                                  PlayerSessionRegistry playerSessionRegistry,
                                  MessageService messageService,
                                  GameMapper gameMapper,
                                  EventPublisher eventPublisher) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.gameMapper = gameMapper;
        this.eventPublisher = eventPublisher;
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
        var result = gameService.movePlayerToTile(coordinates.getRow(), coordinates.getColumn(), player);

        if (!result.moveSuccess()) {
            throw new ActionErrorException("Cannot move pawn to the specified coordinates.", ErrorCode.INVALID_MOVE);
        }

        gameService.withGameReadLock(game -> {
            var gameState = gameMapper.toGameStateDto(game);
            messageService.broadcastToPlayers(gameState);
            return null;
        });

        if (result.gameOver()) {
            for (var award : gameService.getEndGameAchievements()) {
                var achievementEvent = new AchievementUnlockedEvent(award.player(), award.achievement());
                eventPublisher.publishAsync(achievementEvent);
            }

            var gameOverEvent = new GameOverEvent(gameService.getPlayers());
            eventPublisher.publishAsync(gameOverEvent);
        }
    }
}
