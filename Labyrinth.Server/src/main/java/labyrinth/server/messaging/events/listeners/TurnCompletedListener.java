package labyrinth.server.messaging.events.listeners;

import labyrinth.server.game.GameService;
import labyrinth.server.game.events.TurnCompletedEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.GameMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TurnCompletedListener {
    private static final Logger log = LoggerFactory.getLogger(TurnCompletedListener.class);

    private final MessageService messageService;
    private final GameMapper gameMapper;
    private final GameService gameService;

    @EventListener
    public void onTurnCompleted(TurnCompletedEvent event) {
        try {
            // Broadcast updated game state to all players
            var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
            messageService.broadcastToPlayers(gameState);
        } catch (Exception ex) {
            log.error("Error while handling TurnCompletedEvent", ex);
        }
    }
}
