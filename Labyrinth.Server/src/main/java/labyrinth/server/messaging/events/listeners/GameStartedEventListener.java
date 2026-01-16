package labyrinth.server.messaging.events.listeners;

import labyrinth.server.game.GameService;
import labyrinth.server.game.events.GameStartedEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.GameMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class GameStartedEventListener {
    private static final Logger log = LoggerFactory.getLogger(GameStartedEventListener.class);

    private final MessageService messageService;
    private final GameService gameService;
    private final GameMapper gameMapper;

    @EventListener
    public void onEvent(GameStartedEvent ignoreEvent) {
        try {
            var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
            messageService.broadcastToPlayers(gameState);

        } catch (Exception ex) {
            log.error("Error while handling GameStartedEvent", ex);
        }
    }
}
