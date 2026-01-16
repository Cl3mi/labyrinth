package labyrinth.server.messaging.events.listeners;

import labyrinth.server.game.GameService;
import labyrinth.server.game.events.PlayerMovedEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.GameMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PlayerMovedEventListener {
    private static final Logger log = LoggerFactory.getLogger(PlayerMovedEventListener.class);

    private final MessageService messageService;
    private final GameService gameService;
    private final GameMapper gameMapper;

    @EventListener
    public void onEvent(PlayerMovedEvent ignoredEvent) {
        try {

            var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
            messageService.broadcastToPlayers(gameState);

            log.info("GameStateUpdate event send after PlayerMovedEvent");
        } catch (Exception ex) {
            log.error("Error while handling PlayerMovedEvent", ex);
        }
    }
}
