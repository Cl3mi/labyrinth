package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.PlayerUpdatedEventPayload;
import labyrinth.server.game.events.PlayerUpdatedEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PlayerUpdatedEventListener {
    private static final Logger log = LoggerFactory.getLogger(PlayerUpdatedEventListener.class);

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;

    @EventListener
    public void onEvent(PlayerUpdatedEvent event) {
        try {
            var playerUpdatedEventPayload = new PlayerUpdatedEventPayload();
            playerUpdatedEventPayload.setType(EventType.PLAYER_UPDATED);
            playerUpdatedEventPayload.setPlayer(playerInfoMapper.toDto(event.player()));

            messageService.broadcastToPlayers(playerUpdatedEventPayload);

            log.info("PlayerUpdatedEventPayload sent after PlayerUpdatedEvent for player {}", event.player().getId());
        } catch (Exception ex) {
            log.error("Error while processing PlayerUpdatedEvent", ex);
        }
    }
}

