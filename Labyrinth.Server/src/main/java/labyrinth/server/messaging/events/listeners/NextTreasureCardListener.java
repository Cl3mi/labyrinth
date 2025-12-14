package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.NextTreasureCardEventPayload;
import labyrinth.server.game.events.NextTreasureCardEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.TreasureMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class NextTreasureCardListener {
    private static final Logger log = LoggerFactory.getLogger(NextTreasureCardListener.class);

    private final MessageService messageService;
    private final TreasureMapper treasureMapper;

    @EventListener
    public void onEvent(NextTreasureCardEvent event) {
        try {
            var playerId = event.player().getId();

            var payload = new NextTreasureCardEventPayload();
            payload.setType(EventType.NEXT_TREASURE);
            payload.setTreasure(treasureMapper.toDto(event.treasureCard()));

            messageService.sendToPlayer(playerId, payload);
        } catch (Exception ex) {
            log.error("Error while handling NextTreasureCardEvent", ex);
        }
    }
}
