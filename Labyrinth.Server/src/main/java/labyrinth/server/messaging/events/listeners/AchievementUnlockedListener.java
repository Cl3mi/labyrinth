package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.AchievementUnlockedEventPayload;
import labyrinth.contracts.models.EventType;
import labyrinth.server.game.events.AchievementUnlockedEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.AchievementTypeMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;


@Component
@RequiredArgsConstructor
public class AchievementUnlockedListener {

    private static final Logger log = LoggerFactory.getLogger(AchievementUnlockedListener.class);

    private final MessageService messageService;
    private final AchievementTypeMapper achievementTypeMapper;


    @EventListener
    public void onEvent(AchievementUnlockedEvent event) {
        try {
            var playerId = event.player().getId();

            var payload = new AchievementUnlockedEventPayload();
            payload.setType(EventType.ACHIEVEMENT_UNLOCKED);
            payload.setAchievement(achievementTypeMapper.toDto(event.achievement()));
            payload.setPlayerId(playerId.toString());

            messageService.broadcastToPlayers(payload);
        } catch (Exception ex) {
            log.error("Error while handling AchievementUnlockedEvent", ex);
        }
    }
}
