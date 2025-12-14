package labyrinth.server.game.events;

import labyrinth.server.game.enums.Achievement;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.events.abstractions.IEvent;

public record AchievementUnlockedEvent(
        Player player,
        Achievement achievement
) implements IEvent {
}
