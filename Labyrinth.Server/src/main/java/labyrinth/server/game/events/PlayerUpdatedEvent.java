package labyrinth.server.game.events;

import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.events.abstractions.IEvent;

public record PlayerUpdatedEvent(Player player) implements IEvent {
}
