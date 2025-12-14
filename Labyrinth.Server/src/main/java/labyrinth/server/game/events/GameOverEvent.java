package labyrinth.server.game.events;

import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.events.abstractions.IEvent;

import java.util.List;

public record GameOverEvent(List<Player> players) implements IEvent {
}
