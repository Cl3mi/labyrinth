package labyrinth.server.game.events;

import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.messaging.events.abstractions.IEvent;

public record NextTreasureCardEvent(
        Player player,
        TreasureCard treasureCard
) implements IEvent {
}
