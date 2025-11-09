package labyrinth.client.abstractions;

import labyrinth.client.models.Game;
import labyrinth.client.models.TreasureCard;

import java.util.List;

public interface ITreasureCardFactory {
    List<TreasureCard> createCardsForGame(Game game);
}
