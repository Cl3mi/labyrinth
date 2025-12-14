package labyrinth.client.abstractions;

import labyrinth.client.models.Game;
import labyrinth.contracts.models.Treasure;

import java.util.List;

public interface ITreasureCardFactory {
    List<Treasure> createCardsForGame(Game game);
}
