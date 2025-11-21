package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.TreasureCard;

import java.util.List;

public interface ITreasureCardFactory {
    List<TreasureCard> createCardsForGame(Game game);
}
