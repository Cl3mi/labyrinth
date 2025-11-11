package labyrinth.game.abstractions;

import labyrinth.game.models.Game;
import labyrinth.game.models.TreasureCard;

import java.util.List;

public interface ITreasureCardFactory {
    List<TreasureCard> createCardsForGame(Game game);
}
