package labyrinth.server.game.bonuses;

import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;

public interface IBonusEffect {
    boolean apply(Game game, Player player, Object... args);
}
