package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Player;

public interface IGame {
    Player join(Player player);
    void leave(Player player);
}
