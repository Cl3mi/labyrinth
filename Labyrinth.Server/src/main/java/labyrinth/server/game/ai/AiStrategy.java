package labyrinth.server.game.ai;

import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;

public interface AiStrategy {
    void performTurn(Game game, Player player);
}
