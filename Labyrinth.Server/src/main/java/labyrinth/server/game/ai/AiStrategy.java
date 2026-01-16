package labyrinth.server.game.ai;

import labyrinth.server.game.models.Player;

public interface AiStrategy {
    void performTurn(Player player);
}