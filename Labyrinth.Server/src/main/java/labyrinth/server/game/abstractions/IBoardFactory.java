package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;

public interface IBoardFactory {

    /**
     * Generates a random board for a given game
     *
     * @param game  the game with the according settings
     * @return generated Board
     */
    Board createBoardForGame(Game game);
}
