package labyrinth.game.abstractions;

import labyrinth.game.models.Board;
import labyrinth.game.models.Game;

public interface IBoardFactory {

    /**
     * Generates a random board for a given game
     *
     * @param game  the game with the according settings
     * @return generated Board
     */
    Board createBoardForGame(Game game);
}
