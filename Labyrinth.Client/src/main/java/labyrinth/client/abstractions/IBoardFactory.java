package labyrinth.client.abstractions;

import labyrinth.client.models.Board;
import labyrinth.client.models.Game;

public interface IBoardFactory {

    /**
     * Generates a random board for a given game
     *
     * @param game  the game with the according settings
     * @return generated Board
     */
    Board createBoardForGame(Game game);
}
