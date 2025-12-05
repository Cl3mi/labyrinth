package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Board;

public interface IBoardFactory {
    Board createBoard(int width, int height);
}
