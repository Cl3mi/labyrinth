package labyrinth.game.abstractions;

import labyrinth.game.events.BoardEvent;

public interface IBoardEventListener {
    void onBoardEvent(BoardEvent event);
}
