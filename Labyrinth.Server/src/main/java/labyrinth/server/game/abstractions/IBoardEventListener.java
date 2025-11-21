package labyrinth.server.game.abstractions;

import labyrinth.server.game.events.BoardEvent;

public interface IBoardEventListener {
    void onBoardEvent(BoardEvent event);
}
