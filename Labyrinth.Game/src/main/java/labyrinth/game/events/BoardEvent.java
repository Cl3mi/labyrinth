package labyrinth.game.events;

import labyrinth.game.enums.BoardEventType;

public record BoardEvent(BoardEventType type, int index) {}