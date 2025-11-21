package labyrinth.server.game.events;

import labyrinth.server.game.enums.BoardEventType;

public record BoardEvent(BoardEventType type, int index) {}