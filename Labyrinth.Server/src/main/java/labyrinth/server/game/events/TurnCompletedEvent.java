package labyrinth.server.game.events;

import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;

public record TurnCompletedEvent(Game game, Player player) {}
