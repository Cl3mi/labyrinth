package labyrinth.server.game.results;

import labyrinth.server.game.models.Player;

public record LeaveResult(
        boolean playerRemoved,
        Player newAdmin,
        boolean shouldShutdown
) {

}
