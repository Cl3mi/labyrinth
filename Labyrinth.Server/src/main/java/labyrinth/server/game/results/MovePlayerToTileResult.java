package labyrinth.server.game.results;

public record MovePlayerToTileResult(
        boolean moveSuccess,
        int distanceMoved,
        boolean treasureCollected,
        boolean gameOver,
        boolean runnerAchieved
) {

}

