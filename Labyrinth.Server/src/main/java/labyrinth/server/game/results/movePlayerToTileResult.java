package labyrinth.server.game.results;

public record movePlayerToTileResult(
        boolean moveSuccess,
        int distanceMoved,
        boolean treasureCollected,
        boolean gameOver
) {

}
