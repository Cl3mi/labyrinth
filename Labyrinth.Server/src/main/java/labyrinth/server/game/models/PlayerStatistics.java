package labyrinth.server.game.models;

import lombok.Getter;

@Getter
//TODO: use this class to track player statistics such as score, steps taken, tiles pushed, and treasures collected
public class PlayerStatistics {
    private int score;
    private int stepsTaken;
    private int tilesPushed;
    private int treasuresCollected;

    public void increaseScore(int amount) {
        this.score += amount;
    }

    public void increaseStepsTaken(int amount) {
        this.stepsTaken += amount;
    }

    public void increaseTilesPushed(int amount) {
        this.tilesPushed += amount;
    }

    public void increaseTreasuresCollected(int amount) {
        this.treasuresCollected += amount;
    }
}
