package labyrinth.server.game.models;

import labyrinth.server.game.enums.Achievement;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
//TODO: use this class to track player statistics such as score, steps taken, tiles pushed, and treasures collected
public class PlayerStatistics {
    private int score;
    private int stepsTaken;
    private int tilesPushed;
    private int treasuresCollected;

    private List<Achievement> collectedAchievements = new ArrayList<>();

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

    public void reset() {
        this.score = 0;
        this.stepsTaken = 0;
        this.tilesPushed = 0;
        this.treasuresCollected = 0;
    }

    public void collectAchievement(Achievement achievement){
        collectedAchievements.add(achievement);
    }

}
