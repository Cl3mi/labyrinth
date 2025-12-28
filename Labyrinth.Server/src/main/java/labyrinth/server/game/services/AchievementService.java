package labyrinth.server.game.services;

import labyrinth.server.game.enums.Achievement;
import labyrinth.server.game.models.Player;

import java.util.Optional;

/**
 * Service responsible for checking and awarding achievements to players.
 * Encapsulates achievement criteria and checking logic in one place.
 */
public class AchievementService {

    private static final int PUSHER_TILES_REQUIRED = 20;
    private static final int RUNNER_STEPS_REQUIRED = 200;

    /**
     * Checks if the player has earned the PUSHER achievement.
     * PUSHER is awarded when a player has pushed tiles 20 or more times.
     *
     * @param player the player to check
     * @return Optional containing PUSHER achievement if newly earned, empty otherwise
     */
    public Optional<Achievement> checkPusherAchievement(Player player) {
        var statistics = player.getStatistics();
        boolean alreadyHas = statistics.getCollectedAchievements().contains(Achievement.PUSHER);
        boolean meetsRequirement = statistics.getTilesPushed() >= PUSHER_TILES_REQUIRED;

        if (!alreadyHas && meetsRequirement) {
            statistics.collectAchievement(Achievement.PUSHER);
            return Optional.of(Achievement.PUSHER);
        }
        return Optional.empty();
    }

    /**
     * Checks if the player has earned the RUNNER achievement.
     * RUNNER is awarded when a player has taken 200 or more steps.
     *
     * @param player the player to check
     * @return Optional containing RUNNER achievement if newly earned, empty otherwise
     */
    public Optional<Achievement> checkRunnerAchievement(Player player) {
        var statistics = player.getStatistics();
        boolean alreadyHas = statistics.getCollectedAchievements().contains(Achievement.RUNNER);
        boolean meetsRequirement = statistics.getStepsTaken() >= RUNNER_STEPS_REQUIRED;

        if (!alreadyHas && meetsRequirement) {
            statistics.collectAchievement(Achievement.RUNNER);
            return Optional.of(Achievement.RUNNER);
        }
        return Optional.empty();
    }
}
