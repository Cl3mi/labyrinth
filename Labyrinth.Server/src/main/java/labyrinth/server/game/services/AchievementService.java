package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IAchievementService;
import labyrinth.server.game.enums.Achievement;
import labyrinth.server.game.models.Player;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * Service responsible for checking and awarding achievements to players.
 * Encapsulates achievement criteria and checking logic in one place.
 */
public class AchievementService implements IAchievementService {

    /**
     * Awards achievements at the end of the game.
     * PUSHER goes to the player(s) with the most tiles pushed.
     * RUNNER goes to the player(s) with the most steps taken.
     *
     * @param players all players in the game
     * @return list of players who received achievements
     */
    public List<AchievementAward> awardEndGameAchievements(List<Player> players) {
        List<AchievementAward> awards = new ArrayList<>();

        if (players == null || players.isEmpty()) {
            return awards;
        }

        int maxTilesPushed = players.stream()
                .mapToInt(p -> p.getStatistics().getTilesPushed())
                .max()
                .orElse(0);

        if (maxTilesPushed > 0) {
            players.stream()
                    .filter(p -> p.getStatistics().getTilesPushed() == maxTilesPushed)
                    .forEach(player -> {
                        if (!player.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER)) {
                            player.getStatistics().collectAchievement(Achievement.PUSHER);
                            awards.add(new AchievementAward(player, Achievement.PUSHER));
                        }
                    });
        }

        int maxStepsTaken = players.stream()
                .mapToInt(p -> p.getStatistics().getStepsTaken())
                .max()
                .orElse(0);

        if (maxStepsTaken > 0) {
            players.stream()
                    .filter(p -> p.getStatistics().getStepsTaken() == maxStepsTaken)
                    .forEach(player -> {
                        if (!player.getStatistics().getCollectedAchievements().contains(Achievement.RUNNER)) {
                            player.getStatistics().collectAchievement(Achievement.RUNNER);
                            awards.add(new AchievementAward(player, Achievement.RUNNER));
                        }
                    });
        }

        return awards;
    }


    /**
     * Record representing an achievement award.
     */
    public record AchievementAward(Player player, Achievement achievement) {
    }
}
