package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Player;
import labyrinth.server.game.services.AchievementService;

import java.util.List;

/**
 * Interface for checking and awarding achievements to players.
 * Encapsulates achievement criteria and checking logic.
 *
 * <p>This interface enables dependency injection and testing by decoupling
 * the Game class from the concrete AchievementService implementation.</p>
 */
public interface IAchievementService {

    /**
     * Awards achievements at the end of the game based on player statistics.
     * Examples: PUSHER (most tiles pushed), RUNNER (most steps taken)
     *
     * @param players all players in the game
     * @return list of achievement awards
     */
    List<AchievementService.AchievementAward> awardEndGameAchievements(List<Player> players);
}
