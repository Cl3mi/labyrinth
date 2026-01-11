package labyrinth.server.game.abstractions;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;

/**
 * Interface for managing bonus activation and validation.
 * Encapsulates bonus effect application and turn-based usage tracking.
 *
 * <p>This interface enables dependency injection and testing by decoupling
 * the Game class from bonus management logic.</p>
 */
public interface IBonusManager {

    /**
     * Applies a bonus effect for the given player.
     * Validates that the bonus can be used (not already used this turn, player has bonus).
     *
     * @param type the type of bonus to use
     * @param game the game instance (for accessing game state)
     * @param player the player using the bonus
     * @param args additional arguments required by the specific bonus effect
     * @return true if the bonus was successfully applied, false otherwise
     * @throws IllegalArgumentException if no strategy exists for the bonus type
     * @throws IllegalStateException if a bonus has already been used this turn
     */
    boolean useBonus(BonusTypes type, Game game, Player player, Object... args);

    /**
     * Checks if the bonus manager has a strategy for the given bonus type.
     *
     * @param type the bonus type to check
     * @return true if a strategy exists for this bonus type
     */
    boolean hasStrategy(BonusTypes type);
}
