package labyrinth.server.game.models;

import labyrinth.server.game.enums.BonusTypes;

import java.util.Optional;

/**
 * Represents the state of an active bonus in the game.
 * Uses the State pattern to eliminate nullable enum checks.
 */
public interface BonusState {

    /**
     * Checks if this represents an active bonus.
     *
     * @return true if a bonus is currently active, false otherwise
     */
    boolean isActive();

    /**
     * Gets the type of the active bonus, if any.
     *
     * @return Optional containing the bonus type if active, empty otherwise
     */
    Optional<BonusTypes> getBonusType();

    /**
     * Checks if the active bonus is of a specific type.
     *
     * @param type the bonus type to check
     * @return true if the active bonus matches the given type
     */
    boolean isOfType(BonusTypes type);

    /**
     * Consumes the bonus, transitioning to the no-bonus state.
     *
     * @return a new BonusState representing no active bonus
     */
    BonusState consume();
}
