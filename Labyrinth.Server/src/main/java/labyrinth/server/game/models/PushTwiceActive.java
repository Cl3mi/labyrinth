package labyrinth.server.game.models;

import labyrinth.server.game.enums.BonusTypes;
import lombok.Getter;

import java.util.Optional;

/**
 * Represents the PUSH_TWICE bonus state with remaining push count.
 * This state tracks how many pushes remain (starts at 2, decrements on each consume).
 */
@Getter
public final class PushTwiceActive implements BonusState {

    /**
     * -- GETTER --
     *  Gets the number of remaining pushes.
     */
    private final int remainingPushes;

    /**
     * Creates a new PushTwiceActive state with 2 remaining pushes.
     */
    public PushTwiceActive() {
        this.remainingPushes = 2;
    }

    /**
     * Creates a PushTwiceActive state with a specific number of remaining pushes.
     *
     * @param remainingPushes the number of pushes remaining
     */
    private PushTwiceActive(int remainingPushes) {
        this.remainingPushes = remainingPushes;
    }

    @Override
    public boolean isActive() {
        return true;
    }

    @Override
    public Optional<BonusTypes> getBonusType() {
        return Optional.of(BonusTypes.PUSH_TWICE);
    }

    @Override
    public boolean isOfType(BonusTypes type) {
        return type == BonusTypes.PUSH_TWICE;
    }

    @Override
    public BonusState consume() {
        if (remainingPushes <= 1) {
            return NoBonusActive.getInstance();
        }
        return new PushTwiceActive(remainingPushes - 1);
    }

    @Override
    public String toString() {
        return "PushTwiceActive{remainingPushes=" + remainingPushes + '}';
    }
}
