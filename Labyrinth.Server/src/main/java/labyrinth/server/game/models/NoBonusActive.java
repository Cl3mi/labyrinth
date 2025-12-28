package labyrinth.server.game.models;

import labyrinth.server.game.enums.BonusTypes;

import java.util.Optional;

/**
 * Represents the state when no bonus is currently active.
 * Singleton implementation of the Null Object pattern.
 */
public final class NoBonusActive implements BonusState {

    private static final NoBonusActive INSTANCE = new NoBonusActive();

    private NoBonusActive() {
    }

    /**
     * Gets the singleton instance representing no active bonus.
     *
     * @return the singleton NoBonusActive instance
     */
    public static NoBonusActive getInstance() {
        return INSTANCE;
    }

    @Override
    public boolean isActive() {
        return false;
    }

    @Override
    public Optional<BonusTypes> getBonusType() {
        return Optional.empty();
    }

    @Override
    public boolean isOfType(BonusTypes type) {
        return false;
    }

    @Override
    public BonusState consume() {
        return this; // Already inactive, nothing to consume
    }

    @Override
    public String toString() {
        return "NoBonusActive";
    }
}
