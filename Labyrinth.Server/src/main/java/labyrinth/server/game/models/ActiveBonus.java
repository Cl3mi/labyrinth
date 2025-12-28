package labyrinth.server.game.models;

import labyrinth.server.game.enums.BonusTypes;

import java.util.Objects;
import java.util.Optional;

/**
 * Represents an active bonus in the game.
 * Encapsulates the bonus type and provides type-safe operations.
 */
public final class ActiveBonus implements BonusState {

    private final BonusTypes bonusType;

    /**
     * Creates a new active bonus state.
     *
     * @param bonusType the type of bonus that is active
     * @throws NullPointerException if bonusType is null
     */
    public ActiveBonus(BonusTypes bonusType) {
        this.bonusType = Objects.requireNonNull(bonusType, "bonusType cannot be null");
    }

    @Override
    public boolean isActive() {
        return true;
    }

    @Override
    public Optional<BonusTypes> getBonusType() {
        return Optional.of(bonusType);
    }

    @Override
    public boolean isOfType(BonusTypes type) {
        return bonusType == type;
    }

    @Override
    public BonusState consume() {
        return NoBonusActive.getInstance();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ActiveBonus that = (ActiveBonus) o;
        return bonusType == that.bonusType;
    }

    @Override
    public int hashCode() {
        return Objects.hash(bonusType);
    }

    @Override
    public String toString() {
        return "ActiveBonus{" + bonusType + '}';
    }
}
