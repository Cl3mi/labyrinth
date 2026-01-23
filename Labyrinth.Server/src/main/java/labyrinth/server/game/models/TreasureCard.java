package labyrinth.server.game.models;

import lombok.Getter;

import java.util.Objects;

/**
 * Represents a treasure card in the Labyrinth game.
 * Each card corresponds to a specific treasure that a player must collect.
 */
@Getter
public class TreasureCard {

    private final int id;
    private final String treasureName;
    private boolean collected;

    /**
     * Creates a new treasure card.
     *
     * @param id                unique identifier of the card
     * @param treasureName      display name of the treasure
     */
    public TreasureCard(int id, String treasureName) {
        this.id = id;
        this.treasureName = treasureName;
        this.collected = false;
    }

    /**
     * Marks the card as collected by the player.
     */
    public void collect() {
        this.collected = true;
    }

    @Override
    public String toString() {
        return "TreasureCard{" +
                "id='" + id + '\'' +
                ", treasureName='" + treasureName + '\'' +
                ", collected=" + collected +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TreasureCard that)) return false;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
