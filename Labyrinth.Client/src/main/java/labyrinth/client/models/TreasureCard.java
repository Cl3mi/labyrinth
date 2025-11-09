package labyrinth.client.models;

import lombok.Getter;
import lombok.Setter;

import java.util.Objects;

/**
 * Represents a treasure card in the Labyrinth game.
 * Each card corresponds to a specific treasure that a player must collect.
 */
@Getter
@Setter
public class TreasureCard {

    private final String id;
    private final String treasureName;
    private final String treasureImagePath;
    private boolean collected;

    /**
     * Creates a new treasure card.
     *
     * @param id                unique identifier of the card
     * @param treasureName      display name of the treasure
     * @param treasureImagePath path to the image resource
     */
    public TreasureCard(String id, String treasureName, String treasureImagePath) {
        this.id = id;
        this.treasureName = treasureName;
        this.treasureImagePath = treasureImagePath;
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
        if (!(o instanceof TreasureCard)) return false;
        TreasureCard that = (TreasureCard) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
