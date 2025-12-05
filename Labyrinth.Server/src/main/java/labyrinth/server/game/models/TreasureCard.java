package labyrinth.server.game.models;

import java.util.Objects;

/**
 * Represents a treasure card in the Labyrinth game.
 * Each card corresponds to a specific treasure that a player must collect.
 */
public class TreasureCard {

    private final int id;
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
    public TreasureCard(int id, String treasureName, String treasureImagePath) {
        this.id = id;
        this.treasureName = treasureName;
        this.treasureImagePath = treasureImagePath;
        this.collected = false;
    }

    public int getId() {
        return id;
    }

    public String getTreasureName() {
        return treasureName;
    }

    public String getTreasureImagePath() {
        return treasureImagePath;
    }

    public boolean isCollected() {
        return collected;
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
