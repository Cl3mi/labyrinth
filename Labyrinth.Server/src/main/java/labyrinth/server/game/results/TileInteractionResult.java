package labyrinth.server.game.results;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.TreasureCard;

/**
 * Result of a player stepping onto a tile. Contains information about
 * what was collected without actually applying any side effects.
 * <p>
 * This allows the tile to remain a pure data object while letting
 * a service manage the actual effects.
 */
public record TileInteractionResult(
        TreasureCard collectedTreasure,
        BonusTypes collectedBonus,
        boolean wasCurrentTarget) {
    /**
     * Creates an empty result indicating nothing was collected.
     */
    public static TileInteractionResult empty() {
        return new TileInteractionResult(null, null, false);
    }

    /**
     * Returns true if any item was collected.
     */
    public boolean hasCollections() {
        return collectedTreasure != null || collectedBonus != null;
    }
}
