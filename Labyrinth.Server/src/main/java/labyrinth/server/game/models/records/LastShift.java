package labyrinth.server.game.models.records;

import labyrinth.server.game.enums.Direction;

/**
 * Tracks the last shift operation performed on the board.
 * Used to prevent players from immediately reversing the previous shift.
 *
 * @param index     the row or column index that was shifted
 * @param direction the direction of the shift
 */
public record LastShift(int index, Direction direction) {

    /**
     * Checks if a proposed shift would reverse this shift.
     * A reverse shift is one that uses the same index but the opposite direction.
     *
     * @param proposedIndex     the index of the proposed shift
     * @param proposedDirection the direction of the proposed shift
     * @return true if the proposed shift would reverse this shift
     */
    public boolean isReversedBy(int proposedIndex, Direction proposedDirection) {
        return this.index == proposedIndex && this.direction.opposite() == proposedDirection;
    }
}
