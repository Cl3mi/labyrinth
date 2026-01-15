package labyrinth.client.ai;

import labyrinth.contracts.models.Direction;

/**
 * Represents a shift operation (push) on the board.
 */
public record ShiftOperation(Direction direction, int index, boolean isRow) {

    /**
     * Creates a row shift operation.
     */
    public static ShiftOperation row(int index, Direction direction) {
        return new ShiftOperation(direction, index, true);
    }

    /**
     * Creates a column shift operation.
     */
    public static ShiftOperation column(int index, Direction direction) {
        return new ShiftOperation(direction, index, false);
    }
}
