package labyrinth.client.models;

import lombok.Getter;
import lombok.Setter;

import java.util.Objects;

/**
 * Represents a position on the Labyrinth board with row and column coordinates.
 */


@Getter
public final class Position {

    private final int row;
    private final int column;

    /**
     * Creates a new position on the board.
     *
     * @param row    row index (0-based)
     * @param column column index (0-based)
     */
    public Position(int row, int column) {
        if (row < 0 || column < 0) {
            throw new IllegalArgumentException("Row and column must be non-negative");
        }
        this.row = row;
        this.column = column;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Position position)) return false;
        return row == position.row && column == position.column;
    }

    @Override
    public int hashCode() {
        return Objects.hash(row, column);
    }

    @Override
    public String toString() {
        return "Position{" + "row=" + row + ", column=" + column + '}';
    }
}
