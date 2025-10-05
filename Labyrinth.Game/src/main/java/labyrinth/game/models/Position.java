package labyrinth.game.models;

import java.util.Objects;

/**
 * Represents a position on the Labyrinth board with row and column coordinates.
 */
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

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    /**
     * Creates a new Position offset by the given row and column deltas.
     *
     * @param deltaRow    change in row
     * @param deltaColumn change in column
     * @return new Position after applying the offset
     */
    public Position offset(int deltaRow, int deltaColumn) {
        return new Position(this.row + deltaRow, this.column + deltaColumn);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Position)) return false;
        Position position = (Position) o;
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
