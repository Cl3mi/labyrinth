package labyrinth.game.models;

import labyrinth.game.enums.*;

import java.util.HashSet;
import java.util.Set;

/**
 * Represents the game board for the Labyrinth game.
 * Contains a 2D grid of tiles and a graph representing tile connectivity.
 */
public class Board {

    private final int width;
    private final int height;
    private final Tile[][] tiles;
    private final Graph graph;

    /**
     * Creates a board with the given dimensions and tiles.
     *
     * @param width  number of columns
     * @param height number of rows
     * @param tiles  2D array of tiles (height x width)
     */
    public Board(int width, int height, Tile[][] tiles) {
        if (tiles.length != height || tiles[0].length != width) {
            throw new IllegalArgumentException("Tile array dimensions must match width and height");
        }
        this.width = width;
        this.height = height;
        this.tiles = tiles;
        this.graph = new Graph();
        initializeGraph();
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public Tile[][] getTiles() {
        return tiles;
    }

    public Graph getGraph() {
        return graph;
    }

    /**
     * Initializes the graph by connecting adjacent tiles based on entrances.
     */
    private void initializeGraph() {
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile = tiles[row][col];
                graph.addTile(tile);

                // Connect with neighbors if entrances match
                if (row > 0) {
                    Tile upNeighbor = tiles[row - 1][col];
                    graph.connect(tile, upNeighbor, Direction.UP);
                }
                if (row < height - 1) {
                    Tile downNeighbor = tiles[row + 1][col];
                    graph.connect(tile, downNeighbor, Direction.DOWN);
                }
                if (col > 0) {
                    Tile leftNeighbor = tiles[row][col - 1];
                    graph.connect(tile, leftNeighbor, Direction.LEFT);
                }
                if (col < width - 1) {
                    Tile rightNeighbor = tiles[row][col + 1];
                    graph.connect(tile, rightNeighbor, Direction.RIGHT);
                }
            }
        }
    }

    /**
     * Shifts a row in the specified direction. Tiles wrap around.
     *
     * @param rowIndex the row to shift
     * @param direction LEFT or RIGHT
     */
    public void shiftRow(int rowIndex, Direction direction) {
        if (rowIndex < 0 || rowIndex >= height) {
            throw new IllegalArgumentException("Invalid row index");
        }
        if (direction != Direction.LEFT && direction != Direction.RIGHT) {
            throw new IllegalArgumentException("Row can only be shifted LEFT or RIGHT");
        }

        Tile[] row = tiles[rowIndex];
        if (direction == Direction.RIGHT) {
            Tile last = row[width - 1];
            System.arraycopy(row, 0, row, 1, width - 1);
            row[0] = last;
        } else {
            Tile first = row[0];
            System.arraycopy(row, 1, row, 0, width - 1);
            row[width - 1] = first;
        }

        initializeGraph();
    }

    /**
     * Shifts a column in the specified direction. Tiles wrap around.
     *
     * @param columnIndex the column to shift
     * @param direction   UP or DOWN
     */
    public void shiftColumn(int columnIndex, Direction direction) {
        if (columnIndex < 0 || columnIndex >= width) {
            throw new IllegalArgumentException("Invalid column index");
        }
        if (direction != Direction.UP && direction != Direction.DOWN) {
            throw new IllegalArgumentException("Column can only be shifted UP or DOWN");
        }

        if (direction == Direction.DOWN) {
            Tile last = tiles[height - 1][columnIndex];
            for (int row = height - 1; row > 0; row--) {
                tiles[row][columnIndex] = tiles[row - 1][columnIndex];
            }
            tiles[0][columnIndex] = last;
        } else {
            Tile first = tiles[0][columnIndex];
            for (int row = 0; row < height - 1; row++) {
                tiles[row][columnIndex] = tiles[row + 1][columnIndex];
            }
            tiles[height - 1][columnIndex] = first;
        }

        initializeGraph();
    }

    /**
     * Returns all tiles reachable by a player from their current position.
     *
     * @param player the player
     * @return set of reachable tiles
     */
    public Set<Tile> getReachableTiles(Player player) {
        Position pos = player.getCurrentPosition();
        Tile startTile = tiles[pos.getRow()][pos.getColumn()];
        return graph.findReachable(startTile);
    }
}
