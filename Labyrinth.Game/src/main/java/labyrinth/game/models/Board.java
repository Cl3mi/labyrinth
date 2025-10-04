package labyrinth.game.models;

import labyrinth.game.enums.*;

import java.util.*;

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

    /**
     * Calculates reachable tiles for a player directly using the board array
     * instead of the prebuilt Graph. Uses BFS for traversal.
     *
     * @param player the player whose reachable tiles should be calculated
     * @return set of reachable tiles
     */
    public Set<Tile> getReachableTilesArrayBased(Player player) {
        Position pos = player.getCurrentPosition();
        Tile start = tiles[pos.getRow()][pos.getColumn()];

        Set<Tile> visited = new HashSet<>();
        Queue<Position> queue = new ArrayDeque<>();

        visited.add(start);
        queue.add(pos);

        while (!queue.isEmpty()) {
            Position current = queue.poll();
            Tile currentTile = tiles[current.getRow()][current.getColumn()];

            for (Direction dir : Direction.values()) {
                int newRow = current.getRow();
                int newCol = current.getColumn();

                switch (dir) {
                    case UP -> newRow--;
                    case DOWN -> newRow++;
                    case LEFT -> newCol--;
                    case RIGHT -> newCol++;
                }

                // Bounds check BEFORE creating a Position
                if (newRow < 0 || newRow >= height || newCol < 0 || newCol >= width) {
                    continue;
                }

                Position neighborPos = new Position(newRow, newCol);
                Tile neighbor = tiles[newRow][newCol];
                Direction oppositeDir = dir.opposite();

                // Check if tiles are connected in this direction
                if (currentTile.isConnectedTo(neighbor, dir) &&
                        neighbor.isConnectedTo(currentTile, oppositeDir) &&
                        !visited.contains(neighbor)) {

                    visited.add(neighbor);
                    queue.add(neighborPos);
                }
            }
        }

        return visited;
    }


    /**
     * Prints a simple ASCII representation of the board to the console.
     * Each tile's shape depends on its entrances.
     */
    public void drawToConsole() {
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile = tiles[row][col];
                System.out.print(getTileSymbol(tile) + " ");
            }
            System.out.println();
        }
    }

    /**
     * Determines a printable symbol for a given tile based on its entrances.
     */
    private String getTileSymbol(Tile tile) {
        var e = tile.getEntrances();
        boolean up = e.contains(Direction.UP);
        boolean down = e.contains(Direction.DOWN);
        boolean left = e.contains(Direction.LEFT);
        boolean right = e.contains(Direction.RIGHT);

        // Corner pieces
        if (up && right && !down && !left) return "┌";
        if (up && left && !down && !right) return "┐";
        if (down && right && !up && !left) return "└";
        if (down && left && !up && !right) return "┘";

        // Straight pieces
        if (up && down && !left && !right) return "│";
        if (left && right && !up && !down) return "─";

        // T-junctions
        if (up && left && right && !down) return "┴";
        if (down && left && right && !up) return "┬";
        if (left && up && down && !right) return "┤";
        if (right && up && down && !left) return "├";

        // Cross (shouldn’t happen in labyrinth, but included for completeness)
        if (up && down && left && right) return "┼";

        // Default fallback
        return "·";
    }

    /**
     * Draws the board as a connected maze using ASCII art.
     * Each tile is drawn as a 3x3 block to properly visualize corridors.
     */
    public void drawPretty() {
        StringBuilder sb = new StringBuilder();

        for (int row = 0; row < height; row++) {
            // We build each tile as 3 rows of text
            StringBuilder top = new StringBuilder();
            StringBuilder mid = new StringBuilder();
            StringBuilder bot = new StringBuilder();

            for (int col = 0; col < width; col++) {
                Tile tile = tiles[row][col];
                Set<Direction> e = tile.getEntrances();
                boolean up = e.contains(Direction.UP);
                boolean down = e.contains(Direction.DOWN);
                boolean left = e.contains(Direction.LEFT);
                boolean right = e.contains(Direction.RIGHT);

                // Top row: either corridor or wall
                top.append(up ? "  |  " : "#####");

                // Middle row: walls left/right + treasure indicator
                if (left && right) mid.append("  o  ");
                else if (left)     mid.append("o####");
                else if (right)    mid.append("####o");
                else               mid.append("#####");

                // Bottom row: corridor or wall
                bot.append(down ? "  |  " : "#####");
            }

            // Add to output
            sb.append(top).append("\n");
            sb.append(mid).append("\n");
            sb.append(bot).append("\n");
        }

        System.out.println(sb.toString());
    }

}
