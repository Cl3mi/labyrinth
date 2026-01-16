package labyrinth.client.ai;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;

import java.util.*;

/**
 * Lightweight board simulation for AI move evaluation.
 * Creates a copy of the board state that can be safely modified during simulation.
 */
public class BoardSimulator {

    private final int width;
    private final int height;
    private final SimTile[][] tiles;
    private SimTile extraTile;
    private Position playerPosition;
    private final Position homePosition;
    private final Position targetTreasurePosition;

    /**
     * Creates a new board simulator from the given board and player.
     */
    public BoardSimulator(Board board, Player player) {
        this.width = board.getWidth();
        this.height = board.getHeight();
        this.tiles = copyTiles(board.getTiles());
        this.extraTile = copyTile(board.getExtraTile());
        this.playerPosition = new Position(
                player.getCurrentPosition().getRow(),
                player.getCurrentPosition().getColumn()
        );
        this.homePosition = player.getHomePosition();
        this.targetTreasurePosition = findCurrentTreasurePosition(board, player);
    }

    /**
     * Copy constructor for creating simulation variants.
     */
    private BoardSimulator(BoardSimulator other) {
        this.width = other.width;
        this.height = other.height;
        this.tiles = copySimTiles(other.tiles);
        this.extraTile = other.extraTile.copy();
        this.playerPosition = other.playerPosition;
        this.homePosition = other.homePosition;
        this.targetTreasurePosition = other.targetTreasurePosition;
    }

    /**
     * Creates a copy of this simulator for further simulation.
     */
    public BoardSimulator copy() {
        return new BoardSimulator(this);
    }

    private SimTile[][] copyTiles(Tile[][] original) {
        SimTile[][] copy = new SimTile[height][width];
        for (int r = 0; r < height; r++) {
            for (int c = 0; c < width; c++) {
                copy[r][c] = copyTile(original[r][c]);
            }
        }
        return copy;
    }

    private SimTile[][] copySimTiles(SimTile[][] original) {
        SimTile[][] copy = new SimTile[height][width];
        for (int r = 0; r < height; r++) {
            for (int c = 0; c < width; c++) {
                copy[r][c] = original[r][c].copy();
            }
        }
        return copy;
    }

    private SimTile copyTile(Tile original) {
        Direction[] entrances = original.getEntrances();
        Direction[] entrancesCopy = entrances != null ? entrances.clone() : new Direction[0];
        boolean isFixed = Boolean.TRUE.equals(original.getIsFixed());
        Treasure treasure = original.getTreasure();
        return new SimTile(entrancesCopy, isFixed, treasure);
    }

    /**
     * Finds the position of the player's current target treasure.
     */
    private Position findCurrentTreasurePosition(Board board, Player player) {
        Treasure currentTarget = player.getCurrentTargetTreasure();
        if (currentTarget == null) {
            return null; // All treasures collected - will target home
        }

        Tile[][] boardTiles = board.getTiles();
        for (int r = 0; r < height; r++) {
            for (int c = 0; c < width; c++) {
                Tile tile = boardTiles[r][c];
                if (tile.getTreasure() != null &&
                    tile.getTreasure().getId() == currentTarget.getId()) {
                    return new Position(r, c);
                }
            }
        }
        return null;
    }

    /**
     * Rotates the extra tile clockwise once.
     */
    public void rotateExtraTile() {
        extraTile.rotateClockwise();
    }

    /**
     * Applies a shift operation to the simulated board.
     *
     * @return true if the shift was successful, false if blocked by fixed tiles
     */
    public boolean applyShift(ShiftOperation op) {
        if (op.isRow()) {
            return applyRowShift(op.index(), op.direction());
        } else {
            return applyColumnShift(op.index(), op.direction());
        }
    }

    private boolean applyRowShift(int rowIndex, Direction direction) {
        // Check for fixed tiles in the row
        for (int c = 0; c < width; c++) {
            if (tiles[rowIndex][c].isFixed()) {
                return false;
            }
        }

        if (direction == Direction.RIGHT) {
            SimTile last = tiles[rowIndex][width - 1];
            for (int c = width - 1; c > 0; c--) {
                tiles[rowIndex][c] = tiles[rowIndex][c - 1];
            }
            tiles[rowIndex][0] = extraTile;
            extraTile = last;

            // Update player position if affected
            if (playerPosition.getRow() == rowIndex) {
                int newCol = playerPosition.getColumn() + 1;
                if (newCol >= width) newCol = 0; // Wrap around
                playerPosition = new Position(rowIndex, newCol);
            }
        } else { // LEFT
            SimTile first = tiles[rowIndex][0];
            for (int c = 0; c < width - 1; c++) {
                tiles[rowIndex][c] = tiles[rowIndex][c + 1];
            }
            tiles[rowIndex][width - 1] = extraTile;
            extraTile = first;

            // Update player position if affected
            if (playerPosition.getRow() == rowIndex) {
                int newCol = playerPosition.getColumn() - 1;
                if (newCol < 0) newCol = width - 1; // Wrap around
                playerPosition = new Position(rowIndex, newCol);
            }
        }
        return true;
    }

    private boolean applyColumnShift(int colIndex, Direction direction) {
        // Check for fixed tiles in the column
        for (int r = 0; r < height; r++) {
            if (tiles[r][colIndex].isFixed()) {
                return false;
            }
        }

        if (direction == Direction.DOWN) {
            SimTile bottom = tiles[height - 1][colIndex];
            for (int r = height - 1; r > 0; r--) {
                tiles[r][colIndex] = tiles[r - 1][colIndex];
            }
            tiles[0][colIndex] = extraTile;
            extraTile = bottom;

            // Update player position if affected
            if (playerPosition.getColumn() == colIndex) {
                int newRow = playerPosition.getRow() + 1;
                if (newRow >= height) newRow = 0; // Wrap around
                playerPosition = new Position(newRow, colIndex);
            }
        } else { // UP
            SimTile top = tiles[0][colIndex];
            for (int r = 0; r < height - 1; r++) {
                tiles[r][colIndex] = tiles[r + 1][colIndex];
            }
            tiles[height - 1][colIndex] = extraTile;
            extraTile = top;

            // Update player position if affected
            if (playerPosition.getColumn() == colIndex) {
                int newRow = playerPosition.getRow() - 1;
                if (newRow < 0) newRow = height - 1; // Wrap around
                playerPosition = new Position(newRow, colIndex);
            }
        }
        return true;
    }

    /**
     * Gets all positions reachable from the player's current position.
     */
    public Set<Position> getReachablePositions() {
        Set<Position> visited = new HashSet<>();
        Queue<Position> queue = new ArrayDeque<>();

        visited.add(playerPosition);
        queue.add(playerPosition);

        while (!queue.isEmpty()) {
            Position current = queue.poll();
            SimTile currentTile = tiles[current.getRow()][current.getColumn()];

            for (Direction dir : Direction.values()) {
                int newRow = current.getRow();
                int newCol = current.getColumn();

                switch (dir) {
                    case UP -> newRow--;
                    case DOWN -> newRow++;
                    case LEFT -> newCol--;
                    case RIGHT -> newCol++;
                }

                // Bounds check
                if (newRow < 0 || newRow >= height || newCol < 0 || newCol >= width) {
                    continue;
                }

                Position neighborPos = new Position(newRow, newCol);
                if (visited.contains(neighborPos)) {
                    continue;
                }

                SimTile neighbor = tiles[newRow][newCol];
                Direction opposite = opposite(dir);

                // Check if both tiles have matching entrances
                if (currentTile.hasEntrance(dir) && neighbor.hasEntrance(opposite)) {
                    visited.add(neighborPos);
                    queue.add(neighborPos);
                }
            }
        }

        return visited;
    }

    private Direction opposite(Direction dir) {
        return switch (dir) {
            case UP -> Direction.DOWN;
            case DOWN -> Direction.UP;
            case LEFT -> Direction.RIGHT;
            case RIGHT -> Direction.LEFT;
        };
    }

    /**
     * Checks if a row contains any fixed tiles.
     */
    public boolean rowContainsFixedTile(int rowIndex) {
        for (int c = 0; c < width; c++) {
            if (tiles[rowIndex][c].isFixed()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if a column contains any fixed tiles.
     */
    public boolean colContainsFixedTile(int colIndex) {
        for (int r = 0; r < height; r++) {
            if (tiles[r][colIndex].isFixed()) {
                return true;
            }
        }
        return false;
    }

    public int getWidth() { return width; }
    public int getHeight() { return height; }
    public Position getPlayerPosition() { return playerPosition; }
    public Position getHomePosition() { return homePosition; }
    public Position getTargetTreasurePosition() { return targetTreasurePosition; }

    /**
     * Gets the target position: treasure position if available, otherwise home.
     */
    public Position getTargetPosition() {
        return targetTreasurePosition != null ? targetTreasurePosition : homePosition;
    }

    /**
     * Checks if the player is going home (all treasures collected).
     */
    public boolean isGoingHome() {
        return targetTreasurePosition == null;
    }

    /**
     * Lightweight tile representation for simulation.
     */
    private static class SimTile {
        private Direction[] entrances;
        private final boolean isFixed;
        private final Treasure treasure;

        SimTile(Direction[] entrances, boolean isFixed, Treasure treasure) {
            this.entrances = entrances;
            this.isFixed = isFixed;
            this.treasure = treasure;
        }

        SimTile copy() {
            return new SimTile(entrances.clone(), isFixed, treasure);
        }

        boolean isFixed() {
            return isFixed;
        }

        boolean hasEntrance(Direction dir) {
            for (Direction d : entrances) {
                if (d == dir) return true;
            }
            return false;
        }

        void rotateClockwise() {
            Direction[] rotated = new Direction[entrances.length];
            for (int i = 0; i < entrances.length; i++) {
                rotated[i] = switch (entrances[i]) {
                    case UP -> Direction.RIGHT;
                    case RIGHT -> Direction.DOWN;
                    case DOWN -> Direction.LEFT;
                    case LEFT -> Direction.UP;
                };
            }
            entrances = rotated;
        }
    }
}
