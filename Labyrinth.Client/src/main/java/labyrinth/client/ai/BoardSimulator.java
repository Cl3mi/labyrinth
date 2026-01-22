package labyrinth.client.ai;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
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
    private final Treasure targetTreasure;
    private final Position targetTreasurePosition;
    private final Set<Position> otherPlayerPositions;
    private final List<BonusType> availableBonuses;

    /**
     * Creates a new board simulator from the given board and player.
     */
    public BoardSimulator(Board board, Player player) {
        this(board, player, null);
    }

    /**
     * Creates a new board simulator from the given board, player, and all players list.
     */
    public BoardSimulator(Board board, Player player, List<Player> allPlayers) {
        this.width = board.getWidth();
        this.height = board.getHeight();
        this.tiles = copyTiles(board.getTiles());
        this.extraTile = copyTile(board.getExtraTile());
        this.playerPosition = new Position(
                player.getCurrentPosition().getRow(),
                player.getCurrentPosition().getColumn()
        );
        this.homePosition = player.getHomePosition();
        this.targetTreasure = player.getCurrentTargetTreasure();
        this.targetTreasurePosition = findCurrentTreasurePosition(board, player);
        this.availableBonuses = new ArrayList<>(player.getAvailableBonuses());

        // Track other player positions
        this.otherPlayerPositions = new HashSet<>();
        if (allPlayers != null) {
            for (Player p : allPlayers) {
                if (!p.getId().equals(player.getId()) && p.getCurrentPosition() != null) {
                    otherPlayerPositions.add(p.getCurrentPosition());
                }
            }
        }
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
        this.targetTreasure = other.targetTreasure;
        this.targetTreasurePosition = other.targetTreasurePosition;
        this.otherPlayerPositions = new HashSet<>(other.otherPlayerPositions);
        this.availableBonuses = new ArrayList<>(other.availableBonuses);
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
        BonusType bonus = original.getBonus();
        return new SimTile(entrancesCopy, isFixed, treasure, bonus);
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
     * Recalculates the target treasure position based on current simulated board state.
     * This is needed because tiles shift during simulation.
     */
    public Position recalculateTargetTreasurePosition() {
        if (targetTreasure == null) {
            return null; // Going home
        }

        for (int r = 0; r < height; r++) {
            for (int c = 0; c < width; c++) {
                SimTile tile = tiles[r][c];
                if (tile.getTreasure() != null &&
                    tile.getTreasure().getId() == targetTreasure.getId()) {
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
    public Set<Position> getOtherPlayerPositions() { return otherPlayerPositions; }
    public List<BonusType> getAvailableBonuses() { return availableBonuses; }

    /**
     * Gets the target position: treasure position if available, otherwise home.
     * Recalculates the treasure position in case tiles have shifted.
     */
    public Position getTargetPosition() {
        if (targetTreasure == null) {
            return homePosition; // Going home
        }
        // Recalculate because tiles may have shifted
        Position currentTreasurePos = recalculateTargetTreasurePosition();
        return currentTreasurePos != null ? currentTreasurePos : homePosition;
    }

    /**
     * Checks if the player is going home (all treasures collected).
     */
    public boolean isGoingHome() {
        return targetTreasurePosition == null;
    }

    /**
     * Checks if a position is occupied by another player.
     */
    public boolean isPositionBlocked(Position pos) {
        return otherPlayerPositions.contains(pos);
    }

    /**
     * Gets all reachable positions that are not blocked by other players.
     */
    public Set<Position> getReachableUnblockedPositions() {
        Set<Position> reachable = getReachablePositions();
        reachable.removeAll(otherPlayerPositions);
        return reachable;
    }

    /**
     * Finds all bonus positions on the board.
     */
    public Map<Position, BonusType> findAllBonusPositions() {
        Map<Position, BonusType> bonuses = new HashMap<>();
        for (int r = 0; r < height; r++) {
            for (int c = 0; c < width; c++) {
                SimTile tile = tiles[r][c];
                if (tile.getBonus() != null) {
                    bonuses.put(new Position(r, c), tile.getBonus());
                }
            }
        }
        return bonuses;
    }

    /**
     * Finds reachable bonus positions (not blocked by other players).
     */
    public Map<Position, BonusType> findReachableBonuses() {
        Set<Position> reachable = getReachableUnblockedPositions();
        Map<Position, BonusType> bonuses = new HashMap<>();
        for (Position pos : reachable) {
            SimTile tile = tiles[pos.getRow()][pos.getColumn()];
            if (tile.getBonus() != null) {
                bonuses.put(pos, tile.getBonus());
            }
        }
        return bonuses;
    }

    /**
     * Gets the tile at a given position.
     */
    public SimTile getTileAt(Position pos) {
        if (pos.getRow() < 0 || pos.getRow() >= height || pos.getColumn() < 0 || pos.getColumn() >= width) {
            return null;
        }
        return tiles[pos.getRow()][pos.getColumn()];
    }

    /**
     * Checks if the player has a specific bonus available.
     */
    public boolean hasBonus(BonusType bonus) {
        return availableBonuses.contains(bonus);
    }

    /**
     * Gets all positions reachable after a BEAM teleport to any reachable tile.
     * This is a superset of normal reachability - from any reachable tile, compute further reachability.
     */
    public Set<Position> getAllPositionsReachableWithBeam() {
        Set<Position> allReachable = new HashSet<>();
        Set<Position> directlyReachable = getReachablePositions();
        allReachable.addAll(directlyReachable);

        // BEAM allows teleporting to any reachable position
        // From there, we can reach more positions (but this is the same as just being at that position)
        // So BEAM essentially lets us move to any reachable position directly
        // The key advantage is we can teleport THEN walk in the same turn
        return allReachable;
    }

    /**
     * Simulates using BEAM to teleport to a target position.
     * Updates player position to the target.
     */
    public void simulateBeam(Position target) {
        this.playerPosition = target;
    }

    /**
     * Simulates a SWAP with another player at the given position.
     * Updates player position and removes the other player from their old position.
     */
    public void simulateSwap(Position otherPlayerPos) {
        Position oldPos = this.playerPosition;
        this.playerPosition = otherPlayerPos;
        // The other player moves to our old position
        otherPlayerPositions.remove(otherPlayerPos);
        otherPlayerPositions.add(oldPos);
    }

    /**
     * Checks if a row contains fixed tiles (for PUSH_FIXED evaluation).
     */
    public boolean rowHasFixedTiles(int rowIndex) {
        for (int c = 0; c < width; c++) {
            if (tiles[rowIndex][c].isFixed()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if a column contains fixed tiles (for PUSH_FIXED evaluation).
     */
    public boolean colHasFixedTiles(int colIndex) {
        for (int r = 0; r < height; r++) {
            if (tiles[r][colIndex].isFixed()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets all valid PUSH_FIXED operations (rows/columns that have fixed tiles but could be pushed).
     * Note: Outer rows/columns (0 and height-1/width-1) cannot be pushed even with PUSH_FIXED.
     */
    public List<ShiftOperation> getPushFixedCandidates() {
        List<ShiftOperation> candidates = new ArrayList<>();

        // Row shifts - only inner rows that have fixed tiles
        for (int row = 1; row < height - 1; row++) {
            if (rowHasFixedTiles(row)) {
                candidates.add(ShiftOperation.row(row, Direction.LEFT));
                candidates.add(ShiftOperation.row(row, Direction.RIGHT));
            }
        }

        // Column shifts - only inner columns that have fixed tiles
        for (int col = 1; col < width - 1; col++) {
            if (colHasFixedTiles(col)) {
                candidates.add(ShiftOperation.column(col, Direction.UP));
                candidates.add(ShiftOperation.column(col, Direction.DOWN));
            }
        }

        return candidates;
    }

    /**
     * Applies a shift ignoring fixed tiles (for PUSH_FIXED simulation).
     * This version always succeeds for valid indices.
     */
    public void applyShiftIgnoringFixed(ShiftOperation op) {
        if (op.isRow()) {
            applyRowShiftIgnoringFixed(op.index(), op.direction());
        } else {
            applyColumnShiftIgnoringFixed(op.index(), op.direction());
        }
    }

    private void applyRowShiftIgnoringFixed(int rowIndex, Direction direction) {
        if (direction == Direction.RIGHT) {
            SimTile last = tiles[rowIndex][width - 1];
            for (int c = width - 1; c > 0; c--) {
                tiles[rowIndex][c] = tiles[rowIndex][c - 1];
            }
            tiles[rowIndex][0] = extraTile;
            extraTile = last;

            if (playerPosition.getRow() == rowIndex) {
                int newCol = playerPosition.getColumn() + 1;
                if (newCol >= width) newCol = 0;
                playerPosition = new Position(rowIndex, newCol);
            }
            // Update other player positions in this row
            Set<Position> newOtherPositions = new HashSet<>();
            for (Position pos : otherPlayerPositions) {
                if (pos.getRow() == rowIndex) {
                    int newCol = pos.getColumn() + 1;
                    if (newCol >= width) newCol = 0;
                    newOtherPositions.add(new Position(rowIndex, newCol));
                } else {
                    newOtherPositions.add(pos);
                }
            }
            otherPlayerPositions.clear();
            otherPlayerPositions.addAll(newOtherPositions);
        } else { // LEFT
            SimTile first = tiles[rowIndex][0];
            for (int c = 0; c < width - 1; c++) {
                tiles[rowIndex][c] = tiles[rowIndex][c + 1];
            }
            tiles[rowIndex][width - 1] = extraTile;
            extraTile = first;

            if (playerPosition.getRow() == rowIndex) {
                int newCol = playerPosition.getColumn() - 1;
                if (newCol < 0) newCol = width - 1;
                playerPosition = new Position(rowIndex, newCol);
            }
            // Update other player positions in this row
            Set<Position> newOtherPositions = new HashSet<>();
            for (Position pos : otherPlayerPositions) {
                if (pos.getRow() == rowIndex) {
                    int newCol = pos.getColumn() - 1;
                    if (newCol < 0) newCol = width - 1;
                    newOtherPositions.add(new Position(rowIndex, newCol));
                } else {
                    newOtherPositions.add(pos);
                }
            }
            otherPlayerPositions.clear();
            otherPlayerPositions.addAll(newOtherPositions);
        }
    }

    private void applyColumnShiftIgnoringFixed(int colIndex, Direction direction) {
        if (direction == Direction.DOWN) {
            SimTile bottom = tiles[height - 1][colIndex];
            for (int r = height - 1; r > 0; r--) {
                tiles[r][colIndex] = tiles[r - 1][colIndex];
            }
            tiles[0][colIndex] = extraTile;
            extraTile = bottom;

            if (playerPosition.getColumn() == colIndex) {
                int newRow = playerPosition.getRow() + 1;
                if (newRow >= height) newRow = 0;
                playerPosition = new Position(newRow, colIndex);
            }
            // Update other player positions in this column
            Set<Position> newOtherPositions = new HashSet<>();
            for (Position pos : otherPlayerPositions) {
                if (pos.getColumn() == colIndex) {
                    int newRow = pos.getRow() + 1;
                    if (newRow >= height) newRow = 0;
                    newOtherPositions.add(new Position(newRow, colIndex));
                } else {
                    newOtherPositions.add(pos);
                }
            }
            otherPlayerPositions.clear();
            otherPlayerPositions.addAll(newOtherPositions);
        } else { // UP
            SimTile top = tiles[0][colIndex];
            for (int r = 0; r < height - 1; r++) {
                tiles[r][colIndex] = tiles[r + 1][colIndex];
            }
            tiles[height - 1][colIndex] = extraTile;
            extraTile = top;

            if (playerPosition.getColumn() == colIndex) {
                int newRow = playerPosition.getRow() - 1;
                if (newRow < 0) newRow = height - 1;
                playerPosition = new Position(newRow, colIndex);
            }
            // Update other player positions in this column
            Set<Position> newOtherPositions = new HashSet<>();
            for (Position pos : otherPlayerPositions) {
                if (pos.getColumn() == colIndex) {
                    int newRow = pos.getRow() - 1;
                    if (newRow < 0) newRow = height - 1;
                    newOtherPositions.add(new Position(newRow, colIndex));
                } else {
                    newOtherPositions.add(pos);
                }
            }
            otherPlayerPositions.clear();
            otherPlayerPositions.addAll(newOtherPositions);
        }
    }

    /**
     * Calculate Manhattan distance between two positions.
     */
    public static int manhattanDistance(Position a, Position b) {
        return Math.abs(a.getRow() - b.getRow()) + Math.abs(a.getColumn() - b.getColumn());
    }

    /**
     * Lightweight tile representation for simulation.
     */
    public static class SimTile {
        private Direction[] entrances;
        private final boolean isFixed;
        private final Treasure treasure;
        private final BonusType bonus;

        SimTile(Direction[] entrances, boolean isFixed, Treasure treasure, BonusType bonus) {
            this.entrances = entrances;
            this.isFixed = isFixed;
            this.treasure = treasure;
            this.bonus = bonus;
        }

        SimTile copy() {
            return new SimTile(entrances.clone(), isFixed, treasure, bonus);
        }

        public boolean isFixed() {
            return isFixed;
        }

        public boolean hasEntrance(Direction dir) {
            for (Direction d : entrances) {
                if (d == dir) return true;
            }
            return false;
        }

        public BonusType getBonus() {
            return bonus;
        }

        public Treasure getTreasure() {
            return treasure;
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
