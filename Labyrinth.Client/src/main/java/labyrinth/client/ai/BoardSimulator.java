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

    public BoardSimulator(Board board, Player player) {
        this(board, player, null);
    }

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

        this.otherPlayerPositions = new HashSet<>();
        if (allPlayers != null) {
            for (Player p : allPlayers) {
                if (!p.getId().equals(player.getId()) && p.getCurrentPosition() != null) {
                    otherPlayerPositions.add(p.getCurrentPosition());
                }
            }
        }
    }

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

    private Position findCurrentTreasurePosition(Board board, Player player) {
        Treasure currentTarget = player.getCurrentTargetTreasure();
        if (currentTarget == null) {
            return null;
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
     */
    public Position recalculateTargetTreasurePosition() {
        if (targetTreasure == null) {
            return null;
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

    public void rotateExtraTile() {
        extraTile.rotateClockwise();
    }

    /**
     * Applies a shift operation to the simulated board.
     * @return true if successful, false if blocked by fixed tiles
     */
    public boolean applyShift(ShiftOperation op) {
        if (op.isRow()) {
            return applyRowShift(op.index(), op.direction());
        } else {
            return applyColumnShift(op.index(), op.direction());
        }
    }

    private boolean applyRowShift(int rowIndex, Direction direction) {
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

            if (playerPosition.getRow() == rowIndex) {
                int newCol = (playerPosition.getColumn() + 1) % width;
                playerPosition = new Position(rowIndex, newCol);
            }
        } else {
            SimTile first = tiles[rowIndex][0];
            for (int c = 0; c < width - 1; c++) {
                tiles[rowIndex][c] = tiles[rowIndex][c + 1];
            }
            tiles[rowIndex][width - 1] = extraTile;
            extraTile = first;

            if (playerPosition.getRow() == rowIndex) {
                int newCol = (playerPosition.getColumn() - 1 + width) % width;
                playerPosition = new Position(rowIndex, newCol);
            }
        }
        return true;
    }

    private boolean applyColumnShift(int colIndex, Direction direction) {
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

            if (playerPosition.getColumn() == colIndex) {
                int newRow = (playerPosition.getRow() + 1) % height;
                playerPosition = new Position(newRow, colIndex);
            }
        } else {
            SimTile top = tiles[0][colIndex];
            for (int r = 0; r < height - 1; r++) {
                tiles[r][colIndex] = tiles[r + 1][colIndex];
            }
            tiles[height - 1][colIndex] = extraTile;
            extraTile = top;

            if (playerPosition.getColumn() == colIndex) {
                int newRow = (playerPosition.getRow() - 1 + height) % height;
                playerPosition = new Position(newRow, colIndex);
            }
        }
        return true;
    }

    /**
     * Gets all positions reachable from the player's current position using BFS.
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

                if (newRow < 0 || newRow >= height || newCol < 0 || newCol >= width) {
                    continue;
                }

                Position neighborPos = new Position(newRow, newCol);
                if (visited.contains(neighborPos)) {
                    continue;
                }

                SimTile neighbor = tiles[newRow][newCol];
                Direction opposite = opposite(dir);

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

    public boolean rowContainsFixedTile(int rowIndex) {
        for (int c = 0; c < width; c++) {
            if (tiles[rowIndex][c].isFixed()) {
                return true;
            }
        }
        return false;
    }

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
     * Gets the target position (treasure or home). Recalculates treasure position in case tiles shifted.
     */
    public Position getTargetPosition() {
        if (targetTreasure == null) {
            return homePosition;
        }
        Position currentTreasurePos = recalculateTargetTreasurePosition();
        return currentTreasurePos != null ? currentTreasurePos : homePosition;
    }

    public boolean isGoingHome() {
        return targetTreasurePosition == null;
    }

    public boolean isPositionBlocked(Position pos) {
        return otherPlayerPositions.contains(pos);
    }

    public Set<Position> getReachableUnblockedPositions() {
        Set<Position> reachable = getReachablePositions();
        reachable.removeAll(otherPlayerPositions);
        return reachable;
    }

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

    public SimTile getTileAt(Position pos) {
        if (pos.getRow() < 0 || pos.getRow() >= height || pos.getColumn() < 0 || pos.getColumn() >= width) {
            return null;
        }
        return tiles[pos.getRow()][pos.getColumn()];
    }

    public boolean hasBonus(BonusType bonus) {
        return availableBonuses.contains(bonus);
    }

    public void simulateBeam(Position target) {
        this.playerPosition = target;
    }

    public void simulateSwap(Position otherPlayerPos) {
        Position oldPos = this.playerPosition;
        this.playerPosition = otherPlayerPos;
        otherPlayerPositions.remove(otherPlayerPos);
        otherPlayerPositions.add(oldPos);
    }

    public boolean rowHasFixedTiles(int rowIndex) {
        for (int c = 0; c < width; c++) {
            if (tiles[rowIndex][c].isFixed()) {
                return true;
            }
        }
        return false;
    }

    public boolean colHasFixedTiles(int colIndex) {
        for (int r = 0; r < height; r++) {
            if (tiles[r][colIndex].isFixed()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets rows/columns with fixed tiles that can be pushed with PUSH_FIXED bonus.
     */
    public List<ShiftOperation> getPushFixedCandidates() {
        List<ShiftOperation> candidates = new ArrayList<>();

        for (int row = 1; row < height - 1; row++) {
            if (rowHasFixedTiles(row)) {
                candidates.add(ShiftOperation.row(row, Direction.LEFT));
                candidates.add(ShiftOperation.row(row, Direction.RIGHT));
            }
        }

        for (int col = 1; col < width - 1; col++) {
            if (colHasFixedTiles(col)) {
                candidates.add(ShiftOperation.column(col, Direction.UP));
                candidates.add(ShiftOperation.column(col, Direction.DOWN));
            }
        }

        return candidates;
    }

    /**
     * Applies a shift ignoring fixed tile restrictions (for PUSH_FIXED bonus simulation).
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
                int newCol = (playerPosition.getColumn() + 1) % width;
                playerPosition = new Position(rowIndex, newCol);
            }
            updateOtherPlayersInRow(rowIndex, 1);
        } else {
            SimTile first = tiles[rowIndex][0];
            for (int c = 0; c < width - 1; c++) {
                tiles[rowIndex][c] = tiles[rowIndex][c + 1];
            }
            tiles[rowIndex][width - 1] = extraTile;
            extraTile = first;

            if (playerPosition.getRow() == rowIndex) {
                int newCol = (playerPosition.getColumn() - 1 + width) % width;
                playerPosition = new Position(rowIndex, newCol);
            }
            updateOtherPlayersInRow(rowIndex, -1);
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
                int newRow = (playerPosition.getRow() + 1) % height;
                playerPosition = new Position(newRow, colIndex);
            }
            updateOtherPlayersInColumn(colIndex, 1);
        } else {
            SimTile top = tiles[0][colIndex];
            for (int r = 0; r < height - 1; r++) {
                tiles[r][colIndex] = tiles[r + 1][colIndex];
            }
            tiles[height - 1][colIndex] = extraTile;
            extraTile = top;

            if (playerPosition.getColumn() == colIndex) {
                int newRow = (playerPosition.getRow() - 1 + height) % height;
                playerPosition = new Position(newRow, colIndex);
            }
            updateOtherPlayersInColumn(colIndex, -1);
        }
    }

    private void updateOtherPlayersInRow(int rowIndex, int colDelta) {
        Set<Position> newPositions = new HashSet<>();
        for (Position pos : otherPlayerPositions) {
            if (pos.getRow() == rowIndex) {
                int newCol = (pos.getColumn() + colDelta + width) % width;
                newPositions.add(new Position(rowIndex, newCol));
            } else {
                newPositions.add(pos);
            }
        }
        otherPlayerPositions.clear();
        otherPlayerPositions.addAll(newPositions);
    }

    private void updateOtherPlayersInColumn(int colIndex, int rowDelta) {
        Set<Position> newPositions = new HashSet<>();
        for (Position pos : otherPlayerPositions) {
            if (pos.getColumn() == colIndex) {
                int newRow = (pos.getRow() + rowDelta + height) % height;
                newPositions.add(new Position(newRow, colIndex));
            } else {
                newPositions.add(pos);
            }
        }
        otherPlayerPositions.clear();
        otherPlayerPositions.addAll(newPositions);
    }

    public static int manhattanDistance(Position a, Position b) {
        return Math.abs(a.getRow() - b.getRow()) + Math.abs(a.getColumn() - b.getColumn());
    }

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
