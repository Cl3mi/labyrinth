package labyrinth.client.models;

import labyrinth.client.enums.MoveState;
import labyrinth.client.models.extensions.TreasureUtils;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * Repräsentiert das Spielfeld für das Labyrinth-Spiel.
 * Nutzt contracts.Tile und contracts.Direction.
 */

@Getter
@Setter
public class Board {

    private final int width;
    private final int height;
    private final Tile[][] tiles;
    private final Graph graph;

    private List<Player> players;
    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;
    private Tile extraTile;
    private boolean freeRoam = false;

    public Board(int width, int height, Tile[][] tiles, Tile extraTile) {
        if (tiles.length != height || tiles[0].length != width) {
            throw new IllegalArgumentException("Tile array dimensions must match width and height");
        }
        this.width = width;
        this.height = height;
        this.tiles = tiles;
        this.graph = new Graph();
        this.extraTile = extraTile;

        initializeGraph();
    }

    private void initializeGraph() {
        graph.clear();
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile = tiles[row][col];
                graph.addTile(tile);

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

    // =================================================================================
    // Row / Column shifts
    // =================================================================================

    public void shiftRow(int rowIndex, Direction direction, Player player) {
        if (currentMoveState == MoveState.MOVE && !freeRoam) {
            System.out.println("Player needs to move a tile first");
            return;
        }
        if (player != players.get(currentPlayerIndex) && !freeRoam) {
            System.out.println("It's not the player's turn!");
            return;
        }

        if (rowIndex < 0 || rowIndex >= height)
            throw new IllegalArgumentException("Invalid row index");

        if (direction != Direction.LEFT && direction != Direction.RIGHT)
            throw new IllegalArgumentException("Row can only be shifted LEFT or RIGHT");

        for (int col = 0; col < width; col++) {
            if (Boolean.TRUE.equals(tiles[rowIndex][col].getIsFixed()) && !freeRoam) {
                System.out.println("Row " + rowIndex + " contains fixed tiles. Cannot shift.");
                return;
            }
        }

        Map<Player, Position> affectedPlayers = new HashMap<>();
        for (Player p : players) {
            Position pos = p.getCurrentPosition();
            if (pos != null && pos.getRow() == rowIndex) {
                affectedPlayers.put(p, pos);
            }
        }

        if (direction == Direction.RIGHT) {
            Tile last = tiles[rowIndex][width - 1];
            for (int col = width - 1; col > 0; col--) {
                tiles[rowIndex][col] = tiles[rowIndex][col - 1];
            }
            tiles[rowIndex][0] = extraTile;
            extraTile = last;

            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newCol = oldPos.getColumn() + 1;
                if (newCol >= width) {
                    System.out.println(p.getName() + " got pushed out! Appears on left.");
                    newCol = 0;
                }
                p.setCurrentPosition(new Position(rowIndex, newCol));
            }
        } else {
            Tile first = tiles[rowIndex][0];
            for (int col = 0; col < width - 1; col++) {
                tiles[rowIndex][col] = tiles[rowIndex][col + 1];
            }
            tiles[rowIndex][width - 1] = extraTile;
            extraTile = first;

            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newCol = oldPos.getColumn() - 1;
                if (newCol < 0) {
                    System.out.println(p.getName() + " got pushed out! Appears on right.");
                    newCol = width - 1;
                }
                p.setCurrentPosition(new Position(rowIndex, newCol));
            }
        }

        currentMoveState = MoveState.MOVE;
        initializeGraph();
    }

    public void shiftColumn(int columnIndex, Direction direction, Player player) {
        if (currentMoveState == MoveState.MOVE && !freeRoam) {
            System.out.println("Player needs to move a tile first");
            return;
        }
        if (player != players.get(currentPlayerIndex) && !freeRoam) {
            System.out.println("It's not the player's turn!");
            return;
        }

        if (columnIndex < 0 || columnIndex >= width)
            throw new IllegalArgumentException("Invalid column index");

        if (direction != Direction.UP && direction != Direction.DOWN)
            throw new IllegalArgumentException("Column can only be shifted UP or DOWN");

        for (int row = 0; row < height; row++) {
            Tile tile = tiles[row][columnIndex];
            if (Boolean.TRUE.equals(tile.getIsFixed()) && !freeRoam) {
                System.out.println("Column " + columnIndex + " contains fixed tiles. Cannot shift.");
                return;
            }
        }

        Map<Player, Position> affectedPlayers = new HashMap<>();
        for (Player p : players) {
            Position pos = p.getCurrentPosition();
            if (pos != null && pos.getColumn() == columnIndex) {
                affectedPlayers.put(p, pos);
            }
        }

        if (direction == Direction.DOWN) {
            Tile bottom = tiles[height - 1][columnIndex];
            for (int row = height - 1; row > 0; row--) {
                tiles[row][columnIndex] = tiles[row - 1][columnIndex];
            }
            tiles[0][columnIndex] = extraTile;
            extraTile = bottom;

            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newRow = oldPos.getRow() + 1;

                if (newRow >= height) {
                    System.out.println(p.getName() + " got pushed out! Appears on top.");
                    newRow = 0;
                }

                p.setCurrentPosition(new Position(newRow, columnIndex));
            }
        } else {
            Tile top = tiles[0][columnIndex];
            for (int row = 0; row < height - 1; row++) {
                tiles[row][columnIndex] = tiles[row + 1][columnIndex];
            }
            tiles[height - 1][columnIndex] = extraTile;
            extraTile = top;

            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newRow = oldPos.getRow() - 1;

                if (newRow < 0) {
                    System.out.println(p.getName() + " got pushed out! Appears on bottom.");
                    newRow = height - 1;
                }

                p.setCurrentPosition(new Position(newRow, columnIndex));
            }
        }

        currentMoveState = MoveState.MOVE;
        initializeGraph();
    }

    // =================================================================================
    // Reachability
    // =================================================================================

    public Set<Tile> getReachableTiles(Player player) {
        Position pos = player.getCurrentPosition();
        Tile startTile = tiles[pos.getRow()][pos.getColumn()];
        return graph.findReachable(startTile);
    }

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

                if (newRow < 0 || newRow >= height || newCol < 0 || newCol >= width) {
                    continue;
                }

                Tile neighbor = tiles[newRow][newCol];
                Direction oppositeDir = opposite(dir);

                if (hasEntrance(currentTile, dir) &&
                        hasEntrance(neighbor, oppositeDir) &&
                        !visited.contains(neighbor)) {

                    visited.add(neighbor);
                    queue.add(new Position(newRow, newCol));
                }
            }
        }

        return visited;
    }

    private boolean hasEntrance(Tile tile, Direction dir) {
        Direction[] entrances = tile.getEntrances();
        if (entrances == null) return false;
        for (Direction d : entrances) {
            if (d == dir) return true;
        }
        return false;
    }

    private Direction opposite(Direction dir) {
        return switch (dir) {
            case UP -> Direction.DOWN;
            case DOWN -> Direction.UP;
            case LEFT -> Direction.RIGHT;
            case RIGHT -> Direction.LEFT;
        };
    }

    // =================================================================================
    // Treasures & Movement
    // =================================================================================

    public void placeRandomTreasure(Treasure treasure) {
        Random random = new Random();
        Tile tile;
        int row, col;
        boolean tileHasTreasure;

        do {
            row = random.nextInt(tiles.length);
            col = random.nextInt(tiles[0].length);

            tile = tiles[row][col];
            tileHasTreasure = tile.getTreasure() != null;
        } while (isCornerCoordinate(row, col) || tileHasTreasure);
        System.out.println("Placing " + TreasureUtils.getLocalName(treasure.getId()) + " at " + row + "/" + col);

        tile.setTreasure(treasure);
    }

    public boolean isCornerCoordinate(int row, int col) {
        int h = tiles.length;
        int w = tiles[0].length;

        boolean isTopLeft = (row == 0 && col == 0);
        boolean isTopRight = (row == 0 && col == w - 1);
        boolean isBottomLeft = (row == h - 1 && col == 0);
        boolean isBottomRight = (row == h - 1 && col == w - 1);

        return isTopLeft || isTopRight || isBottomLeft || isBottomRight;
    }

    public boolean movePlayerToTile(Player player, int targetRow, int targetCol) {
        if (player != players.get(currentPlayerIndex) && !freeRoam) {
            System.out.println("It's not the players turn!");
            return false;
        }

        if (currentMoveState != MoveState.MOVE && !freeRoam) {
            System.out.println("A tile needs to be moved!");
            return false;
        }

        Position currentPos = player.getCurrentPosition();
        Tile currentTile = tiles[currentPos.getRow()][currentPos.getColumn()];
        Tile targetTile = tiles[targetRow][targetCol];

        System.out.println("Current position: " + currentPos.getRow() + "/" + currentPos.getColumn());
        System.out.println("Moving " + player.getName() + " to " + targetRow + "/" + targetCol);

        for (Player p : players) {
            if (p == player) continue;
            Position pPos = p.getCurrentPosition();
            if (pPos != null && pPos.getRow() == targetRow && pPos.getColumn() == targetCol) {
                System.out.println("Cant move, another player is already on the target tile!");
                return false;
            }
        }

        Set<Tile> reachable = getReachableTiles(player);
        if (!reachable.contains(targetTile)) {
            System.out.println("Tile is not reachable!");
            return false;
        }

        player.setCurrentPosition(new Position(targetRow, targetCol));

        System.out.println("Player moved to " + player.getCurrentPosition());
        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        currentMoveState = MoveState.PLACE_TILE;

        return true;
    }
}
