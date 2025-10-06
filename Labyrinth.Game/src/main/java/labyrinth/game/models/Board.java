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
    private List<Player> players;
    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;
    private Tile extraTile;
    private boolean freeRoam = false;

    /**
     * Creates a board with the given dimensions and tiles.
     *
     * @param width  number of columns
     * @param height number of rows
     * @param tiles  2D array of tiles (height x width)
     */
    public Board(int width, int height, Tile[][] tiles, Tile extraTile) {
        if (tiles.length != height || tiles[0].length != width) {
            throw new IllegalArgumentException("Tile array dimensions must match width and height");
        }
        this.width = width;
        this.height = height;
        this.tiles = tiles;
        this.graph = new Graph();
        initializeGraph();
        this.extraTile = extraTile;
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

    public List<Player> getPlayers() {
        return new ArrayList<>(players);
    }

    public void setPlayers(List<Player> players) {
        this.players = players;
    }

    public Tile getExtraTile() {
        return extraTile;
    }

    public void setFreeRoam(boolean freeRoam){
        this.freeRoam = freeRoam;
    }

    public boolean getFreeRoam(){
        return freeRoam;
    }

    public MoveState getCurrentMoveState() {
        return currentMoveState;
    }

    public int getCurrentPlayerIndex() {
        return currentPlayerIndex;
    }

    /**
     * Initializes the graph by connecting adjacent tiles based on entrances.
     */
    private void initializeGraph() {
        graph.clear();
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
    public void shiftRow(int rowIndex, Direction direction, Player player) {
        if(currentMoveState == MoveState.MOVE && !freeRoam) {
            System.out.println("Player needs to move a tile first");
            return;
        }
        if(player != players.get(currentPlayerIndex) && !freeRoam) {
            System.out.println("It's not the players turn!");
            return;
        }

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
            row[0] = extraTile;
            extraTile = last;
        } else {
            Tile first = row[0];
            System.arraycopy(row, 1, row, 0, width - 1);
            row[width - 1] = extraTile;
            extraTile = first;
        }

        currentMoveState = MoveState.MOVE;
        initializeGraph();
    }

    /**
     * Shifts a column in the specified direction. Tiles wrap around.
     *
     * @param columnIndex the column to shift
     * @param direction   UP or DOWN
     */
    public void shiftColumn(int columnIndex, Direction direction, Player player) {
        if(currentMoveState == MoveState.MOVE && !freeRoam){
            System.out.println("Player needs to move a tile first");
            return;
        }
        if(player != players.get(currentPlayerIndex) && !freeRoam) {
            System.out.println("It's not the players turn!");
            return;
        }

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
            tiles[0][columnIndex] = extraTile;
            extraTile = last;
        } else {
            Tile first = tiles[0][columnIndex];
            for (int row = 0; row < height - 1; row++) {
                tiles[row][columnIndex] = tiles[row + 1][columnIndex];
            }
            tiles[height - 1][columnIndex] = extraTile;
            extraTile = first;
        }

        currentMoveState = MoveState.MOVE;
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

    public void placeRandomTreasure(TreasureCard treasureCard) {
        Random random = new Random();
        Tile tile;
        int row, col;
        boolean tileHasTreasure;

        do {
            row = random.nextInt(tiles.length);
            col = random.nextInt(tiles[0].length);

            tile = tiles[row][col];
            tileHasTreasure = tile.getTreasureCard() != null;
        } while (isCornerCoordinate(row, col) || tileHasTreasure);
        System.out.println("Placing " + treasureCard.getTreasureName() + " at " + row + "/" + col);

        tile.setTreasureCard(treasureCard);
    }

    public boolean isCornerCoordinate(int row, int col) {
        int height = tiles.length;
        int width = tiles[0].length;

        boolean isTopLeft = (row == 0 && col == 0);
        boolean isTopRight = (row == 0 && col == width - 1);
        boolean isBottomLeft = (row == height - 1 && col == 0);
        boolean isBottomRight = (row == height - 1 && col == width - 1);

        return isTopLeft || isTopRight || isBottomLeft || isBottomRight;
    }

    public boolean movePlayerToTile(Player player, int targetRow, int targetCol) {
        if(player != players.get(currentPlayerIndex) && !freeRoam) {
            System.out.println("It's not the players turn!");
            return false;
        }

        if(currentMoveState != MoveState.MOVE && !freeRoam) {
            System.out.println("A tile needs to be moved!");
            return false;
        }

        Tile currentTile = tiles[player.getCurrentPosition().getRow()][player.getCurrentPosition().getColumn()];
        Tile targetTile = tiles[targetRow][targetCol];

        System.out.println("Current position: " + player.getCurrentPosition().getRow() + "/" + player.getCurrentPosition().getColumn());
        System.out.println("Moving " + player.getName() + " to " + targetRow + "/" + targetCol);
        if(targetTile.getPlayer() != null){
            System.out.println("Cant move a player is already on the target tile!");
            return false;
        }

        Set<Tile> reachable = getReachableTiles(player);
        if (!reachable.contains(targetTile)) {
            System.out.println("Tile is not reachable!");
            return false;
        }

        targetTile.getSteppedOnBy(player);
        player.setCurrentPosition(new Position(targetRow, targetCol));
        currentTile.setPlayer(null);

        System.out.println("Player moved to " + player.getCurrentPosition());
        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        currentMoveState = MoveState.PLACE_TILE;

        return  true;
    }
}
