package labyrinth.game.models;

import labyrinth.game.enums.*;

import java.util.*;

/**
 * Represents the game board for the Labyrinth game.
 * The layout of tiles is stored in a bidirectional map between {@link Position}
 * instances and {@link Tile} instances. A separate graph tracks the connectivity
 * of neighboring tiles based on their entrances.
 */
public class Board {

    private final int width;
    private final int height;
    /**
     * A bidirectional mapping between board positions and the corresponding tiles. The forward
     * direction maps a {@link Position} (row/column coordinates) to a {@link Tile}. The
     * backward direction maps a {@link Tile} back to its {@link Position} on the board.
     *
     * <p>This replaces the old 2D array representation of the board and allows constant
     * time lookups of either side.</p>
     */
    private final BiMap<Position, Tile> tileMap;
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
    /**
     * Creates a board with the given dimensions. Although the constructor accepts a 2D
     * array of tiles for backwards compatibility, it immediately populates a
     * {@link BiMap} mapping each {@link Position} to its associated {@link Tile}. The
     * original array reference is not stored internally.
     *
     * @param width  number of columns
     * @param height number of rows
     * @param tiles  initial 2D array of tiles (height x width)
     * @param extraTile the spare tile which will be inserted during shifts
     */
    public Board(int width, int height, Tile[][] tilesArray, Tile extraTile) {
        if (tilesArray.length != height || tilesArray[0].length != width) {
            throw new IllegalArgumentException("Tile array dimensions must match width and height");
        }
        this.width = width;
        this.height = height;
        this.tileMap = new BiMap<>();
        // Populate the BiMap with positions and tiles
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Position position = new Position(row, col);
                Tile tile = tilesArray[row][col];
                tileMap.put(position, tile);
            }
        }
        this.graph = new Graph();
        // Build the connectivity graph based on the initial layout
        initializeGraph();
        this.extraTile = extraTile;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    /**
     * Returns a new 2D array representing the current board state. This method
     * constructs a fresh array on each invocation by reading from the underlying
     * {@link BiMap}. The caller should not attempt to modify the returned
     * array, as changes will not be reflected in the board.
     *
     * @return a copy of the current board tiles as a 2D array
     */
    public Tile[][] getTiles() {
        Tile[][] arr = new Tile[height][width];
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                arr[row][col] = tileMap.getForward(new Position(row, col));
            }
        }
        return arr;
    }

    /**
     * Retrieves the tile at the specified coordinates.
     *
     * @param row    row index (0-based)
     * @param column column index (0-based)
     * @return the tile located at (row, column), or null if not present
     */
    public Tile getTileAt(int row, int column) {
        return tileMap.getForward(new Position(row, column));
    }

    /**
     * Retrieves the {@link Position} of the given tile on the board.
     *
     * @param tile the tile to look up
     * @return the position of the tile, or null if it is not currently on the board
     */
    public Position getPositionOfTile(Tile tile) {
        return tileMap.getBackward(tile);
    }

    public Graph getGraph() {
        return graph;
    }

    public List<Player> getPlayers() {
        return new ArrayList<>(players);
    }

    public void setPlayers(List<Player> players) {
        this.players = players;
        // Ensure that each player's current tile and the tile's player reference
        // are synchronized when setting the list of players. This is necessary
        // after initial player placement in the game.
        synchronizePlayerTiles();
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
                Tile tile = tileMap.getForward(new Position(row, col));
                graph.addTile(tile);

                // Connect with neighbors if entrances match
                if (row > 0) {
                    Tile upNeighbor = tileMap.getForward(new Position(row - 1, col));
                    graph.connect(tile, upNeighbor, Direction.UP);
                }
                if (row < height - 1) {
                    Tile downNeighbor = tileMap.getForward(new Position(row + 1, col));
                    graph.connect(tile, downNeighbor, Direction.DOWN);
                }
                if (col > 0) {
                    Tile leftNeighbor = tileMap.getForward(new Position(row, col - 1));
                    graph.connect(tile, leftNeighbor, Direction.LEFT);
                }
                if (col < width - 1) {
                    Tile rightNeighbor = tileMap.getForward(new Position(row, col + 1));
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
            Tile tile = tileMap.getForward(new Position(rowIndex, col));
            if (tile.isFixed() && !freeRoam) {
                System.out.println("Row " + rowIndex + " contains fixed tiles. Cannot shift.");
                return;
            }
        }

        Map<Player, Position> affectedPlayers = new HashMap<>();
        // Determine which players are on this row by inspecting their current tiles
        for (Player p : players) {
            Tile playerTile = p.getCurrentTile();
            if (playerTile != null) {
                Position pos = getPositionOfTile(playerTile);
                if (pos != null && pos.getRow() == rowIndex) {
                    affectedPlayers.put(p, pos);
                }
            }
        }

        if (direction == Direction.RIGHT) {
            // Save the tile at the far right as it will become the new extra tile
            Tile last = tileMap.getForward(new Position(rowIndex, width - 1));
            // Shift tiles to the right
            for (int col = width - 1; col > 0; col--) {
                Tile from = tileMap.getForward(new Position(rowIndex, col - 1));
                tileMap.put(new Position(rowIndex, col), from);
            }
            // Insert the extra tile at the beginning
            tileMap.put(new Position(rowIndex, 0), extraTile);
            // The last tile becomes the new extra tile
            extraTile = last;

            // Update player tiles
            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newCol = oldPos.getColumn() + 1;
                if (newCol >= width) {
                    System.out.println(p.getName() + " got pushed out! Appears on left.");
                    newCol = 0;
                }
                // Set the player's tile to the tile now at the new position
                Tile newTile = tileMap.getForward(new Position(rowIndex, newCol));
                p.setCurrentTile(newTile);
            }
        } else {
            // Save the tile at the far left as it will become the new extra tile
            Tile first = tileMap.getForward(new Position(rowIndex, 0));
            // Shift tiles to the left
            for (int col = 0; col < width - 1; col++) {
                Tile from = tileMap.getForward(new Position(rowIndex, col + 1));
                tileMap.put(new Position(rowIndex, col), from);
            }
            // Insert the extra tile at the end
            tileMap.put(new Position(rowIndex, width - 1), extraTile);
            // The first tile becomes the new extra tile
            extraTile = first;

            // Update player tiles
            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newCol = oldPos.getColumn() - 1;
                if (newCol < 0) {
                    System.out.println(p.getName() + " got pushed out! Appears on right.");
                    newCol = width - 1;
                }
                Tile newTile = tileMap.getForward(new Position(rowIndex, newCol));
                p.setCurrentTile(newTile);
            }
        }

        // After shifting tiles and updating positions, update each player's current tile
        // and clear/set tile occupants accordingly.
        synchronizePlayerTiles();

        currentMoveState = MoveState.MOVE;
        initializeGraph();
    }

    /**
     * Synchronizes player state after tile shifts. Prior to removing the player reference
     * from {@link Tile}, this method cleared and reassigned tile occupants. Now it
     * remains as a placeholder so that any player-related post-shift logic can be
     * centralised here. Currently it performs no actions because player occupancy is
     * tracked solely on the {@link Player} instances.
     */
    private void synchronizePlayerTiles() {
        // Tiles no longer maintain occupancy; player location is stored
        // on each Player via its currentTile reference. This method remains
        // as a placeholder to be invoked after shifting tiles, ensuring any
        // board-specific logic related to players can be centralized here.
        // Currently it performs no actions.
    }


    /**
     * Shifts a column in the specified direction. Tiles wrap around.
     *
     * @param columnIndex the column to shift
     * @param direction   UP or DOWN
     */
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
            Tile tile = tileMap.getForward(new Position(row, columnIndex));
            if (tile.isFixed() && !freeRoam) {
                System.out.println("Column " + columnIndex + " contains fixed tiles. Cannot shift.");
                return;
            }
        }

        Map<Player, Position> affectedPlayers = new HashMap<>();
        // Determine which players are on this column by inspecting their current tiles
        for (Player p : players) {
            Tile playerTile = p.getCurrentTile();
            if (playerTile != null) {
                Position pos = getPositionOfTile(playerTile);
                if (pos != null && pos.getColumn() == columnIndex) {
                    affectedPlayers.put(p, pos);
                }
            }
        }

        if (direction == Direction.DOWN) {
            // Save the tile at the bottom which will become the new extra tile
            Tile bottom = tileMap.getForward(new Position(height - 1, columnIndex));
            // Shift tiles downwards
            for (int row = height - 1; row > 0; row--) {
                Tile from = tileMap.getForward(new Position(row - 1, columnIndex));
                tileMap.put(new Position(row, columnIndex), from);
            }
            // Insert the extra tile at the top
            tileMap.put(new Position(0, columnIndex), extraTile);
            // The bottom tile becomes the new extra tile
            extraTile = bottom;

            // Update player tiles
            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newRow = oldPos.getRow() + 1;
                if (newRow >= height) {
                    System.out.println(p.getName() + " got pushed out! Appears on top.");
                    newRow = 0;
                }
                Tile newTile = tileMap.getForward(new Position(newRow, columnIndex));
                p.setCurrentTile(newTile);
            }
        } else { // UP
            // Save the tile at the top which will become the new extra tile
            Tile top = tileMap.getForward(new Position(0, columnIndex));
            // Shift tiles upwards
            for (int row = 0; row < height - 1; row++) {
                Tile from = tileMap.getForward(new Position(row + 1, columnIndex));
                tileMap.put(new Position(row, columnIndex), from);
            }
            // Insert the extra tile at the bottom
            tileMap.put(new Position(height - 1, columnIndex), extraTile);
            // The top tile becomes the new extra tile
            extraTile = top;

            // Update player tiles
            for (Map.Entry<Player, Position> entry : affectedPlayers.entrySet()) {
                Player p = entry.getKey();
                Position oldPos = entry.getValue();
                int newRow = oldPos.getRow() - 1;
                if (newRow < 0) {
                    System.out.println(p.getName() + " got pushed out! Appears on bottom.");
                    newRow = height - 1;
                }
                Tile newTile = tileMap.getForward(new Position(newRow, columnIndex));
                p.setCurrentTile(newTile);
            }
        }

        // Synchronize players with their new tiles
        synchronizePlayerTiles();

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
        Tile startTile = player.getCurrentTile();
        if (startTile == null) {
            return Collections.emptySet();
        }
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
        Tile start = player.getCurrentTile();
        if (start == null) {
            return Collections.emptySet();
        }
        Position pos = getPositionOfTile(start);

        Set<Tile> visited = new HashSet<>();
        Queue<Position> queue = new ArrayDeque<>();

        visited.add(start);
        queue.add(pos);

        while (!queue.isEmpty()) {
            Position current = queue.poll();
            Tile currentTile = tileMap.getForward(current);

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
                Tile neighbor = tileMap.getForward(new Position(newRow, newCol));
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
            row = random.nextInt(height);
            col = random.nextInt(width);
            tile = tileMap.getForward(new Position(row, col));
            tileHasTreasure = tile != null && tile.getTreasureCard() != null;
        } while (isCornerCoordinate(row, col) || tileHasTreasure);
        System.out.println("Placing " + treasureCard.getTreasureName() + " at " + row + "/" + col);

        tile.setTreasureCard(treasureCard);
    }

    public boolean isCornerCoordinate(int row, int col) {
        boolean isTopLeft = (row == 0 && col == 0);
        boolean isTopRight = (row == 0 && col == this.width - 1);
        boolean isBottomLeft = (row == this.height - 1 && col == 0);
        boolean isBottomRight = (row == this.height - 1 && col == this.width - 1);

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

        // Lookup the player's current tile and the target tile using the bi-directional mapping
        Tile currentTile = player.getCurrentTile();
        Tile targetTile = tileMap.getForward(new Position(targetRow, targetCol));

        Position currPos = (currentTile != null) ? getPositionOfTile(currentTile) : null;
        System.out.println("Current position: " + (currPos != null ? currPos.getRow() + "/" + currPos.getColumn() : "none"));
        System.out.println("Moving " + player.getName() + " to " + targetRow + "/" + targetCol);
        // Check if another player is already on the target tile by inspecting players' currentTile
        for (Player other : players) {
            if (other != player && other.getCurrentTile() == targetTile) {
                System.out.println("Cant move a player is already on the target tile!");
                return false;
            }
        }

        Set<Tile> reachable = getReachableTiles(player);
        if (!reachable.contains(targetTile)) {
            System.out.println("Tile is not reachable!");
            return false;
        }

        // Step onto the target tile (which may collect treasures)
        targetTile.getSteppedOnBy(player);
        // Update player's logical tile
        player.setCurrentTile(targetTile);

        Position newPos = getPositionOfTile(targetTile);
        System.out.println("Player moved to " + (newPos != null ? newPos : "unknown"));
        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        currentMoveState = MoveState.PLACE_TILE;

        return true;
    }
}
