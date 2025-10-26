package labyrinth.game.models;

import labyrinth.game.abstractions.IBoardEventListener;
import labyrinth.game.enums.*;
import labyrinth.game.events.BoardEvent;

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
    private final BiMap<Position, Tile> tileMap;
    private final Graph graph;
    private List<Player> players;
    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;
    private Tile extraTile;
    private boolean freeRoam = false;

    /**
     * Creates a board with the given dimensions. Although the constructor accepts a 2D
     * array of tiles for backwards compatibility, it immediately populates a
     * {@link BiMap} mapping each {@link Position} to its associated {@link Tile}. The
     * original array reference is not stored internally.
     *
     * @param width  number of columns
     * @param height number of rows
     * @param tileMap  the bidirectional tilemap
     * @param extraTile the spare tile which will be inserted during shifts
     */
    public Board(int width, int height, BiMap<Position, Tile> tileMap, Tile extraTile) {
        this.width = width;
        this.height = height;
        this.tileMap = tileMap;
        this.graph = new Graph(this);
        initializeGraph();
        this.extraTile = extraTile;
    }


    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public BiMap<Position, Tile> getTileMap() {
        return tileMap;
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
    public void initializeGraph() {
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
        System.out.println("Graph initialized");
    }

    protected boolean shiftColumnDown(int columnIndex) {
        if(colContainsFixedTile(columnIndex)) {
            return false;
        }

        Tile pushedOut = tileMap.getForward(new Position(height - 1, columnIndex));
        Tile bottom = tileMap.getForward(new Position(height - 1, columnIndex));

        for (int row = height - 1; row > 0; row--) {
            Tile from = tileMap.getForward(new Position(row - 1, columnIndex));
            tileMap.put(new Position(row, columnIndex), from);
        }

        tileMap.put(new Position(0, columnIndex), extraTile);
        adjustPlayersOnPushedOutTile(pushedOut);
        extraTile = bottom;
        notifyListeners(BoardEventType.COLUMN_SHIFTED, columnIndex);
        return true;
    }

    protected boolean shiftColumnUp(int columnIndex) {
        if(colContainsFixedTile(columnIndex)) {
            return false;
        }

        Tile pushedOut = tileMap.getForward(new Position(0, columnIndex));
        Tile top = tileMap.getForward(new Position(0, columnIndex));
        for (int row = 0; row < height - 1; row++) {
            Tile from = tileMap.getForward(new Position(row + 1, columnIndex));
            tileMap.put(new Position(row, columnIndex), from);
        }
        tileMap.put(new Position(height - 1, columnIndex), extraTile);
        adjustPlayersOnPushedOutTile(pushedOut);
        extraTile = top;
        notifyListeners(BoardEventType.COLUMN_SHIFTED, columnIndex);
        return true;
    }

    protected boolean shiftRowLeft(int rowIndex) {
        if(rowContainsFixedTile(rowIndex)) {
            return false;
        }

        Tile pushedOut = tileMap.getForward(new Position(rowIndex, 0));
        Tile first = tileMap.getForward(new Position(rowIndex, 0));

        for (int col = 0; col < width - 1; col++) {
            Tile from = tileMap.getForward(new Position(rowIndex, col + 1));
            tileMap.put(new Position(rowIndex, col), from);
        }

        tileMap.put(new Position(rowIndex, width - 1), extraTile);
        adjustPlayersOnPushedOutTile(pushedOut);
        extraTile = first;
        notifyListeners(BoardEventType.ROW_SHIFTED, rowIndex);
        return true;
    }

    protected boolean shiftRowRight(int rowIndex) {
        if(rowContainsFixedTile(rowIndex)) {
            return false;
        }

        Tile pushedOut = tileMap.getForward(new Position(rowIndex, width - 1));
        Tile last = tileMap.getForward(new Position(rowIndex, width - 1));

        for (int col = width - 1; col > 0; col--) {
            Tile from = tileMap.getForward(new Position(rowIndex, col - 1));
            tileMap.put(new Position(rowIndex, col), from);
        }

        tileMap.put(new Position(rowIndex, 0), extraTile);
        adjustPlayersOnPushedOutTile(pushedOut);
        extraTile = last;
        notifyListeners(BoardEventType.ROW_SHIFTED, rowIndex);
        return true;
    }

    public boolean colContainsFixedTile(int columnIndex) {
        for (int row = 0; row < height; row++) {
            Tile tile = tileMap.getForward(new Position(row, columnIndex));
            if (tile.isFixed() && !freeRoam) {
                System.out.println("Column " + columnIndex + " contains fixed tiles. Cannot shift.");
                return true;
            }
        }
        return false;
    }

    public boolean rowContainsFixedTile(int rowIndex) {
        for (int col = 0; col < width; col++) {
            Tile tile = tileMap.getForward(new Position(rowIndex, col));
            if (tile.isFixed() && !freeRoam) {
                System.out.println("Row " + rowIndex + " contains fixed tiles. Cannot shift.");
                return true;
            }
        }
        return false;
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

    private void adjustPlayersOnPushedOutTile(Tile pushedOutTile) {
        for (Player player : players) {
            if (player.getCurrentTile() == pushedOutTile) {
                player.setCurrentTile(extraTile);
            }
        }
    }


    // Obserser stuff, refactor later
    private final List<IBoardEventListener> listeners = new ArrayList<>();

    public void addListener(IBoardEventListener listener) {
        listeners.add(listener);
    }

    public void removeListener(IBoardEventListener listener) {
        listeners.remove(listener);
    }

    private void notifyListeners(BoardEventType type, int index) {
        var event = new BoardEvent(type, index);
        for (IBoardEventListener listener : listeners) {
            listener.onBoardEvent(event);
        }
    }
}
