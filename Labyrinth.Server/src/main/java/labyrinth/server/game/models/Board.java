package labyrinth.server.game.models;

import labyrinth.server.game.abstractions.IBoardEventListener;
import labyrinth.server.game.enums.*;
import labyrinth.server.game.events.BoardEvent;
import labyrinth.server.game.models.records.Position;
import labyrinth.server.game.services.MovementManager;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * Represents the game board for the Labyrinth game.
 * The layout of tiles is stored in a bidirectional map between {@link Position}
 * instances and {@link Tile} instances. A separate graph tracks the
 * connectivity
 * of neighboring tiles based on their entrances.
 */
@Getter
public class Board {

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(Board.class.getName());
    private final int width;
    private final int height;
    private final BiMap<Position, Tile> tileMap;
    private final Graph graph;
    private Tile extraTile;

    @Setter
    private List<Player> players;
    @Setter
    private boolean freeRoam = false;

    /**
     * Creates a board with the given dimensions. Although the constructor accepts a
     * 2D
     * array of tiles for backwards compatibility, it immediately populates a
     * {@link BiMap} mapping each {@link Position} to its associated {@link Tile}.
     * The
     * original array reference is not stored internally.
     *
     * @param width     number of columns
     * @param height    number of rows
     * @param tileMap   the bidirectional tilemap
     * @param extraTile the spare tile which will be inserted during shifts
     */
    public Board(int width, int height, BiMap<Position, Tile> tileMap, Tile extraTile) {
        if (width < 3 || width > 11) {
            throw new IllegalArgumentException("Board width must be between 3 and 11");
        }
        if (height < 3 || height > 11) {
            throw new IllegalArgumentException("Board height must be between 3 and 11");
        }
        if (width % 2 == 0) {
            throw new IllegalArgumentException("Board width must be odd");
        }
        if (height % 2 == 0) {
            throw new IllegalArgumentException("Board height must be odd");
        }

        this.width = width;
        this.height = height;
        this.tileMap = tileMap;
        this.graph = new Graph(this);
        this.addListener(this.graph);
        this.extraTile = extraTile;
    }

    public Board copy() {
        BiMap<Position, Tile> newTileMap = this.tileMap.copy(p -> p, Tile::copy);

        Tile newExtraTile = this.extraTile.copy();

        Board newBoard = new Board(this.width, this.height, newTileMap, newExtraTile);

        newBoard.setFreeRoam(this.freeRoam);

        if (this.players != null) {
            List<Player> newPlayers = new ArrayList<>();
            for (Player p : this.players) {
                Player newP = p.copy();
                if (p.getCurrentTile() != null) {
                    Position pos = this.getPositionOfTile(p.getCurrentTile());
                    if (pos != null) {
                        Tile newTile = newTileMap.getForward(pos);
                        newP.setCurrentTile(newTile);
                    }
                }
                if (p.getHomeTile() != null) {
                    Position homePos = this.getPositionOfTile(p.getHomeTile());
                    if (homePos != null) {
                        Tile newHomeTile = newTileMap.getForward(homePos);
                        newP.setHomeTile(newHomeTile);
                    }
                }
                newPlayers.add(newP);
            }
            newBoard.setPlayers(newPlayers);
        }

        return newBoard;
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
     * Retrieves the tile at the specified coordinates.
     *
     * @param position the position
     * @return the tile located at (row, column), or null if not present
     */
    public Tile getTileAt(Position position) {
        return tileMap.getForward(position);
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

    public boolean shiftColumnDown(int columnIndex, boolean fixedBonusActive) {
        // Never allow pushing the outer columns (home positions)
        if (columnIndex == 0 || columnIndex == width - 1) {
            return false;
        }

        if (colContainsFixedTile(columnIndex) && !fixedBonusActive) {
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

    public boolean shiftColumnUp(int columnIndex, boolean fixedBonusActive) {
        // Never allow pushing the outer columns (home positions)
        if (columnIndex == 0 || columnIndex == width - 1) {
            return false;
        }

        if (colContainsFixedTile(columnIndex) && !fixedBonusActive) {
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

    public boolean shiftRowLeft(int rowIndex, boolean fixedBonusActive) {
        // Never allow pushing the outer rows (home positions)
        if (rowIndex == 0 || rowIndex == height - 1) {
            return false;
        }

        if (rowContainsFixedTile(rowIndex) && !fixedBonusActive) {
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

    public boolean shiftRowRight(int rowIndex, boolean fixedBonusActive) {
        // Never allow pushing the outer rows (home positions)
        if (rowIndex == 0 || rowIndex == height - 1) {
            return false;
        }

        if (rowContainsFixedTile(rowIndex) && !fixedBonusActive) {
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
                LOGGER.info("Column " + columnIndex + " contains fixed tiles. Cannot shift.");
                return true;
            }
        }
        return false;
    }

    public boolean rowContainsFixedTile(int rowIndex) {
        for (int col = 0; col < width; col++) {
            Tile tile = tileMap.getForward(new Position(rowIndex, col));
            if (tile.isFixed() && !freeRoam) {
                LOGGER.info("Row " + rowIndex + " contains fixed tiles. Cannot shift.");
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

    public boolean isCornerCoordinate(int row, int col) {
        boolean isTopLeft = (row == 0 && col == 0);
        boolean isTopRight = (row == 0 && col == this.width - 1);
        boolean isBottomLeft = (row == this.height - 1 && col == 0);
        boolean isBottomRight = (row == this.height - 1 && col == this.width - 1);

        return isTopLeft || isTopRight || isBottomLeft || isBottomRight;
    }

    int movePlayerToTile(Player player, int targetRow, int targetCol, MovementManager movementManager) {
        Tile currentTile = player.getCurrentTile();
        Tile targetTile = tileMap.getForward(new Position(targetRow, targetCol));

        Position currPos = (currentTile != null) ? getPositionOfTile(currentTile) : null;
        LOGGER.info("Current position: " + (currPos != null ? currPos.row() + "/" + currPos.column() : "none"));
        LOGGER.info("Moving " + player.getUsername() + " to " + targetRow + "/" + targetCol);

        if (movementManager.isTileBlockedByPlayer(targetTile, players, player)) {
            LOGGER.info("Cant move - a player is already on the target tile!");
            return -1;
        }

        Set<Tile> reachable = getReachableTiles(player);
        if (!reachable.contains(targetTile)) {
            LOGGER.info("Tile is not reachable!");
            return -1;
        }

        movementManager.processPlayerStepOnTile(player, targetTile);
        var distance = graph.getDistance(currentTile, targetTile);

        Position newPos = getPositionOfTile(targetTile);
        LOGGER.info("Player moved to " + (newPos != null ? newPos : "unknown"));

        return distance;
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
