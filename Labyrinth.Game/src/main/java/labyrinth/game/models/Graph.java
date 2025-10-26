package labyrinth.game.models;

import java.io.Serializable;
import java.util.*;

import labyrinth.game.abstractions.IBoardEventListener;
import labyrinth.game.enums.*;
import labyrinth.game.events.BoardEvent;

/**
 * Represents the connectivity graph of the Labyrinth board.
 * Each tile can have neighbors it is directly connected to.
 */
public class Graph implements IBoardEventListener {

    private final Map<Tile, Set<Tile>> adjacencyList;
    private final Board board;

    /**
     * Creates an empty graph.
     */
    public Graph(Board board) {
        this.board = board;
        this.adjacencyList = new HashMap<>();
        initializeGraph();
    }

    /**
     * Adds a tile to the graph.
     *
     * @param tile the tile to add
     */
    public void addTile(Tile tile) {
        adjacencyList.putIfAbsent(tile, new HashSet<>());
    }

    /**
     * Clears all tiles and connections from the graph.
     */
    public void clear() {
        adjacencyList.clear();
    }

    /**
     * Connects two tiles bidirectionally if they are connected.
     *
     * @param tile1 first tile
     * @param tile2 second tile
     * @param direction direction from tile1 to tile2
     */
    public void connect(Tile tile1, Tile tile2, Direction direction) {
        if (tile1.isConnectedTo(tile2, direction)) {
            adjacencyList.computeIfAbsent(tile1, k -> new HashSet<>()).add(tile2);
            adjacencyList.computeIfAbsent(tile2, k -> new HashSet<>()).add(tile1);
        }
    }

    /**
     * Returns the neighboring tiles directly connected to the given tile.
     *
     * @param tile the tile
     * @return set of neighboring tiles
     */
    public Set<Tile> getNeighbors(Tile tile) {
        return adjacencyList.getOrDefault(tile, Collections.emptySet());
    }

    /**
     * Finds all tiles reachable from the start tile using BFS.
     *
     * @param start the starting tile
     * @return set of reachable tiles
     */
    public Set<Tile> findReachable(Tile start) {
        Set<Tile> visited = new HashSet<>();
        Queue<Tile> queue = new ArrayDeque<>();
        queue.add(start);
        visited.add(start);

        while (!queue.isEmpty()) {
            Tile current = queue.poll();
            for (Tile neighbor : getNeighbors(current)) {
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    queue.add(neighbor);
                }
            }
        }
        return visited;
    }

    /**
     * Finds the shortest path from start to target using BFS.
     *
     * @param start  starting tile
     * @param target target tile
     * @return list of tiles forming the path, or empty list if no path exists
     */
    public List<Tile> findPath(Tile start, Tile target) {
        Map<Tile, Tile> parentMap = new HashMap<>();
        Queue<Tile> queue = new ArrayDeque<>();
        Set<Tile> visited = new HashSet<>();

        queue.add(start);
        visited.add(start);
        parentMap.put(start, null);

        while (!queue.isEmpty()) {
            Tile current = queue.poll();
            if (current.equals(target)) {
                break;
            }
            for (Tile neighbor : getNeighbors(current)) {
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    parentMap.put(neighbor, current);
                    queue.add(neighbor);
                }
            }
        }

        // Reconstruct path
        List<Tile> path = new ArrayList<>();
        Tile current = target;
        while (current != null && parentMap.containsKey(current)) {
            path.add(current);
            current = parentMap.get(current);
        }
        Collections.reverse(path);
        if (path.isEmpty() || !path.get(0).equals(start)) {
            return Collections.emptyList(); // no path found
        }
        return path;
    }

    @Override
    public void onBoardEvent(BoardEvent event) {
        switch (event.type()) {
            case ROW_SHIFTED -> updateAfterRowShift(event.index());
            case COLUMN_SHIFTED -> updateAfterColumnShift(event.index());
        }
    }

    private void updateAfterRowShift(int rowIndex) {
        for (int col = 0; col < board.getWidth(); col++) {
            updateTileConnectionsAt(rowIndex, col);
            if (rowIndex > 0) updateTileConnectionsAt(rowIndex - 1, col);
            if (rowIndex < board.getHeight() - 1) updateTileConnectionsAt(rowIndex + 1, col);
        }
    }

    private void updateAfterColumnShift(int colIndex) {
        for (int row = 0; row < board.getHeight(); row++) {
            updateTileConnectionsAt(row, colIndex);
            if (colIndex > 0) updateTileConnectionsAt(row, colIndex - 1);
            if (colIndex < board.getWidth() - 1) updateTileConnectionsAt(row, colIndex + 1);
        }
    }

    private void updateTileConnectionsAt(int row, int col) {
        Tile tile = board.getTileAt(row, col);

        // Remove old connections
        adjacencyList.getOrDefault(tile, Collections.emptySet())
                .forEach(n -> adjacencyList.get(n).remove(tile));
        adjacencyList.computeIfAbsent(tile, k -> new HashSet<>()).clear();

        // Try to reconnect with neighbors
        if (row > 0) connect(tile, board.getTileAt(row - 1, col), Direction.UP);
        if (row < board.getHeight() - 1) connect(tile, board.getTileAt(row + 1, col), Direction.DOWN);
        if (col > 0) connect(tile, board.getTileAt(row, col - 1), Direction.LEFT);
        if (col < board.getWidth() - 1) connect(tile, board.getTileAt(row, col + 1), Direction.RIGHT);
    }

    public void initializeGraph() {
        clear();

        var height = board.getHeight();
        var width = board.getWidth();
        var tileMap = board.getTileMap();
        
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile = tileMap.getForward(new Position(row, col));
                addTile(tile);

                // Connect with neighbors if entrances match
                if (row > 0) {
                    Tile upNeighbor = tileMap.getForward(new Position(row - 1, col));
                    connect(tile, upNeighbor, Direction.UP);
                }
                if (row < height - 1) {
                    Tile downNeighbor = tileMap.getForward(new Position(row + 1, col));
                    connect(tile, downNeighbor, Direction.DOWN);
                }
                if (col > 0) {
                    Tile leftNeighbor = tileMap.getForward(new Position(row, col - 1));
                    connect(tile, leftNeighbor, Direction.LEFT);
                }
                if (col < width - 1) {
                    Tile rightNeighbor = tileMap.getForward(new Position(row, col + 1));
                    connect(tile, rightNeighbor, Direction.RIGHT);
                }
            }
        }
        System.out.println("Graph initialized");
    }
}