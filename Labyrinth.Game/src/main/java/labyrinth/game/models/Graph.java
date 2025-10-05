package labyrinth.game.models;

import java.util.*;
import labyrinth.game.enums.*;

/**
 * Represents the connectivity graph of the Labyrinth board.
 * Each tile can have neighbors it is directly connected to.
 */
public class Graph {

    private final Map<Tile, Set<Tile>> adjacencyList;

    /**
     * Creates an empty graph.
     */
    public Graph() {
        this.adjacencyList = new HashMap<>();
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
}