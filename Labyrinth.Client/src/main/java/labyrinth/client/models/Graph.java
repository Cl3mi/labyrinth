package labyrinth.client.models;

import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;

import java.util.*;

/**
 * Repräsentiert die Konnektivität des Labyrinth-Bretts als Graph.
 * Arbeitet mit contracts.Tile und deren entrances[].
 */
public class Graph {

    private final Map<Tile, Set<Tile>> adjacencyList;

    public Graph() {
        this.adjacencyList = new HashMap<>();
    }

    public void addTile(Tile tile) {
        adjacencyList.putIfAbsent(tile, new HashSet<>());
    }

    public void clear() {
        adjacencyList.clear();
    }

    /**
     * Verbindet zwei Tiles, wenn sie über ihre Entrances in der angegebenen Richtung verbunden sind.
     *
     * @param tile1     Starttile
     * @param tile2     Nachbartile
     * @param direction Richtung von tile1 nach tile2
     */
    public void connect(Tile tile1, Tile tile2, Direction direction) {
        if (tile1 == null || tile2 == null) return;

        if (areConnected(tile1, tile2, direction)) {
            adjacencyList.computeIfAbsent(tile1, k -> new HashSet<>()).add(tile2);
            adjacencyList.computeIfAbsent(tile2, k -> new HashSet<>()).add(tile1);
        }
    }

    private boolean areConnected(Tile t1, Tile t2, Direction dir) {
        Direction[] e1 = t1.getEntrances();
        Direction[] e2 = t2.getEntrances();

        if (e1 == null || e2 == null) return false;

        Direction opposite = switch (dir) {
            case UP -> Direction.DOWN;
            case DOWN -> Direction.UP;
            case LEFT -> Direction.RIGHT;
            case RIGHT -> Direction.LEFT;
        };

        boolean t1HasDir = hasEntrance(e1, dir);
        boolean t2HasOpp = hasEntrance(e2, opposite);

        return t1HasDir && t2HasOpp;
    }

    private boolean hasEntrance(Direction[] entrances, Direction dir) {
        if (entrances == null) return false;
        for (Direction d : entrances) {
            if (d == dir) return true;
        }
        return false;
    }

    public Set<Tile> getNeighbors(Tile tile) {
        return adjacencyList.getOrDefault(tile, Collections.emptySet());
    }

    /**
     * BFS über den Graphen, um alle erreichbaren Tiles zu finden.
     */
    public Set<Tile> findReachable(Tile start) {
        Set<Tile> visited = new HashSet<>();
        if (start == null) return visited;

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
     * Kürzester Pfad von start zu target über BFS.
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

        List<Tile> path = new ArrayList<>();
        Tile current = target;
        while (current != null && parentMap.containsKey(current)) {
            path.add(current);
            current = parentMap.get(current);
        }
        Collections.reverse(path);

        if (path.isEmpty() || !path.get(0).equals(start)) {
            return Collections.emptyList();
        }
        return path;
    }
}
