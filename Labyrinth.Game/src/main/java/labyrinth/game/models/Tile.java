package labyrinth.game.models;

import labyrinth.game.enums.*;

import java.util.EnumSet;
import java.util.Set;

/**
 * Represents a single tile in the Labyrinth board.
 * Each tile has entrances in specific directions and may contain a treasure.
 */
public class Tile {

    private final EnumSet<Direction> entrances;
    private boolean hasTreasure;

    /**
     * Creates a tile with specified entrances and optional treasure.
     *
     * @param entrances    directions where the tile has openings (at least 2)
     * @param hasTreasure  whether the tile contains a treasure
     */
    public Tile(Set<Direction> entrances, boolean hasTreasure) {
        if (entrances.size() < 2) {
            throw new IllegalArgumentException("A tile must have at least 2 entrances.");
        }
        this.entrances = EnumSet.copyOf(entrances);
        this.hasTreasure = hasTreasure;
    }

    public Set<Direction> getEntrances() {
        return EnumSet.copyOf(entrances);
    }

    public boolean hasTreasure() {
        return hasTreasure;
    }

    public void setHasTreasure(boolean hasTreasure) {
        this.hasTreasure = hasTreasure;
    }

    /**
     * Rotates the tile 90 degrees clockwise.
     * Updates the entrances accordingly.
     */
    public void rotate() {
        EnumSet<Direction> rotated = EnumSet.noneOf(Direction.class);
        for (Direction dir : entrances) {
            rotated.add(switch (dir) {
                case UP -> Direction.RIGHT;
                case RIGHT -> Direction.DOWN;
                case DOWN -> Direction.LEFT;
                case LEFT -> Direction.UP;
            });
        }
        entrances.clear();
        entrances.addAll(rotated);
    }

    /**
     * Checks if this tile is connected to another tile.
     * Two tiles are connected if they have matching entrances in opposite directions.
     *
     * @param neighbor the neighboring tile to check connection with
     * @param direction the direction from this tile to the neighbor
     * @return true if connected, false otherwise
     */
    public boolean isConnectedTo(Tile neighbor, Direction direction) {
        return entrances.contains(direction) && neighbor.getEntrances().contains(direction.opposite());
    }

    @Override
    public String toString() {
        return "Tile{" +
                "entrances=" + entrances +
                ", hasTreasure=" + hasTreasure +
                '}';
    }
}