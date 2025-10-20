package labyrinth.game.models;

import jdk.jshell.spi.ExecutionControl;

import java.util.*;

/**
 * Represents a player in the Labyrinth game.
 * Each player has a unique ID, a name, a list of assigned treasure cards,
 * and a current position on the board.
 */
public class Player {

    private final String id;
    private final String name;
    private final List<TreasureCard> assignedTreasureCards;
    private Position currentPosition;
    private Set<Tile> reachableTiles; // Optional cache for reachable tiles

    /**
     * Creates a new player with a given ID, name, and list of treasure cards.
     *
     * @param id                    unique identifier
     * @param name                  player name
     */
    public Player(String id, String name) {
        this.id = Objects.requireNonNull(id);
        this.name = Objects.requireNonNull(name);
        this.assignedTreasureCards = new ArrayList<>();
        this.currentPosition = null;
        this.reachableTiles = null;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public List<TreasureCard> getAssignedTreasureCards() {
        return assignedTreasureCards;
    }

    public Position getCurrentPosition() {
        return currentPosition;
    }

    public void setCurrentPosition(Position currentPosition) {
        this.currentPosition = currentPosition;
    }

    public Set<Tile> getReachableTiles() {
        return reachableTiles == null ? Set.of() : new HashSet<>(reachableTiles);
    }

    public void setReachableTiles(Set<Tile> reachableTiles) {
        this.reachableTiles = new HashSet<>(reachableTiles);
    }

    /**
     * Moves the player to the specified tile.
     *
     * @param tile the tile to move to
     */
    public void moveTo(Tile tile) throws ExecutionControl.NotImplementedException {
        throw new ExecutionControl.NotImplementedException("Not implemented yet");
    }
    /**
     * Collects the treasure if the player is on a tile with a matching treasure card.
     */
    public void collectTreasure() throws ExecutionControl.NotImplementedException {
        throw new ExecutionControl.NotImplementedException("Not implemented yet");
    }

    @Override
    public String toString() {
        return "Player{" +
                "id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", currentPosition=" + currentPosition +
                ", treasures=" + assignedTreasureCards +
                '}';
    }
}