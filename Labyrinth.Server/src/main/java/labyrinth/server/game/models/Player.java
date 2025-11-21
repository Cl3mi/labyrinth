package labyrinth.server.game.models;

import jdk.jshell.spi.ExecutionControl;

import java.util.*;

/**
 * Represents a player in the Labyrinth game.
 * Each player has a unique ID, a name, a list of assigned treasure cards,
 * and a current tile on the board.
 */
public class Player {

    private final String id;
    private final String name;
    private final List<TreasureCard> assignedTreasureCards;
    /**
     * The tile on which the player is currently standing. The board is
     * responsible for updating this reference when the player moves.
     */
    private Tile currentTile;

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
        this.currentTile = null;
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


    /**
     * Returns the tile on which this player is currently standing.
     *
     * @return the current tile, or null if the player is not placed on the board
     */
    public Tile getCurrentTile() {
        return currentTile;
    }

    /**
     * Sets the tile on which this player is standing. This should only be
     * invoked by the board when it moves players or reassigns their
     * positions.  The board is responsible for updating this reference
     * whenever a player moves on the board.
     *
     * @param tile the tile on which the player now stands
     */
    public void setCurrentTile(Tile tile) {
        this.currentTile = tile;
    }

    @Override
    public String toString() {
        return "Player{" +
                "id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", currentTile=" + (currentTile != null ? currentTile : "null") +
                ", treasures=" + assignedTreasureCards +
                '}';
    }
}