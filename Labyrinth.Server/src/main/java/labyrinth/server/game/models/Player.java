package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;

import java.time.OffsetDateTime;
import java.util.*;

/**
 * Represents a player in the Labyrinth game.
 * Each player has a unique ID, a name, a list of assigned treasure cards,
 * and a current tile on the board.
 */
public class Player {
    private final UUID id;
    private final String username;

    private OffsetDateTime joinDate;
    private int score;
    private boolean isAiActive;
    private boolean isAdmin;
    private PlayerColor color;

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
     * @param username                  player name
     */
    public Player(UUID id, String username) {
        this.id = Objects.requireNonNull(id);
        this.username = Objects.requireNonNull(username);
        this.assignedTreasureCards = new ArrayList<>();
        this.currentTile = null;
    }

    public UUID getId() {
        return id;
    }

    public String getUsername() {
        return username;
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
                ", name='" + username + '\'' +
                ", currentTile=" + (currentTile != null ? currentTile : "null") +
                ", treasures=" + assignedTreasureCards +
                '}';
    }

    public void setJoinDate(OffsetDateTime joinDate) {
        this.joinDate = joinDate;
    }

    public void setScore(int score) {
        this.score = score;
    }

    public void setAiActive(boolean aiActive) {
        isAiActive = aiActive;
    }

    public void setAdmin(boolean admin) {
        isAdmin = admin;
    }

    public void setColor(PlayerColor color) {
        this.color = color;
    }

    public OffsetDateTime getJoinDate() {
        return joinDate;
    }

    public int getScore() {
        return score;
    }

    public boolean isAiActive() {
        return isAiActive;
    }

    public boolean isAdmin() {
        return isAdmin;
    }

    public PlayerColor getColor() {
        return color;
    }
}