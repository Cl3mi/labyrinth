package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import lombok.Getter;
import lombok.Setter;

import java.time.OffsetDateTime;
import java.util.*;

/**
 * Represents a player in the Labyrinth game.
 * Each player has a unique ID, a name, a list of assigned treasure cards,
 * and a current tile on the board.
 */
@Setter
@Getter
public class Player {
    private final UUID id;
    private final String username;

    private OffsetDateTime joinDate;
    private int score;
    private boolean isAiActive;
    private boolean isAdmin;
    private PlayerColor color;

    // AI State Memory
    private labyrinth.server.game.models.records.Position lastTurnPosition;
    private int turnsStuck = 0;
    private String lastShiftDescription;

    private final List<TreasureCard> assignedTreasureCards;

    /**
     * The tile on which the player is currently standing. The board is
     * responsible for updating this reference when the player moves.
     */
    private Tile currentTile;

    /**
     * Creates a new player with a given ID, name, and list of treasure cards.
     *
     * @param id       unique identifier
     * @param username player name
     */
    public Player(UUID id, String username) {
        this.id = Objects.requireNonNull(id);
        this.username = Objects.requireNonNull(username);
        this.assignedTreasureCards = new ArrayList<>();
        this.currentTile = null;
    }

    public Player copy() {
        Player newPlayer = new Player(this.id, this.username);
        newPlayer.setJoinDate(this.joinDate);
        newPlayer.setScore(this.score);
        newPlayer.setAiActive(this.isAiActive);
        newPlayer.setAdmin(this.isAdmin);
        newPlayer.setColor(this.color);
        // Copy AI State
        newPlayer.setLastTurnPosition(this.lastTurnPosition);
        newPlayer.setTurnsStuck(this.turnsStuck);
        newPlayer.setLastShiftDescription(this.lastShiftDescription);

        // Shallow copy list, contents are assumed immutable during simulation
        newPlayer.getAssignedTreasureCards().addAll(this.assignedTreasureCards);
        // currentTile is NOT set here, must be set by caller to point to the new
        // board's tiles
        return newPlayer;
    }

    // Explicit Getters/Setters for AI logic
    public labyrinth.server.game.models.records.Position getLastTurnPosition() {
        return lastTurnPosition;
    }

    public void setLastTurnPosition(labyrinth.server.game.models.records.Position lastTurnPosition) {
        this.lastTurnPosition = lastTurnPosition;
    }

    public int getTurnsStuck() {
        return turnsStuck;
    }

    public void setTurnsStuck(int turnsStuck) {
        this.turnsStuck = turnsStuck;
    }

    public String getLastShiftDescription() {
        return lastShiftDescription;
    }

    public void setLastShiftDescription(String lastShiftDescription) {
        this.lastShiftDescription = lastShiftDescription;
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
}