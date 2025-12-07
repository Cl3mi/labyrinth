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