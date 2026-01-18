package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.enums.BonusTypes;
import lombok.Getter;
import lombok.Setter;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

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

    private boolean isAiActive;
    private boolean isDisconnected;
    private boolean isAdmin;
    private PlayerColor color;
    private PlayerStatistics statistics;

    private final List<TreasureCard> assignedTreasureCards;
    private final List<BonusTypes> bonuses;

    /**
     * The tile on which the player is currently standing. The board is
     * responsible for updating this reference when the player moves.
     */
    private Tile currentTile;
    private Tile homeTile;

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
        this.bonuses = new ArrayList<>();
        this.currentTile = null;
        this.statistics = new PlayerStatistics();
    }

    public Player copy() {
        Player newPlayer = new Player(this.id, this.username);
        newPlayer.setJoinDate(this.joinDate);
        newPlayer.setStatistics(this.statistics);
        newPlayer.setAiActive(this.isAiActive);
        newPlayer.setAdmin(this.isAdmin);
        newPlayer.setColor(this.color);

        newPlayer.getAssignedTreasureCards().addAll(this.assignedTreasureCards);
        return newPlayer;
    }

    public boolean useBonus(BonusTypes bonusType) {
        return bonuses.remove(bonusType);
    }

    public TreasureCard getCurrentTreasureCard() {
        for (TreasureCard card : assignedTreasureCards) {
            if (!card.isCollected()) {
                return card;
            }
        }
        return null;
    }

    /**
     * Determines if AI should perform moves for this player.
     * Returns true if the player is an AI bot OR if they are temporarily disconnected.
     */
    public boolean shouldMoveBePerformedByAi(){
        return isAiActive && isDisconnected;
    }
    /**
     * Resets the player's state for a new game.
     * Clears treasures, bonuses, tiles, and statistics while preserving
     * identity (id, username, color, admin status, AI status).
     */
    public void resetForNewGame() {
        this.assignedTreasureCards.clear();
        this.bonuses.clear();
        this.currentTile = null;
        this.homeTile = null;
        this.statistics = new PlayerStatistics();
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