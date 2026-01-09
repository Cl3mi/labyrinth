package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.enums.BonusTypes;
import lombok.AccessLevel;
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
 *
 * Encapsulation notes:
 * - Collections return defensive copies to prevent external mutation
 * - Tile references are package-private setters (only Board should set)
 * - Admin/AI flags use explicit methods instead of generic setters
 */
@Getter
public class Player {
    private final UUID id;
    private final String username;

    @Setter
    private OffsetDateTime joinDate;

    private boolean isAiActive;
    private boolean isAdmin;

    @Setter
    private PlayerColor color;

    @Setter
    private PlayerStatistics statistics;

    private final List<TreasureCard> assignedTreasureCards;
    private final List<BonusTypes> bonuses;

    /**
     * The tile on which the player is currently standing.
     * Updated by Board, MovementManager, and bonus effects.
     */
    @Setter
    private Tile currentTile;

    /**
     * The player's home/starting tile. Set during game initialization.
     */
    @Setter
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

    // ========== Collection Accessors with Defensive Copies ==========

    /**
     * Returns a defensive copy of assigned treasure cards.
     * External modifications will not affect the player's internal state.
     *
     * @return immutable view of treasure cards
     */
    public List<TreasureCard> getAssignedTreasureCards() {
        return List.copyOf(assignedTreasureCards);
    }

    /**
     * Returns a defensive copy of bonuses.
     * External modifications will not affect the player's internal state.
     *
     * @return immutable view of bonuses
     */
    public List<BonusTypes> getBonuses() {
        return List.copyOf(bonuses);
    }

    /**
     * Internal method for adding treasure cards.
     * Should only be called by GameInitializer during game setup.
     *
     * @param card the treasure card to add
     */
    public void addTreasureCard(TreasureCard card) {
        assignedTreasureCards.add(Objects.requireNonNull(card));
    }

    /**
     * Internal method for adding bonuses.
     * Should only be called by MovementManager when player collects a bonus.
     *
     * @param bonusType the bonus to add
     */
    public void addBonus(BonusTypes bonusType) {
        bonuses.add(Objects.requireNonNull(bonusType));
    }

    // ========== State Transition Methods ==========

    /**
     * Sets the AI active status. Use explicit method instead of generic setter.
     *
     * @param aiActive true if this player should be controlled by AI
     */
    public void setAiActive(boolean aiActive) {
        this.isAiActive = aiActive;
    }

    /**
     * Sets admin status for this player.
     * Should only be called by PlayerRegistry or tests.
     *
     * @param admin true to make this player an admin
     */
    public void setAdmin(boolean admin) {
        this.isAdmin = admin;
    }

    // ========== Game Logic Methods ==========

    public Player copy() {
        Player newPlayer = new Player(this.id, this.username);
        newPlayer.setJoinDate(this.joinDate);
        newPlayer.setStatistics(this.statistics);
        newPlayer.setAiActive(this.isAiActive);
        newPlayer.setAdmin(this.isAdmin);
        newPlayer.setColor(this.color);

        // Direct access to internal list for copying
        newPlayer.assignedTreasureCards.addAll(this.assignedTreasureCards);
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