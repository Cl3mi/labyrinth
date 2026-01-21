package labyrinth.client.models;

import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.PlayerColor;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * Client-seitiges Player-Modell für das Labyrinth-Spiel.
 * Entkoppelt von den Contracts (PlayerState), damit die UI frei ist.
 */
@Getter
@Setter
public class Player {

    private final String id;
    private final String name;


    /**
     * Aktuelle Position auf dem Brett (Zeile/Spalte im Client).
     */
    private Position currentPosition;

    /**
     * Start-/Heimatfeld des Spielers (falls vom Server geliefert).
     */
    private Position homePosition;

    /**
     * Optionaler Cache für erreichbare Tiles (nur UI-Hilfe).
     */
    private Set<Tile> reachableTiles = new HashSet<>();

    /**
     * Player color assigned by server
     */
    private PlayerColor color;

    /**
     * Treasures found by this player
     */
    private final List<Treasure> treasuresFound = new ArrayList<>();

    /**
     * Number of treasures remaining to find
     */
    private int remainingTreasureCount = 0;

    /**
     * Whether this player is connected to the server
     */
    private boolean isConnected = true;

    /**
     * Whether this player is the admin
     */
    private boolean isAdmin = false;

    /**
     * Whether this player is AI controlled
     */
    private boolean isAiControlled = false;

    /**
     * -- GETTER --
     *  Gets the current target treasure (first uncollected treasure).
     *  Returns null if all treasures have been collected.
     */
    private Treasure currentTargetTreasure;


    /**
     * Available bonuses for this player (from server)
     */
    private final List<BonusType> availableBonuses = new ArrayList<>();

    public Player(String id, String name) {
        this.id = Objects.requireNonNull(id, "id must not be null");
        this.name = Objects.requireNonNull(name, "name must not be null");
    }


    /**
     * Updates the available bonuses for this player.
     */
    public void setAvailableBonuses(List<BonusType> bonuses) {
        this.availableBonuses.clear();
        if (bonuses != null) {
            this.availableBonuses.addAll(bonuses);
        }
    }

    @Override
    public String toString() {
        return "Player{" +
                "id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", currentPosition=" + currentPosition +
                ", homePosition=" + homePosition +
                '}';
    }
}
