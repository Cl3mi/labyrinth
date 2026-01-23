package labyrinth.client.models;

import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.PlayerColor;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

@Getter
@Setter
public class Player {

    private final String id;
    private final String name;
    private Position currentPosition;
    private Position homePosition;
    private Set<Tile> reachableTiles = new HashSet<>();
    private PlayerColor color;
    private final List<Treasure> treasuresFound = new ArrayList<>();
    private int remainingTreasureCount = 0;
    private boolean isConnected = true;
    private boolean isAdmin = false;
    private boolean isAiControlled = false;
    private Treasure currentTargetTreasure;

    private final List<BonusType> availableBonuses = new ArrayList<>();

    public Player(String id, String name) {
        this.id = Objects.requireNonNull(id, "id must not be null");
        this.name = Objects.requireNonNull(name, "name must not be null");
    }



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
