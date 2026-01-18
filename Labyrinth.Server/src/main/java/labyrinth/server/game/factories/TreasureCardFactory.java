package labyrinth.server.game.factories;

import labyrinth.server.game.models.TreasureCard;
import org.springframework.stereotype.Component;

import java.util.*;

import static java.util.Map.entry;

/**
 * Factory class to create TreasureCards for the game.
 * Maintains a predefined set of treasure names.
 */
@Component
public class TreasureCardFactory {

    private static final HashMap<Integer, String> treasures = new HashMap<>(Map.ofEntries(
            entry(1, "Geist"),
            entry(2, "Drache"),
            entry(3, "Hexe"),
            entry(4, "Eule"),
            entry(5, "Ratte"),
            entry(6, "Käfer"),
            entry(7, "Spinne"),
            entry(8, "Schlange"),
            entry(9, "Fledermaus"),
            entry(10, "Krone"),
            entry(11, "Schlüssel"),
            entry(12, "Schatztruhe"),
            entry(13, "Helm"),
            entry(14, "Buch"),
            entry(15, "Kerze"),
            entry(16, "Ring"),
            entry(17, "Beutel"),
            entry(18, "Totenkopf"),
            entry(19, "Karte"),
            entry(20, "Schwert"),
            entry(21, "Kelch"),
            entry(22, "Edelstein"),
            entry(23, "Krug"),
            entry(24, "Maus")
    ));



    /**
     * Legacy method for creating treasure cards distributed to players.
     * Kept for backward compatibility.
     */
    public List<TreasureCard> createTreasureCards(int treasureCardCount, int playerCount) {
        if (treasureCardCount % playerCount != 0) {
            throw new IllegalArgumentException("Treasure card count must be divisible by player count.");
        }

        List<Map.Entry<Integer, String>> shuffledTreasures = new ArrayList<>(treasures.entrySet());
        Collections.shuffle(shuffledTreasures);

        List<TreasureCard> cards = new ArrayList<>();
        for(Map.Entry<Integer, String> treasure : shuffledTreasures) {
            String imagePath = "/images/treasures/" + treasure.getValue().toLowerCase() + ".png";
            cards.add(new TreasureCard(treasure.getKey(), treasure.getValue(), imagePath));
        }

        return cards;
    }
}
