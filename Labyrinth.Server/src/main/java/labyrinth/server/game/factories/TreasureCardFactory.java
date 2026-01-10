package labyrinth.server.game.factories;

import labyrinth.server.game.models.TreasureCard;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Factory class to create TreasureCards for the game.
 * Maintains a predefined set of treasure names.
 */
@Component
public class TreasureCardFactory {

    private static final List<String> TREASURE_NAMES = List.of(
            "Geist",       // Ghost
            "Drache",      // Dragon
            "Hexe",        // Witch
            "Eule",        // Owl
            "Ratte",       // Rat
            "Käfer",       // Bug
            "Spinne",      // Spider
            "Schlange",    // Snake
            "Fledermaus",  // Bat
            "Krone",       // Crown
            "Schlüssel",   // Key
            "Schatztruhe", // Treasure chest
            "Helm",        // Helmet (German)
            "Buch",        // Book
            "Kerze",       // Candle
            "Ring",
            "Beutel",      // Bag
            "Totenkopf",   // Skull
            "Karte",       // Map
            "Schwert",
            "Kelch",       // Chalice
            "Edelstein",   // Gemstone
            "Krug",        // Jug
            "Maus"         // Mouse
    );

    /**
     * Creates treasure cards for competitive mode where all players compete for the same treasures.
     * If more treasures are needed than available names, names will be reused with numeric suffixes.
     *
     * @param treasureCardCount Number of treasures to create
     * @return List of treasure cards
     */
    public List<TreasureCard> createTreasureCards(int treasureCardCount) {
        if (treasureCardCount <= 0) {
            throw new IllegalArgumentException("Treasure card count must be at least 1");
        }

        List<String> shuffledNames = new ArrayList<>(TREASURE_NAMES);
        Collections.shuffle(shuffledNames);

        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < treasureCardCount; i++) {
            // Cycle through treasure names, adding suffix if we exceed available names
            int nameIndex = i % shuffledNames.size();
            String baseName = shuffledNames.get(nameIndex);

            // Add numeric suffix if we're reusing names (e.g., "Crown #2")
            String name = baseName;
            if (i >= shuffledNames.size()) {
                int copyNumber = (i / shuffledNames.size()) + 1;
                name = baseName + " #" + copyNumber;
            }

            String imagePath = "/images/treasures/" + baseName.toLowerCase() + ".png";
            cards.add(new TreasureCard(i, name, imagePath));
        }

        return cards;
    }

    /**
     * Legacy method for creating treasure cards distributed to players.
     * Kept for backward compatibility.
     */
    public List<TreasureCard> createTreasureCards(int treasureCardCount, int playerCount) {

        if (treasureCardCount % playerCount != 0) {
            throw new IllegalArgumentException("Treasure card count must be divisible by player count.");
        }

        List<String> shuffledNames = new ArrayList<>(TREASURE_NAMES);
        Collections.shuffle(shuffledNames);

        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < treasureCardCount; i++) {
            String name = shuffledNames.get(i);
            String imagePath = "/images/treasures/" + name.toLowerCase() + ".png";
            cards.add(new TreasureCard(i, name, imagePath));
        }

        return cards;
    }
}
