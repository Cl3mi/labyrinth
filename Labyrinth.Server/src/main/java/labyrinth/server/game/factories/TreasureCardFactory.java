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
            "Beutel",      // Bag
            "Totenkopf",   // Skull
            "Karte",       // Map
            "Kelch",       // Chalice
            "Edelstein",   // Gemstone
            "Krug",        // Jug
            "Maus"         // Mouse
    );

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
