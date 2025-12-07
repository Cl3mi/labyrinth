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
            "Crown", "Jewel", "Goblet", "Ring", "Coin",
            "Scepter", "Amulet", "Diamond", "Emerald", "Ruby",
            "Pearl", "Chalice", "Scroll", "Helmet", "Sword",
            "Shield", "Lantern", "Key", "Statue", "Potion",
            "Gemstone", "Mask", "Orb", "Book", "Cloak",
            "Staff", "Banner", "Bracelet", "Necklace", "Tiara",
            "Choker", "Earring", "Medallion", "CoinPouch", "Seal",
            "Carving", "Figurine", "Crystal", "Talisman", "RingBox"
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
