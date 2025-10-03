package labyrinth.game.factories;

import labyrinth.game.models.TreasureCard;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * Factory class to create TreasureCards for the game.
 * Maintains a predefined set of treasure names.
 */
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

    /**
     * Creates a list of randomly selected TreasureCards.
     *
     * @param count number of cards to create
     * @return list of TreasureCards
     */
    public static List<TreasureCard> createRandomCards(int count) {
        if (count < 1 || count > TREASURE_NAMES.size()) {
            throw new IllegalArgumentException("Count must be between 1 and " + TREASURE_NAMES.size());
        }

        List<String> shuffledNames = new ArrayList<>(TREASURE_NAMES);
        Collections.shuffle(shuffledNames);

        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            String name = shuffledNames.get(i);
            String id = UUID.randomUUID().toString();
            String imagePath = "/images/treasures/" + name.toLowerCase() + ".png";
            cards.add(new TreasureCard(id, name, imagePath));
        }

        return cards;
    }
}
