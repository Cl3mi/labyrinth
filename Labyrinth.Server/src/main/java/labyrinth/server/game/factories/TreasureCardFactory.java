package labyrinth.server.game.factories;

import labyrinth.server.game.abstractions.ITreasureCardFactory;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.TreasureCard;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * Factory class to create TreasureCards for the game.
 * Maintains a predefined set of treasure names.
 */
public class TreasureCardFactory implements ITreasureCardFactory {

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

    @Override
    public List<TreasureCard> createCardsForGame(Game game) {
        var playerCount = game.getPlayers().size();
        var treasuresToGenerate = game.getGameConfig().amountOfTreasuresPerPlayer() * playerCount;

        if (treasuresToGenerate < 1 || treasuresToGenerate > TREASURE_NAMES.size()) {
            throw new IllegalArgumentException("Count must be between 1 and " + TREASURE_NAMES.size()/playerCount);
        }

        List<String> shuffledNames = new ArrayList<>(TREASURE_NAMES);
        Collections.shuffle(shuffledNames);

        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < treasuresToGenerate; i++) {
            String name = shuffledNames.get(i);
            String id = UUID.randomUUID().toString();
            String imagePath = "/images/treasures/" + name.toLowerCase() + ".png";
            cards.add(new TreasureCard(id, name, imagePath));
        }

        return cards;
    }
}
