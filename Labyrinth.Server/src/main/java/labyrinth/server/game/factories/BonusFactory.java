package labyrinth.server.game.factories;

import labyrinth.server.game.enums.BonusTypes;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

@Component
public class BonusFactory {

    private final Random random = new Random();

    public List<BonusTypes> createBonuses(int count) {
        List<BonusTypes> bonuses = new ArrayList<>();
        BonusTypes[] allTypes = BonusTypes.values();

        for (int i = 0; i < count; i++) {
            bonuses.add(allTypes[random.nextInt(allTypes.length)]);
        }

        return bonuses;
    }
}
