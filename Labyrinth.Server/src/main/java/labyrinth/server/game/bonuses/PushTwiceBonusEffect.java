package labyrinth.server.game.bonuses;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;

public class PushTwiceBonusEffect implements IBonusEffect {
    @Override
    public boolean apply(Game game, Player player, Object... args) {
        if (!player.useBonus(BonusTypes.PUSH_TWICE)) {
            return false;
        }

        game.setActiveBonus(BonusTypes.PUSH_TWICE);
        return true;
    }
}
