package labyrinth.server.game.bonuses;

import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;

public class PushFixedBonusEffect implements IBonusEffect {
    @Override
    public boolean apply(Game game, Player player, Object... args) {
        if (!player.useBonus(BonusTypes.PUSH_FIXED)) {
            return false;
        }

        game.setActiveBonus(BonusTypes.PUSH_FIXED);
        player.getStatistics().increaseScore(PointRewards.REWARD_BONUS_USED);
        return true;
    }
}
