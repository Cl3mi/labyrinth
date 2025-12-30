package labyrinth.server.game.bonuses;

import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;

public class SwapBonusEffect implements IBonusEffect {
    @Override
    public boolean apply(Game game, Player player, Object... args) {
        if (args.length < 1 || !(args[0] instanceof Player)) {
            throw new IllegalArgumentException("SwapBonus requires targetPlayer argument");
        }
        Player targetPlayer = (Player) args[0];

        if (!player.useBonus(BonusTypes.SWAP)) {
            return false;
        }

        Tile currentPlayerTile = player.getCurrentTile();
        Tile targetPlayerTile = targetPlayer.getCurrentTile();

        player.setCurrentTile(targetPlayerTile);
        targetPlayer.setCurrentTile(currentPlayerTile);
        player.getStatistics().increaseScore(PointRewards.REWARD_BONUS_USED);

        return true;
    }
}
