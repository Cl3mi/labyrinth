package labyrinth.server.game.bonuses;

import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.records.Position;

public class BeamBonusEffect implements IBonusEffect {
    @Override
    public boolean apply(Game game, Player player, Object... args) {
        if (args.length < 2 || !(args[0] instanceof Integer) || !(args[1] instanceof Integer)) {
            throw new IllegalArgumentException("BeamBonus requires row and col arguments");
        }
        int row = (Integer) args[0];
        int col = (Integer) args[1];

        Tile targetTile = game.getBoard().getTileAt(row, col);

        for (Player other : game.getPlayers()) {
            if (other != player && other.getCurrentTile() == targetTile) {
                // Cannot move if player is already there
                return false;
            }
        }

        // Fix: Use correct BonusType BEAM
        if (!player.useBonus(BonusTypes.BEAM)) {
            return false;
        }

        player.getStatistics().increaseScore(PointRewards.REWARD_BONUS_USED);
        player.setCurrentTile(targetTile);
        return true;
    }
}
