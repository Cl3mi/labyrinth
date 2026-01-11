package labyrinth.server.game.services;

import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.results.TileInteractionResult;

import java.util.logging.Logger;

/**
 * Manages player movement and tile interaction effects.
 * Responsible for applying the results of stepping on tiles (collecting
 * treasures, bonuses)
 * while keeping the Tile class as a pure data entity.
 */
public class MovementManager {

    private static final Logger LOGGER = Logger.getLogger(MovementManager.class.getName());

    /**
     * Processes a player stepping onto a tile. Returns what was collected
     * and applies all side effects (scoring, collecting items, updating player
     * position).
     *
     * @param player the player moving onto the tile
     * @param tile   the tile being stepped on
     * @return the result of the interaction
     */
    public TileInteractionResult processPlayerStepOnTile(Player player, Tile tile) {
        TileInteractionResult result = checkTileForCollectibles(player, tile);

        applyInteractionEffects(player, tile, result);

        return result;
    }

    /**
     * Checks what collectibles are available on the tile for this player.
     * Does NOT apply any side effects - only inspects.
     *
     * @param player the player
     * @param tile   the tile to inspect
     * @return what the player would collect
     */
    public TileInteractionResult checkTileForCollectibles(Player player, Tile tile) {
        var treasureCard = tile.getTreasureCard();
        var bonus = tile.getBonus();

        boolean isCurrentTarget = treasureCard != null &&
                player.getCurrentTreasureCard() == treasureCard;

        return new TileInteractionResult(
                isCurrentTarget ? treasureCard : null,
                bonus,
                isCurrentTarget);
    }

    /**
     * Applies the effects of a tile interaction result.
     *
     * @param player the player
     * @param tile   the tile
     * @param result the interaction result to apply
     */
    private void applyInteractionEffects(Player player, Tile tile, TileInteractionResult result) {
        // Handle treasure collection
        if (result.collectedTreasure() != null) {
            LOGGER.info("Player " + player.getUsername() + " collected treasure: " + result.collectedTreasure().getTreasureName());
            result.collectedTreasure().collect();
            player.getStatistics().increaseScore(PointRewards.REWARD_TREASURE);
            player.getStatistics().increaseTreasuresCollected(1);
            tile.setTreasureCard(null);
        }

        // Handle bonus collection
        if (result.collectedBonus() != null) {
            LOGGER.info("Player " + player.getUsername() + " collected bonus: " + result.collectedBonus());
            player.getBonuses().add(result.collectedBonus());
            tile.setBonus(null);
        }

        // Update player position
        player.setCurrentTile(tile);
    }

    /**
     * Checks if a tile is reachable by the player considering blocking players.
     * This delegates movement validation to the Board class.
     *
     * @param player the player attempting to move
     * @param tile   the target tile
     * @return true if the player can move to this tile
     */
    public boolean canPlayerReachTile(Player player, Tile tile, java.util.Set<Tile> reachableTiles) {
        return reachableTiles.contains(tile);
    }

    /**
     * Checks if a tile is blocked by another player.
     *
     * @param tile    the tile to check
     * @param players the list of all players
     * @param moving  the player attempting to move (excluded from collision check)
     * @return true if blocked by another player
     */
    public boolean isTileBlockedByPlayer(Tile tile, java.util.List<Player> players, Player moving) {
        for (Player other : players) {
            if (other != moving && other.getCurrentTile() == tile) {
                return true;
            }
        }
        return false;
    }
}
