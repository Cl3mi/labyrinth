package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.results.TileInteractionResult;

import java.util.List;
import java.util.Set;

/**
 * Interface for managing player movement and tile interaction effects.
 * Responsible for applying the results of stepping on tiles (collecting treasures, bonuses)
 * while keeping the Tile class as a pure data entity.
 *
 * <p>This interface enables dependency injection and testing by decoupling
 * the Game class from the concrete MovementManager implementation.</p>
 */
public interface IMovementManager {

    /**
     * Processes a player stepping onto a tile. Returns what was collected
     * and applies all side effects (scoring, collecting items, updating player position).
     *
     * @param player the player moving onto the tile
     * @param tile   the tile being stepped on
     */
    void processPlayerStepOnTile(Player player, Tile tile);

    /**
     * Checks what collectibles are available on the tile for this player.
     * Does NOT apply any side effects - only inspects.
     *
     * @param player the player
     * @param tile   the tile to inspect
     * @return what the player would collect
     */
    TileInteractionResult checkTileForCollectibles(Player player, Tile tile);

    /**
     * Checks if a player can reach a tile considering blocking players.
     *
     * @param player         the player attempting to move
     * @param tile           the target tile
     * @param reachableTiles the set of tiles reachable by the player
     * @return true if the player can move to this tile
     */
    boolean canPlayerReachTile(Player player, Tile tile, Set<Tile> reachableTiles);

    /**
     * Checks if a tile is blocked by another player.
     *
     * @param tile    the tile to check
     * @param players the list of all players
     * @param moving  the player attempting to move (excluded from collision check)
     * @return true if blocked by another player
     */
    boolean isTileBlockedByPlayer(Tile tile, List<Player> players, Player moving);
}
