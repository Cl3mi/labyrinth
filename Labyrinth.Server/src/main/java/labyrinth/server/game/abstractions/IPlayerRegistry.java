package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Player;

import java.util.List;
import java.util.UUID;

/**
 * Interface for managing players in a game session.
 * Handles player registration, removal, admin assignment, and AI player management.
 *
 * <p>This interface enables dependency injection and testing by decoupling
 * the Game class from the concrete PlayerRegistry implementation.</p>
 */
public interface IPlayerRegistry {

    /**
     * Adds a player to the registry. The first player automatically becomes admin.
     *
     * @param username the username of the player joining
     * @return the newly created player
     * @throws IllegalStateException if the registry is full
     * @throws IllegalArgumentException if username is already taken
     */
    Player addPlayer(String username);

    /**
     * Removes a player from the registry and handles admin reassignment if necessary.
     *
     * @param player the player to remove
     */
    void removePlayer(Player player);

    /**
     * Fills the registry with AI players up to maxPlayers.
     * Called before creating treasure cards to ensure correct player count.
     */
    void fillWithAiPlayers();

    /**
     * Retrieves a player by their unique ID.
     *
     * @param playerId the player's UUID
     * @return the player, or null if not found
     */
    Player getPlayer(UUID playerId);

    /**
     * Gets all players in the registry.
     *
     * @return immutable list of players
     */
    List<Player> getPlayers();

    /**
     * Gets the mutable internal player list.
     * <p><strong>Warning:</strong> This method exposes internal state and should be used with caution.
     * Prefer {@link #getPlayers()} for external access.</p>
     *
     * @return mutable list of players
     */
    List<Player> getPlayersInternal();

    /**
     * Checks if the registry is full.
     *
     * @return true if at max capacity
     */
    boolean isFull();
}
