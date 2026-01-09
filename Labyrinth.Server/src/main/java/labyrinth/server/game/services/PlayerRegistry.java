package labyrinth.server.game.services;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.results.LeaveResult;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Manages the lifecycle and state of players in a game.
 * Handles player registration, removal, admin assignment, and AI player management.
 *
 * Responsibilities:
 * - Player join/leave operations
 * - Admin role assignment and reassignment
 * - AI player creation and management
 * - Username validation
 * - Player color assignment
 */
public class PlayerRegistry {

    private final List<Player> players;
    private final int maxPlayers;

    public PlayerRegistry(int maxPlayers) {
        this.maxPlayers = maxPlayers;
        this.players = new ArrayList<>();
    }

    /**
     * Adds a player to the registry. The first player automatically becomes admin.
     *
     * @param username the username of the player joining
     * @return the newly created player
     * @throws IllegalStateException if the registry is full
     * @throws IllegalArgumentException if username is already taken
     */
    public Player addPlayer(String username) {
        if (isFull()) {
            throw new IllegalStateException("Room is full");
        }

        if (!isUsernameAvailable(username)) {
            throw new IllegalArgumentException("Username is already taken");
        }

        Player player = new Player(UUID.randomUUID(), username);
        player.setColor(getNextColor());

        if (players.isEmpty()) {
            player.setAdmin(true);
        }

        player.setJoinDate(OffsetDateTime.now());
        players.add(player);
        return player;
    }

    /**
     * Removes a player from the registry and handles admin reassignment if necessary.
     *
     * @param player the player to remove
     * @return result containing removal status, new admin (if reassigned), and shutdown flag
     */
    public LeaveResult removePlayer(Player player) {
        boolean wasRemoved = players.removeIf(p -> p.getId().equals(player.getId()));

        if (!wasRemoved) {
            return new LeaveResult(false, null, false);
        }

        Player newAdmin = null;
        if (player.isAdmin()) {
            newAdmin = reassignAdmin();
        }

        boolean shouldShutdown = players.stream().noneMatch(p -> !p.isAiActive());

        return new LeaveResult(true, newAdmin, shouldShutdown);
    }

    /**
     * Fills the registry with AI players up to maxPlayers.
     * Called before creating treasure cards to ensure correct player count.
     */
    public void fillWithAiPlayers() {
        while (players.size() < maxPlayers) {
            addAiPlayer();
        }
    }

    /**
     * Retrieves a player by their unique ID.
     *
     * @param playerId the player's UUID
     * @return the player, or null if not found
     */
    public Player getPlayer(UUID playerId) {
        return players.stream()
                .filter(p -> p.getId().equals(playerId))
                .findFirst()
                .orElse(null);
    }

    /**
     * Gets all players in the registry.
     *
     * @return immutable list of players
     */
    public List<Player> getPlayers() {
        return List.copyOf(players);
    }

    /**
     * Gets the mutable internal player list.
     * Exposed for use by Game class which needs direct access for certain operations.
     * Use with caution - prefer getPlayers() for external access.
     *
     * @return mutable list of players
     */
    public List<Player> getPlayersInternal() {
        return players;
    }

    /**
     * Checks if the registry is full.
     *
     * @return true if at max capacity
     */
    public boolean isFull() {
        return players.size() >= maxPlayers;
    }

    /**
     * Clears all players from the registry.
     * Used when resetting for a new game.
     */
    public void clear() {
        players.clear();
    }

    /**
     * Gets the maximum number of players allowed.
     *
     * @return max players
     */
    public int getMaxPlayers() {
        return maxPlayers;
    }

    // ========== Private Helper Methods ==========

    private void addAiPlayer() {
        Player player = new Player(UUID.randomUUID(), "Bot " + (players.size() + 1));
        player.setColor(getNextColor());
        player.setAiActive(true);
        player.setJoinDate(OffsetDateTime.now());
        players.add(player);
    }

    private Player reassignAdmin() {
        // Find first non-AI player to become new admin
        var nextAdmin = players.stream()
                .filter(p -> !p.isAiActive())
                .findFirst();

        if (nextAdmin.isPresent()) {
            nextAdmin.get().setAdmin(true);
            return nextAdmin.get();
        }

        // If no human players, fallback to first AI player
        if (!players.isEmpty()) {
            players.get(0).setAdmin(true);
            return players.get(0);
        }

        return null;
    }

    private boolean isUsernameAvailable(String username) {
        return players.stream()
                .noneMatch(p -> p.getUsername().equalsIgnoreCase(username));
    }

    private PlayerColor getNextColor() {
        for (PlayerColor color : PlayerColor.values()) {
            boolean used = players.stream()
                    .anyMatch(p -> p.getColor() == color);
            if (!used) {
                return color;
            }
        }
        throw new IllegalStateException("No available colors left");
    }
}
