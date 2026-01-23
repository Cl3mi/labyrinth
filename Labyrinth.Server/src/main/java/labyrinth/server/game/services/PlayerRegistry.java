package labyrinth.server.game.services;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.exceptions.UsernameTakenException;
import labyrinth.server.game.abstractions.IPlayerRegistry;
import labyrinth.server.game.models.Player;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Manages the lifecycle and state of players in a game.
 * Handles player registration, removal, admin assignment, and AI player management.
 * <p>
 * Responsibilities:
 * - Player join/leave operations
 * - Admin role assignment and reassignment
 * - AI player creation and management
 * - Username validation
 * - Player color assignment
 */
public class PlayerRegistry implements IPlayerRegistry {

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
     * @throws IllegalStateException    if the registry is full
     * @throws IllegalArgumentException if username is already taken
     */
    public Player addPlayer(String username) throws UsernameTakenException {
        if (isFull()) {
            throw new IllegalStateException("Room is full");
        }

        if (!isUsernameAvailable(username)) {
            throw new UsernameTakenException();
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
     */
    public void removePlayer(Player player) {
        boolean wasRemoved = players.removeIf(p -> p.getId().equals(player.getId()));

        if (!wasRemoved) {
            return;
        }

        if (player.isAdmin()) {
            reassignAdmin();
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

    public void reassignAdmin() {
        var nextAdmin = players.stream()
                .filter(p -> !p.isAiActive())
                .findFirst();

        nextAdmin.ifPresent(player -> player.setAdmin(true));

        if (!players.isEmpty()) {
            players.getFirst().setAdmin(true);
        }
    }

    public boolean anyPlayerActive() {

        return players.stream()
                .anyMatch(p -> !p.isBot());
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
