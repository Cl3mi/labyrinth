package labyrinth.server.game;


import labyrinth.contracts.models.PlayerColor;
import org.springframework.stereotype.Component;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Component
public class Lobby {
    private static final int MAX_PLAYERS = 4;
    private final List<Player> players = new ArrayList<>();

    public Player tryJoinLobby(String username) throws IllegalArgumentException {
        if (isFull()) {
            throw new IllegalStateException("Lobby is full");
        }

        if (!isUsernameAvailable(username)) {
            throw new IllegalArgumentException("Username is already taken");
        }

        Player player = new Player();
        player.setId(UUID.randomUUID());
        player.setUsername(username);
        player.setJoinDate(OffsetDateTime.now());
        player.setColor(getNextColor());

        players.add(player);

        return player;
    }

    public List<Player> getPlayers() {
        return players;
    }


    private boolean isUsernameAvailable(String username) {
        return players.stream()
                .noneMatch(p -> p.getUsername().equalsIgnoreCase(username));
    }

    private boolean isFull() {
        return players.size() >= MAX_PLAYERS;
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
