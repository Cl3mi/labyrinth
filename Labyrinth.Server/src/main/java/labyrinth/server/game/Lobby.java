package labyrinth.server.game;


import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Component
public class Lobby {
    private static final int MAX_PLAYERS = 4;
    private final List<PlayerNew> players = new ArrayList<>();

    public PlayerNew tryJoinLobby(PlayerNew player) throws IllegalArgumentException {
        if (isFull()) {
            throw new IllegalStateException("Lobby is full");
        }

        //if (!isUsernameAvailable(player.getUsername())) {
        //    throw new IllegalArgumentException("Username is already taken");
        //}

        players.add(player);
        return player;
    }

    public void removePlayer(UUID playerId) {
    //    players.removeIf(p -> p.getId().equals(playerId));
    }

    public List<PlayerNew> getPlayers() {
        return players;
    }


    //private boolean isUsernameAvailable(String username) {
        //return players.stream()
       //         .noneMatch(p -> p.getUsername().equalsIgnoreCase(username));
    //}

    private boolean isFull() {
        return players.size() >= MAX_PLAYERS;
    }


}
