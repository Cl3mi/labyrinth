package labyrinth.server.game;


import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
public class GameService {

    private final Game lobby;
    private final List<Player> players = new ArrayList<>();

    public GameService(Game lobby) {
        this.lobby = lobby;
    }


    public Player connectPlayer(String username) {
        Player player = new Player(UUID.randomUUID(), username);
        player.setJoinDate(OffsetDateTime.now());
        player.setColor(getNextColor());
        players.add(player);

        return lobby.join(player);
    }

    public List<Player> getPlayersInLobby() {
        return lobby.getPlayers();
    }

    public void removePlayer(UUID playerId) {
        lobby.removePlayer(playerId);
    }

    public Player getPlayer(UUID playerId) {
        return players.stream()
                .filter(p -> p.getId().equals(playerId))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Player not found"));
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
