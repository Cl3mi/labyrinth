package labyrinth.server.game;


import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class GameService {

    private final Lobby lobby;

    public GameService(Lobby lobby) {
        this.lobby = lobby;
    }


    public Player connectPlayer(String username) {
        return lobby.tryJoinLobby(username);
    }

    public List<Player> getPlayersInLobby() {
        return lobby.getPlayers();
    }
}
