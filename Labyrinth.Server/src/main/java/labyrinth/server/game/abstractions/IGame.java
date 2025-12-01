package labyrinth.server.game.abstractions;

import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.GameConfig;
import labyrinth.server.game.models.Player;

import java.util.List;
import java.util.UUID;

public interface IGame {
    Player join(String username);
    void leave(Player player);
    List<Player> getPlayers();
    Player getPlayer(UUID playerId);
    Board getBoard();
    void startGame(GameConfig gameConfig);
}
