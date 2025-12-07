package labyrinth.server.game.abstractions;

import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;

import java.util.List;
import java.util.Set;
import java.util.UUID;

public interface IGame {
    Player join(String username);

    void leave(Player player);

    List<Player> getPlayers();

    Player getPlayer(UUID playerId);

    Board getBoard();

    void startGame(GameConfig gameConfig, List<TreasureCard> treasureCards, Board board);

    MoveState getCurrentMoveState();

    Player getCurrentPlayer();

    Position getCurrentPositionOfPlayer(Player player);

    boolean movePlayerToTile(int row, int col, Player player);

    boolean shift(int index, Direction direction, Set<Direction> entrances, Player player);

    void toggleAiForPlayer(Player player);

    void useBeamBonus(int row, int col, Player player);

    void useSwapBonus(Player currentPlayer, Player targetPlayer);

    void usePushTwiceBonus(Player player);

    void usePushFixedBonus(Player player);
}
