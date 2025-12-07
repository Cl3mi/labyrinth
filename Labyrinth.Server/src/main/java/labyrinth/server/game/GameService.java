package labyrinth.server.game;

import labyrinth.server.game.abstractions.IBoardFactory;
import labyrinth.server.game.abstractions.ITreasureCardFactory;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import lombok.Getter;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
public class GameService {

    @Getter
    private final Game game;

    private final ITreasureCardFactory treasureCardFactory;
    private final IBoardFactory boardFactory;

    public GameService(ITreasureCardFactory treasureCardFactory, IBoardFactory boardFactory) {
        this.game = new Game();
        this.treasureCardFactory = treasureCardFactory;
        this.boardFactory = boardFactory;
    }

    public Player join(String username) {
        return game.join(username);
    }


    public void leave(Player player) {
        game.leave(player);
    }


    public List<Player> getPlayers() {
        return game.getPlayers();
    }


    public Player getPlayer(UUID playerId) {
        return game.getPlayer(playerId);
    }


    public Board getBoard() {
        return game.getBoard();
    }


    public void startGame(GameConfig gameConfig) {
        var board = boardFactory.createBoard(gameConfig.boardWidth(), gameConfig.boardHeight());
        var treasureCards = treasureCardFactory.createTreasureCards(gameConfig.treasureCardCount(), game.getPlayers().size());

        game.startGame(gameConfig, treasureCards, board);
    }


    public MoveState getCurrentMoveState() {
        return game.getCurrentMoveState();
    }


    public Player getCurrentPlayer() {
        return game.getCurrentPlayer();
    }


    public Position getCurrentPositionOfPlayer(Player player) {
        return game.getCurrentPositionOfPlayer(player);
    }


    public boolean movePlayerToTile(int row, int col, Player player) {
        return game.movePlayerToTile(row, col, player);
    }


    public boolean shift(int index, Direction direction, Set<Direction> entrances, Player player) {
        return game.shift(index, direction, entrances, player);
    }


    public void toggleAiForPlayer(Player player) {
        game.toggleAiForPlayer(player);
    }


    public void useBeamBonus(int row, int col, Player player) {
        game.useBeamBonus(row, col, player);
    }


    public void useSwapBonus(Player currentPlayer, Player targetPlayer) {
        game.useSwapBonus(currentPlayer, targetPlayer);
    }


    public void usePushTwiceBonus(Player player) {
        game.usePushTwiceBonus(player);
    }


    public void usePushFixedBonus(Player player) {
        game.usePushFixedBonus(player);
    }

}
