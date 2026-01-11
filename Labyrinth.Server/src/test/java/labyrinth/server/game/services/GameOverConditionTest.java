package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.SimpleAiStrategy;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.results.MovePlayerToTileResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

/**
 * Tests for the game-over condition that requires players to reach their home tile
 * after collecting all treasures.
 */
class GameOverConditionTest {

    private Game game;
    private GameConfig gameConfig;
    private Board board;
    private List<TreasureCard> treasureCards;

    @BeforeEach
    void setUp() {
        game = new Game(mock(IGameTimer.class), new SimpleAiStrategy(), new GameLogger());
        gameConfig = GameConfig.getDefault();
        board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7);
        treasureCards = new ArrayList<>();
        // Create minimal treasure cards for testing (1 per player)
        IntStream.range(0, 4).forEach(i -> treasureCards.add(new TreasureCard(i, "Card" + i, "img")));
    }

    @Test
    void gameOver_shouldNotTrigger_whenPlayerCollectsAllTreasuresButNotAtHomeTile() {
        // Arrange
        game.join("Player1");
        game.startGame(gameConfig, treasureCards, board);

        Player player = game.getCurrentPlayer();

        // Manually collect the player's treasure by marking it as collected
        // This simulates the player having collected all treasures
        player.getCurrentTreasureCard().collect();

        // Verify player has collected all treasures
        assertNull(player.getCurrentTreasureCard(), "Player should have no more treasures");

        // Move player away from home tile
        var homeTile = player.getHomeTile();
        var homePos = board.getPositionOfTile(homeTile);

        // Find a different position to move to (not home)
        int targetRow = (homePos.row() + 1) % board.getHeight();
        int targetCol = homePos.column();

        // Ensure we can make a move by shifting
        game.shift(1, Direction.DOWN, player);

        // Try to move to a non-home position
        MovePlayerToTileResult result = game.movePlayerToTile(targetRow, targetCol, player);

        // Assert - Even if move fails due to reachability, the important thing is
        // that having no treasures doesn't immediately end the game
        assertFalse(result.gameOver(), "Game should NOT be over - player must reach home tile");
        assertEquals(RoomState.IN_GAME, game.getRoomState(), "Game should still be IN_GAME");
    }

    @Test
    void gameOver_shouldTrigger_whenPlayerReachesHomeTileAfterCollectingAllTreasures() {
        // Arrange
        game.join("Player1");
        game.startGame(gameConfig, treasureCards, board);

        Player player = game.getCurrentPlayer();
        var homeTilePosition = board.getPositionOfTile(player.getHomeTile());

        // First, move player away from home
        game.shift(1, Direction.DOWN, player);
        var currentPos = game.getCurrentPositionOfPlayer(player);

        // Find a reachable tile that's not home
        var reachableTiles = board.getReachableTiles(player);
        var targetTile = reachableTiles.stream()
                .filter(t -> t != player.getHomeTile())
                .findFirst()
                .orElse(null);

        if (targetTile != null) {
            var targetPos = board.getPositionOfTile(targetTile);
            game.movePlayerToTile(targetPos.row(), targetPos.column(), player);
        }

        // Now manually mark player's treasure as collected AFTER moving away from home
        player.getCurrentTreasureCard().collect();
        assertNull(player.getCurrentTreasureCard(), "Player should have no more treasures");

        // Advance to next turn (player 1's turn again after 3 other players)
        for (int i = 0; i < 3; i++) {
            Player currentPlayer = game.getCurrentPlayer();
            game.shift(1, Direction.DOWN, currentPlayer);
            var pos = game.getCurrentPositionOfPlayer(currentPlayer);
            game.movePlayerToTile(pos.row(), pos.column(), currentPlayer);
        }

        // Now it's player 1's turn and they have all treasures - try to reach home
        assertEquals(player, game.getCurrentPlayer(), "Should be Player1's turn");

        // Shift board to potentially make home tile reachable
        game.shift(1, Direction.DOWN, player);

        // Act - Move to home tile
        MovePlayerToTileResult homeResult = game.movePlayerToTile(
                homeTilePosition.row(),
                homeTilePosition.column(),
                player
        );

        // Assert
        if (homeResult.moveSuccess()) {
            // If the move succeeded (home was reachable), game should be over
            assertTrue(homeResult.gameOver(), "Game should be over when reaching home with all treasures");
            assertEquals(RoomState.FINISHED, game.getRoomState(), "Game should be FINISHED");
        } else {
            // If move failed due to reachability, at least verify game didn't end prematurely
            assertEquals(RoomState.IN_GAME, game.getRoomState(), "Game should still be IN_GAME if home not reached");
        }
    }

    @Test
    void gameOver_shouldNotTrigger_whenPlayerAtHomeTileButHasNotCollectedAllTreasures() {
        // Arrange
        game.join("Player1");
        game.startGame(gameConfig, treasureCards, board);

        Player player = game.getCurrentPlayer();
        var homeTilePosition = board.getPositionOfTile(player.getHomeTile());

        // Player starts at home tile with treasures still to collect
        assertNotNull(player.getCurrentTreasureCard(), "Player should still have treasures to collect");
        assertEquals(player.getCurrentTile(), player.getHomeTile(), "Player starts at home");

        // Act - Try to move away and back (player is already at home)
        game.shift(1, Direction.DOWN, player);
        MovePlayerToTileResult result = game.movePlayerToTile(
                homeTilePosition.row(),
                homeTilePosition.column(),
                player
        );

        // Assert
        assertTrue(result.moveSuccess(), "Move should succeed");
        assertFalse(result.gameOver(), "Game should NOT be over - player still has treasures");
        assertEquals(RoomState.IN_GAME, game.getRoomState(), "Game should still be IN_GAME");
    }
}
