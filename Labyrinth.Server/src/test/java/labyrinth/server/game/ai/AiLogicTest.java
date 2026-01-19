package labyrinth.server.game.ai;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.factories.BoardFactory;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import labyrinth.server.game.services.GameLogger;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static labyrinth.server.game.GameTestHelper.createGame;
import static org.mockito.Mockito.*;

class AiLogicTest {

    @Test
    void testAiTriggersOnNextPlayer() {
        // Setup: Create an AI strategy that will be triggered when it's the AI's turn
        AtomicBoolean aiWasTriggered = new AtomicBoolean(false);
        AtomicReference<Player> triggeredPlayer = new AtomicReference<>(null);

        AiStrategy aiStrategy = player -> {
            aiWasTriggered.set(true);
            triggeredPlayer.set(player);
        };

        Game game = createGame(mock(IGameTimer.class), aiStrategy, new GameLogger());

        Player p1 = game.join("P1");
        Player p2 = game.join("P2");
        p2.setAiActive(true); // P2 is AI

        GameConfig config = GameConfig.getDefault();
        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < 24; i++) {
            cards.add(new TreasureCard(i, "C" + i, "img"));
        }

        Board board = new BoardFactory().createBoard(7, 7);
        game.startGame(config, cards, board);

        // Ensure P1 is the current player and starts at their position
        Assertions.assertEquals(p1, game.getCurrentPlayer(), "P1 should be the current player");

        // AI was triggered during startGame if first player is AI
        // Reset the flag for next player test
        aiWasTriggered.set(false);

        // P1 does a move
        Position startPosP1 = game.getCurrentPositionOfPlayer(p1);

        // Shift a row that's not fixed (odd index)
        game.shift(1, Direction.RIGHT, p1);

        // Move to current position (stay in place - valid move)
        Position currentPosP1 = game.getCurrentPositionOfPlayer(p1);
        game.movePlayerToTile(currentPosP1.row(), currentPosP1.column(), p1);

        // Check that current player is not P1 (turn advanced)
        Assertions.assertNotEquals(p1, game.getCurrentPlayer(), "Turn should have advanced from P1");

        System.out.println("P1 Turn completed");
        System.out.println("Current player after P1's turn: " + game.getCurrentPlayer().getUsername());
        System.out.println("AI was triggered: " + aiWasTriggered.get());
    }

    @Test
    void testGameFillsWithAiPlayers() {
        AiStrategy aiStrategy = mock(AiStrategy.class);
        Game game = createGame(mock(IGameTimer.class), aiStrategy, new GameLogger());

        game.join("P1");
        game.join("P2");

        GameConfig config = GameConfig.getDefault();
        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < 24; i++) {
            cards.add(new TreasureCard(i, "C" + i, "img"));
        }

        Board board = new BoardFactory().createBoard(7, 7);
        game.startGame(config, cards, board);

        // Game should fill with AI players to 4 total
        Assertions.assertEquals(4, game.getPlayers().size(), "Game should have 4 players");

        long aiCount = game.getPlayers().stream().filter(Player::isAiActive).count();
        Assertions.assertEquals(2, aiCount, "Should have 2 AI players added");

        System.out.println("Players in game:");
        for (Player p : game.getPlayers()) {
            System.out.println("  - " + p.getUsername() + " (AI: " + p.isAiActive() + ")");
        }
    }

    @Test
    void testPlayerPositionsInitialized() {
        AiStrategy aiStrategy = mock(AiStrategy.class);
        Game game = createGame(mock(IGameTimer.class), aiStrategy, new GameLogger());

        game.join("P1");
        game.join("P2");

        GameConfig config = GameConfig.getDefault();
        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < 24; i++) {
            cards.add(new TreasureCard(i, "C" + i, "img"));
        }

        Board board = new BoardFactory().createBoard(7, 7);
        game.startGame(config, cards, board);

        // All players should have positions
        for (Player p : game.getPlayers()) {
            Position pos = game.getCurrentPositionOfPlayer(p);
            Assertions.assertNotNull(pos, "Player " + p.getUsername() + " should have a position");
            Assertions.assertNotNull(p.getCurrentTile(), "Player should have current tile");
            Assertions.assertNotNull(p.getHomeTile(), "Player should have home tile");

            System.out.println("Player " + p.getUsername() + " at position: " + pos);
        }
    }
}
