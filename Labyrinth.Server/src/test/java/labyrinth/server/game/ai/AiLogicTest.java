package labyrinth.server.game.ai;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import static labyrinth.server.game.GameTestHelper.createGame;
import static org.mockito.Mockito.mock;

class AiLogicTest {

    @Test
    void testAiTriggersOnNextPlayer() {
        // Setup simple game
        Game game = createGame(mock(IGameTimer.class), new SligthlyLessSimpleAiStrategy(),
                new labyrinth.server.game.services.GameLogger());

        Player p1 = game.join("P1");
        Player p2 = game.join("P2");
        p2.setAiActive(true); // P2 is AI

        // Start game manually to bypass minimum player checks if needed, but
        // Game.startGame checks < 2.
        // We have 2 players.
        GameConfig config = GameConfig.getDefault();
        List<TreasureCard> cards = new ArrayList<>();
        // Add dummy cards
        for (int i = 0; i < 24; i++)
            cards.add(new TreasureCard(i, "C" + i, "img"));

        game.startGame(config, cards, createMockBoard());

        // Ensure P1 starts
        // We want to verify that when P1 finishes turn, P2 (AI) moves.
        // We can check P2's state or board state change.
        // P2 starts at some position.
        Position startPosP2 = game.getCurrentPositionOfPlayer(p2);

        // P1 does a move (dummy move)
        // We need to simulate P1 turn.
        // Shift a fake row/col that is safe
        game.shift(1, Direction.RIGHT, p1);
        game.movePlayerToTile(game.getCurrentPositionOfPlayer(p1).row(), game.getCurrentPositionOfPlayer(p1).column(),
                p1);
        // This triggers nextPlayer -> P2 (Async AI) start

        System.out.println("P2 Turn started, waiting for async...");

        // Wait for P2 to finish
        // We poll until currentPlayerIndex is back to 0 (P1)
        long start = System.currentTimeMillis();
        while (game.getCurrentPlayer().equals(p2)) {
            if (System.currentTimeMillis() - start > 5000) {
                Assertions.fail("AI took too long to finish turn");
            }
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        // Now P2 should have moved.
        Position endPosP2 = game.getCurrentPositionOfPlayer(p2);

        // Ideally P2 moved or shifted board.
        // Since we can't easily deterministic test random AI, we just check no crash
        // and state changed OR log.
        // But SimpleAiStrategy is deterministic if there are treasures.
        // If P2 has a treasure, it should try to reach it.

        System.out.println("P2 Start: " + startPosP2);
        System.out.println("P2 End: " + endPosP2);

        // If AI worked, it must have shifted the board.
        // Game now fills with AI players to 4 total, so after P2 comes Bot 3, not back to P1
        // Check that current player is not P2 (meaning P2's turn finished)
        Assertions.assertNotEquals(p2, game.getCurrentPlayer(), "AI should have finished its turn");
    }

    // Helper to create a board if needed, or use real one
    private Board createMockBoard() {
        // We can rely on Game internal default board creation if we use startGame?
        // Game constructor sets board to null. startGame expects board passed in.
        // We need to create a board.
        // Let's copy logic from a factory or just manual.
        // Use a simpler approach: Mock or partial.
        // Actually, let's just use the real board construction logic if possible.
        // But specific tile layout is hard to forge quickly.
        // We will assume Game.startGame needs a fully valid board.
        // Let's create a minimal 3x3 board for testing if possible, but GameConfig
        // defaults to 7x7.
        // We will try to rely on GameConfig default behavior but we need 'Board'
        // instance.
        // The project structure implies 'Game' doesn't create Board, the
        // Controller/Service does.
        // I'll create a simple 7x7 board.

        BiMap<Position, Tile> tiles = new BiMap<>();
        for (int r = 0; r < 7; r++) {
            for (int c = 0; c < 7; c++) {
                Set<Direction> dirs = EnumSet.of(Direction.UP, Direction.DOWN); // straights
                Tile t = new Tile(dirs);
                tiles.put(new Position(r, c), t);
            }
        }
        Tile extra = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));
        return new Board(7, 7, tiles, extra);
    }
}
