package labyrinth.server.game.ai;

import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.*;

class AdvancedAiLogicTest {

    @Test
    void testAdvancedAiBasicMove() {
        Game game = new Game();
        Player p1 = game.join("P1"); // Human
        Player p2 = game.join("Bot");
        p2.setAiActive(true);

        GameConfig config = GameConfig.getDefault();
        List<TreasureCard> cards = new ArrayList<>();
        for (int i = 0; i < 24; i++)
            cards.add(new TreasureCard(i, "C" + i, "img"));

        game.startGame(config, cards, createMockBoard());

        // Simulate P1 turn
        game.shift(1, Direction.RIGHT, null, p1);
        game.movePlayerToTile(game.getCurrentPositionOfPlayer(p1).row(), game.getCurrentPositionOfPlayer(p1).column(),
                p1);

        // Wait for Bot
        System.out.println("Waiting for Advanced AI...");
        long start = System.currentTimeMillis();
        // Give it generous time (simulating depth 2 might be slower)
        while (game.getCurrentPlayer().equals(p2)) {
            if (System.currentTimeMillis() - start > 10000) {
                Assertions.fail("Advanced AI took too long");
            }
            try {
                Thread.sleep(200);
            } catch (Exception e) {
            }
        }

        System.out.println("Advanced AI finished turn.");
        Assertions.assertEquals(p1, game.getCurrentPlayer());
    }

    private Board createMockBoard() {
        BiMap<Position, Tile> tiles = new BiMap<>();
        for (int r = 0; r < 7; r++) {
            for (int c = 0; c < 7; c++) {
                Set<Direction> dirs = EnumSet.of(Direction.UP, Direction.DOWN);
                Tile t = new Tile(dirs);
                tiles.put(new Position(r, c), t);
            }
        }
        Tile extra = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));
        return new Board(7, 7, tiles, extra);
    }
}
