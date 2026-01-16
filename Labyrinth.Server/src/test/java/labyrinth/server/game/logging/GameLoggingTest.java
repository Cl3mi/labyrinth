package labyrinth.server.game.logging;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.GameLogType;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static labyrinth.server.game.GameTestHelper.createGame;
import static org.mockito.Mockito.mock;

class GameLoggingTest {

    private Game game;

    @BeforeEach
    void setUp() {
        game = createGame(mock(IGameTimer.class), new SimpleAiStrategy(),
                new labyrinth.server.game.services.GameLogger());
    }

    @Test
    void testStartGameLogs() {
        // Arrange
        game.join("P1");
        game.join("P2");
        List<TreasureCard> cards = new ArrayList<>();
        IntStream.range(0, 24).forEach(i -> cards.add(new TreasureCard(i, "Card" + i, "img")));

        Board board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7);

        // Act
        game.startGame(GameConfig.getDefault(), cards, board);

        // Assert
        var logs = game.getExecutionLogs();
        Assertions.assertFalse(logs.isEmpty(), "Logs should not be empty");

        var startLog = logs.stream().filter(l -> l.type() == GameLogType.START_GAME).findFirst().orElseThrow();
        Assertions.assertTrue(startLog.metadata().containsKey("boardState"), "Should have boardState in metadata");
        Assertions.assertTrue(startLog.metadata().get("boardState").contains("width=7"),
                "Board state should contain width");
    }

    @Test
    void testShiftLogs() {
        // Arrange
        game.join("P1");
        game.join("P2");
        List<TreasureCard> cards = new ArrayList<>();
        IntStream.range(0, 24).forEach(i -> cards.add(new TreasureCard(i, "Card" + i, "img")));
        Board board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7);
        game.startGame(GameConfig.getDefault(), cards, board);

        Player current = game.getCurrentPlayer();

        // Act
        game.shift(1, Direction.RIGHT, current);

        // Assert
        var logs = game.getExecutionLogs();
        var shiftLog = logs.stream()
                .filter(l -> l.type() == GameLogType.SHIFT_BOARD)
                .findFirst()
                .orElseThrow();

        Assertions.assertEquals("1", shiftLog.metadata().get("index"));
        Assertions.assertEquals("RIGHT", shiftLog.metadata().get("direction"));
        Assertions.assertTrue(shiftLog.metadata().containsKey("insertedTile"), "Should contain insertedTile info");
        Assertions.assertTrue(shiftLog.metadata().get("insertedTile").contains("entrances="),
                "Inserted tile should have entrances");
    }
}
