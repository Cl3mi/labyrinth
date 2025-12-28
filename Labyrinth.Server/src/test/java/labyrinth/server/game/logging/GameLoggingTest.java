package labyrinth.server.game.logging;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.SimpleAiStrategy;
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

import static org.mockito.Mockito.mock;

class GameLoggingTest {

    private Game game;

    @BeforeEach
    void setUp() {
        game = new Game(mock(IGameTimer.class), new SimpleAiStrategy());
    }

    @Test
    void testStartGameLogs() {
        // Arrange
        game.join("P1");
        game.join("P2");
        List<TreasureCard> cards = new ArrayList<>();
        IntStream.range(0, 24).forEach(i -> cards.add(new TreasureCard(i, "Card" + i, "img")));

        Board board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7, 4);

        // Act
        game.startGame(GameConfig.getDefault(), cards, board);

        // Assert
        var logs = game.getExecutionLogs();
        Assertions.assertFalse(logs.isEmpty(), "Logs should not be empty");

        boolean hasStartLog = logs.stream().anyMatch(l -> l.type() == GameLogType.START_GAME);
        Assertions.assertTrue(hasStartLog, "Should have START_GAME log");
    }

    @Test
    void testShiftLogs() {
        // Arrange
        game.join("P1");
        game.join("P2");
        List<TreasureCard> cards = new ArrayList<>();
        IntStream.range(0, 24).forEach(i -> cards.add(new TreasureCard(i, "Card" + i, "img")));
        Board board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7, 0);
        game.startGame(GameConfig.getDefault(), cards, board);

        Player current = game.getCurrentPlayer();

        // Act
        game.shift(1, Direction.RIGHT, current);

        // Assert
        var logs = game.getExecutionLogs();
        boolean hasShiftLog = logs.stream().anyMatch(l -> l.type() == GameLogType.SHIFT_BOARD
                && l.metadata().get("index").equals("1")
                && l.metadata().get("direction").equals("RIGHT"));
        Assertions.assertTrue(hasShiftLog, "Should have SHIFT_BOARD log with correct metadata");
    }
}
