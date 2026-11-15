package labyrinth.server.game;

import labyrinth.server.game.factories.BoardFactory;
import labyrinth.server.game.factories.BonusFactory;
import labyrinth.server.game.factories.TreasureCardFactory;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.services.GameInitializerService;
import labyrinth.server.game.services.TreasureBonusDistributionService;
import labyrinth.server.persistence.GameRecorder;
import labyrinth.server.persistence.GameRecords;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Checks that GameService reports start and abort of a game to the GameRecorder. */
class GameServiceRecordingTest {

    private final List<GameRecords.GameStarted> started = new ArrayList<>();
    private final List<UUID> aborted = new ArrayList<>();
    private final List<GameRecords.GameFinished> finished = new ArrayList<>();

    private ThreadPoolTaskScheduler scheduler;
    private GameService gameService;

    @BeforeEach
    void setUp() {
        scheduler = new ThreadPoolTaskScheduler();
        scheduler.initialize();

        GameRecorder recorder = new GameRecorder() {
            public void gameStarted(GameRecords.GameStarted game) { started.add(game); }
            public void gameFinished(GameRecords.GameFinished result) { finished.add(result); }
            public void gameAborted(UUID gameId, OffsetDateTime endedAt) { aborted.add(gameId); }
        };

        gameService = new GameService(
                new TreasureCardFactory(),
                new BoardFactory(),
                null,
                scheduler,
                new GameInitializerService(new TreasureBonusDistributionService(new BonusFactory())),
                recorder);
    }

    @AfterEach
    void tearDown() {
        scheduler.shutdown();
    }

    @Test
    void startAndAbortAreRecorded() throws Exception {
        var anna = gameService.join("Anna");
        var ben = gameService.join("Ben");

        gameService.startGame(GameConfig.getDefault());

        assertEquals(1, started.size());
        var game = started.getFirst();
        assertEquals(2, game.playerCount());
        assertEquals(7, game.boardWidth());
        assertEquals(30, game.turnTimeSeconds());

        // All humans leave: game goes back to the lobby without a result
        gameService.leave(anna);
        gameService.leave(ben);

        assertEquals(List.of(game.gameId()), aborted);
        assertTrue(finished.isEmpty());
    }

    @Test
    void eachGameGetsItsOwnId() throws Exception {
        var anna = gameService.join("Anna");
        var ben = gameService.join("Ben");
        gameService.startGame(GameConfig.getDefault());
        gameService.leave(anna);
        gameService.leave(ben);

        gameService.join("Anna");
        gameService.join("Ben");
        gameService.startGame(GameConfig.getDefault());

        assertEquals(2, started.size());
        assertTrue(!started.get(0).gameId().equals(started.get(1).gameId()));
    }

    @Test
    void leavingTheLobbyRecordsNothing() throws Exception {
        var anna = gameService.join("Anna");
        gameService.leave(anna);
        assertTrue(started.isEmpty());
        assertTrue(aborted.isEmpty());
    }
}
