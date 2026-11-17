package labyrinth.server.game;

import labyrinth.server.config.JacksonConfig;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.factories.BoardFactory;
import labyrinth.server.game.factories.BonusFactory;
import labyrinth.server.game.factories.TreasureCardFactory;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.services.GameInitializerService;
import labyrinth.server.game.services.TreasureBonusDistributionService;
import labyrinth.server.persistence.GameRecorder;
import labyrinth.server.persistence.GameRecords;
import labyrinth.server.persistence.GameSnapshot;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GameServiceRecordingTest {

    private final List<GameRecords.GameStarted> started = new ArrayList<>();
    private final List<UUID> aborted = new ArrayList<>();
    private final List<GameRecords.GameFinished> finished = new ArrayList<>();
    private final List<GameSnapshot> snapshots = new CopyOnWriteArrayList<>();
    private final Map<UUID, UUID> tokens = new HashMap<>();

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
            public void gameSnapshot(GameSnapshot snapshot) { snapshots.add(snapshot); }
        };

        gameService = new GameService(
                new TreasureCardFactory(),
                new BoardFactory(),
                null,
                scheduler,
                new GameInitializerService(new TreasureBonusDistributionService(new BonusFactory())),
                recorder,
                () -> Map.copyOf(tokens));
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

    private GameSnapshot lastSnapshot() {
        assertTrue(!snapshots.isEmpty(), "no snapshot recorded");
        return snapshots.getLast();
    }

    @Test
    void startOfGameIsSnapshotted() throws Exception {
        var anna = gameService.join("Anna");
        var ben = gameService.join("Ben");
        var annaToken = UUID.randomUUID();
        tokens.put(anna.getId(), annaToken);
        gameService.startGame(GameConfig.getDefault());

        assertEquals(1, snapshots.size());
        var s = lastSnapshot();
        assertEquals(started.getFirst().gameId(), s.gameId());
        assertEquals(1, s.version());
        assertEquals(GameSnapshot.FORMAT_VERSION, s.formatVersion());
        assertEquals(7, s.board().width());
        assertEquals(7, s.board().rows().size());
        assertEquals(7, s.board().rows().getFirst().size());
        assertNotNull(s.board().extraTile());
        assertEquals(30, s.config().turnTimeInSeconds());
        assertNotNull(s.gameEndTime());
        assertNotNull(s.turnEndTime(), "turn timer runs for a human player");

        assertEquals(List.of("Anna", "Ben"), s.players().stream().map(GameSnapshot.PlayerState::username).toList());
        var annaState = s.players().getFirst();
        assertEquals(anna.getId(), annaState.id());
        assertEquals(annaToken, annaState.identifierToken());
        assertNull(s.players().get(1).identifierToken(), "Ben has not connected via WebSocket");
        assertEquals(new GameSnapshot.Position(0, 0), annaState.position());
        assertEquals(annaState.position(), annaState.home());
        assertEquals(new GameSnapshot.Position(0, 6), s.players().get(1).home());
        assertTrue(!annaState.treasureIds().isEmpty());

        var treasureIds = s.treasures().stream().map(GameSnapshot.Treasure::id).toList();
        assertEquals(treasureIds.size(), treasureIds.stream().distinct().count());
        for (var p : s.players()) {
            assertTrue(treasureIds.containsAll(p.treasureIds()));
        }
        s.board().rows().stream().flatMap(List::stream)
                .filter(t -> t.treasureId() != null)
                .forEach(t -> assertTrue(treasureIds.contains(t.treasureId())));

        assertEquals(0, s.turn().currentPlayerIndex());
        assertEquals(MoveState.PLACE_TILE, s.turn().moveState());
        assertNull(s.turn().lastShift());
        assertNull(s.activeBonus());
        gameService.leave(ben);
    }

    @Test
    void everyChangeCreatesANewerSnapshot() throws Exception {
        var anna = gameService.join("Anna");
        gameService.join("Ben");
        gameService.startGame(GameConfig.getDefault());
        var before = lastSnapshot();

        gameService.rotateExtraTileClockwise(anna);
        var rotated = lastSnapshot();
        assertEquals(2, rotated.version());
        assertNotEquals(before.board().extraTile().entrances(), rotated.board().extraTile().entrances());

        assertTrue(gameService.shift(1, Direction.DOWN, anna));
        var shifted = lastSnapshot();
        assertEquals(3, shifted.version());
        assertEquals(new GameSnapshot.Shift(1, Direction.DOWN), shifted.turn().lastShift());
        assertEquals(MoveState.MOVE, shifted.turn().moveState());
        assertEquals(rotated.board().extraTile(), shifted.board().rows().get(0).get(1),
                "the extra tile was pushed in at the top of column 1");
        assertEquals(1, shifted.players().getFirst().statistics().tilesPushed());
    }

    @Test
    void failedActionCreatesNoSnapshot() throws Exception {
        var anna = gameService.join("Anna");
        gameService.join("Ben");
        gameService.startGame(GameConfig.getDefault());
        assertTrue(!gameService.shift(0, Direction.DOWN, anna));
        assertEquals(1, snapshots.size());
    }

    @Test
    void noSnapshotAfterTheGameEnded() throws Exception {
        var anna = gameService.join("Anna");
        var ben = gameService.join("Ben");
        gameService.startGame(GameConfig.getDefault());
        gameService.leave(anna);
        gameService.leave(ben);
        int count = snapshots.size();
        gameService.join("Anna");
        assertEquals(count, snapshots.size(), "nothing is snapshotted in the lobby");
    }

    @Test
    void snapshotSurvivesJsonRoundTrip() throws Exception {
        var anna = gameService.join("Anna");
        gameService.join("Ben");
        gameService.startGame(GameConfig.getDefault());
        gameService.rotateExtraTileClockwise(anna);
        gameService.shift(1, Direction.DOWN, anna);

        var mapper = new JacksonConfig().objectMapper();
        var json = mapper.writeValueAsString(lastSnapshot());
        var parsed = mapper.readValue(json, GameSnapshot.class);
        assertEquals(lastSnapshot().treasures(), parsed.treasures());
        assertEquals(lastSnapshot().board(), parsed.board());
        assertEquals(lastSnapshot().turn(), parsed.turn());
    }

    @Test
    void turnTimeoutIsSnapshotted() throws Exception {
        gameService.join("Anna");
        gameService.join("Ben");
        gameService.startGame(new GameConfig(7, 7, 24, 1800, 3, 1));
        long deadline = System.currentTimeMillis() + 5000;
        while (lastSnapshot().turn().currentPlayerIndex() == 0 && System.currentTimeMillis() < deadline) {
            Thread.sleep(50);
        }
        var s = lastSnapshot();
        assertEquals(1, s.turn().currentPlayerIndex(), "snapshot after the timeout shows Ben's turn");
        assertEquals(2, s.version());
        assertNotNull(s.turnEndTime(), "taken after Ben's turn timer started");
    }
}
