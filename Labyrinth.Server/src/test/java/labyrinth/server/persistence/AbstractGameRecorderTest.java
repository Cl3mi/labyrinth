package labyrinth.server.persistence;

import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.config.JacksonConfig;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.EnumSet;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Runs the same checks against every supported database (H2 and PostgreSQL). */
abstract class AbstractGameRecorderTest {

    private final ObjectMapper objectMapper = new JacksonConfig().objectMapper();
    private JdbcClient jdbc;
    private JdbcGameRecorder recorderA;
    private JdbcGameRecorder recorderB;

    protected abstract DriverManagerDataSource dataSource();

    @BeforeEach
    void setUp() {
        var ds = dataSource();
        Flyway.configure().dataSource(ds).cleanDisabled(false).load().clean();
        Flyway.configure().dataSource(ds).load().migrate();
        jdbc = JdbcClient.create(ds);
        var tx = new TransactionTemplate(new DataSourceTransactionManager(ds));
        // Two server instances writing into the same database
        recorderA = new JdbcGameRecorder(ds, tx, objectMapper, "vm1:8082", "Team02 #1", 8082);
        recorderB = new JdbcGameRecorder(ds, tx, objectMapper, "", "Team02 #2", 8083);
    }

    private static GameRecords.GameStarted started(UUID id) {
        return new GameRecords.GameStarted(id, OffsetDateTime.now(), 7, 7, 24, 3, 30, 1800, 2);
    }

    @Test
    void finishedGameIsStoredWithRanking() throws Exception {
        var gameId = UUID.randomUUID();
        var winner = UUID.randomUUID();
        var loser = UUID.randomUUID();

        recorderA.gameStarted(started(gameId));
        recorderA.gameFinished(new GameRecords.GameFinished(gameId, OffsetDateTime.now(), List.of(
                new GameRecords.PlayerResult(winner, "Anna", false, 1, 300, 6, 40, 12, List.of("RUNNER", "PUSHER"), true),
                new GameRecords.PlayerResult(loser, "Bot 1", true, 2, 120, 3, 55, 9, List.of(), false))));
        recorderA.flush();

        var game = jdbc.sql("SELECT status, instance_id, server_name, ended_at, player_count FROM game WHERE id = ?")
                .param(gameId).query().singleRow();
        assertEquals("FINISHED", game.get("status"));
        assertEquals("vm1:8082", game.get("instance_id"));
        assertEquals("Team02 #1", game.get("server_name"));
        assertNotNull(game.get("ended_at"));
        assertEquals(2, ((Number) game.get("player_count")).intValue());

        var rows = jdbc.sql("""
                        SELECT username, placement, score, achievements, ended_game, is_bot
                        FROM player_result WHERE game_id = ? ORDER BY placement""")
                .param(gameId).query().listOfRows();
        assertEquals(2, rows.size());
        assertEquals("Anna", rows.get(0).get("username"));
        assertEquals("RUNNER,PUSHER", rows.get(0).get("achievements"));
        assertEquals(true, rows.get(0).get("ended_game"));
        assertEquals("", rows.get(1).get("achievements"));
        assertEquals(true, rows.get(1).get("is_bot"));
    }

    @Test
    void abortedGameKeepsNoResult() throws Exception {
        var gameId = UUID.randomUUID();
        recorderA.gameStarted(started(gameId));
        recorderA.gameAborted(gameId, OffsetDateTime.now());
        recorderA.flush();

        assertEquals("ABORTED", jdbc.sql("SELECT status FROM game WHERE id = ?").param(gameId).query(String.class).single());
        assertEquals(0, jdbc.sql("SELECT count(*) FROM player_result WHERE game_id = ?").param(gameId).query(Integer.class).single());
    }

    @Test
    void severalInstancesWriteIntoTheSameDatabase() throws Exception {
        var g1 = UUID.randomUUID();
        var g2 = UUID.randomUUID();
        recorderA.gameStarted(started(g1));
        recorderB.gameStarted(started(g2));
        recorderA.flush();
        recorderB.flush();

        var instances = jdbc.sql("SELECT instance_id FROM game ORDER BY instance_id").query(String.class).list();
        assertEquals(2, instances.size());
        assertTrue(instances.contains("vm1:8082"));
        // Empty instance id falls back to <hostname>:<port>
        assertTrue(instances.stream().anyMatch(i -> i.endsWith(":8083")), instances.toString());
    }

    @Test
    void databaseErrorDoesNotThrowIntoTheGame() throws Exception {
        // Finishing an unknown game violates the foreign key; must only be logged.
        recorderA.gameFinished(new GameRecords.GameFinished(UUID.randomUUID(), OffsetDateTime.now(), List.of(
                new GameRecords.PlayerResult(UUID.randomUUID(), "x", false, 1, 0, 0, 0, 0, List.of(), true))));
        recorderA.flush();
        assertEquals(0, jdbc.sql("SELECT count(*) FROM player_result").query(Integer.class).single());
    }

    // UTC times, Jackson normalizes offsets to UTC on read
    private static GameSnapshot snapshot(UUID gameId, long version) {
        var t = OffsetDateTime.of(2026, 11, 17, 18, 0, 0, 0, ZoneOffset.UTC);
        var straight = new GameSnapshot.TileState(EnumSet.of(Direction.UP, Direction.DOWN), false, null, null);
        var corner = new GameSnapshot.TileState(EnumSet.of(Direction.RIGHT, Direction.DOWN), true, null, null);
        var withTreasure = new GameSnapshot.TileState(EnumSet.of(Direction.LEFT, Direction.RIGHT, Direction.UP), false, 7, null);
        var withBonus = new GameSnapshot.TileState(EnumSet.of(Direction.LEFT, Direction.UP), false, null, BonusTypes.BEAM);
        var row = List.of(corner, straight, corner);
        var board = new GameSnapshot.BoardState(3, 3, false,
                List.of(row, List.of(straight, withTreasure, withBonus), row), straight);
        var player = new GameSnapshot.PlayerState(UUID.randomUUID(), "Anna", UUID.randomUUID(), PlayerColor.values()[0],
                t.minusMinutes(5), true, false, false,
                new GameSnapshot.Position(1, 1), new GameSnapshot.Position(0, 0),
                List.of(7), List.of(BonusTypes.SWAP),
                new GameSnapshot.Statistics(12, 3, 1, 0, List.of()));
        return new GameSnapshot(GameSnapshot.FORMAT_VERSION, gameId, version, t.plusSeconds(version),
                new GameSnapshot.Config(3, 3, 1, 1800, 1, 30),
                t.plusMinutes(30), t.plusSeconds(30), board,
                List.of(new GameSnapshot.Treasure(7, "Ghost", false)),
                List.of(player),
                new GameSnapshot.TurnState(0, MoveState.MOVE, false, new GameSnapshot.Shift(1, Direction.DOWN)),
                new GameSnapshot.BonusState(BonusTypes.PUSH_TWICE, 1));
    }

    private GameSnapshot storedSnapshot(UUID gameId) throws Exception {
        var json = jdbc.sql("SELECT CAST(state AS VARCHAR) FROM game_snapshot WHERE game_id = ?")
                .param(gameId).query(String.class).single();
        return objectMapper.readValue(json, GameSnapshot.class);
    }

    @Test
    void snapshotIsStoredAndReplacedByNewerOne() throws Exception {
        var gameId = UUID.randomUUID();
        recorderA.gameStarted(started(gameId));
        recorderA.gameSnapshot(snapshot(gameId, 1));
        recorderA.flush();
        assertEquals(snapshot(gameId, 1).turn(), storedSnapshot(gameId).turn());

        var newer = snapshot(gameId, 2);
        recorderA.gameSnapshot(newer);
        recorderA.flush();

        assertEquals(1, jdbc.sql("SELECT count(*) FROM game_snapshot WHERE game_id = ?").param(gameId).query(Integer.class).single());
        assertEquals(2L, jdbc.sql("SELECT version FROM game_snapshot WHERE game_id = ?").param(gameId).query(Long.class).single());
        var stored = storedSnapshot(gameId);
        assertEquals(newer.board(), stored.board());
        assertEquals(newer.players(), stored.players());
        assertEquals(newer.activeBonus(), stored.activeBonus());
        assertEquals(newer.gameEndTime().toInstant(), stored.gameEndTime().toInstant());
    }

    @Test
    void stateIsStoredAsJsonObject() throws Exception {
        var gameId = UUID.randomUUID();
        recorderA.gameStarted(started(gameId));
        recorderA.gameSnapshot(snapshot(gameId, 1));
        recorderA.flush();
        var json = jdbc.sql("SELECT CAST(state AS VARCHAR) FROM game_snapshot WHERE game_id = ?")
                .param(gameId).query(String.class).single();
        assertTrue(json.startsWith("{"), json.substring(0, Math.min(40, json.length())));
    }

    @Test
    void olderSnapshotDoesNotReplaceNewerOne() throws Exception {
        var gameId = UUID.randomUUID();
        recorderA.gameStarted(started(gameId));
        recorderA.gameSnapshot(snapshot(gameId, 5));
        recorderA.flush();
        recorderA.gameSnapshot(snapshot(gameId, 3));
        recorderA.flush();
        assertEquals(5L, jdbc.sql("SELECT version FROM game_snapshot WHERE game_id = ?").param(gameId).query(Long.class).single());
    }

    @Test
    void snapshotsOfSeveralGamesAreKeptApart() throws Exception {
        var g1 = UUID.randomUUID();
        var g2 = UUID.randomUUID();
        recorderA.gameStarted(started(g1));
        recorderB.gameStarted(started(g2));
        recorderA.gameSnapshot(snapshot(g1, 4));
        recorderB.gameSnapshot(snapshot(g2, 9));
        recorderA.flush();
        recorderB.flush();
        assertEquals(4L, storedSnapshot(g1).version());
        assertEquals(9L, storedSnapshot(g2).version());
    }

    @Test
    void snapshotOfUnknownGameIsOnlyLogged() throws Exception {
        recorderA.gameSnapshot(snapshot(UUID.randomUUID(), 1));
        recorderA.flush();
        assertEquals(0, jdbc.sql("SELECT count(*) FROM game_snapshot").query(Integer.class).single());
    }
}
