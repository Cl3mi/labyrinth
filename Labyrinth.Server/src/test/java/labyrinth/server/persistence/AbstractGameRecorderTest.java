package labyrinth.server.persistence;

import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Runs the same checks against every supported database (H2 and PostgreSQL). */
abstract class AbstractGameRecorderTest {

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
        recorderA = new JdbcGameRecorder(jdbc, tx, "vm1:8082", "Team02 #1", 8082);
        recorderB = new JdbcGameRecorder(jdbc, tx, "", "Team02 #2", 8083);
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
}
