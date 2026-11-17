package labyrinth.server.persistence;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PreDestroy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionTemplate;

import javax.sql.DataSource;
import java.net.InetAddress;
import java.sql.DatabaseMetaData;
import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Writes games and results with plain JDBC.
 *
 * <p>All writes run on one background thread in submission order, so the game never
 * waits for the database and the order start → finish is kept. Failures are logged
 * and do not affect the game.</p>
 */
@Component
public class JdbcGameRecorder implements GameRecorder {

    private static final Logger log = LoggerFactory.getLogger(JdbcGameRecorder.class);

    private final DataSource dataSource;
    private final JdbcClient jdbc;
    private final TransactionTemplate tx;
    private final ObjectMapper objectMapper;
    private final AtomicReference<GameSnapshot> pendingSnapshot = new AtomicReference<>();
    private String jsonParameter;
    private final String instanceId;
    private final String serverName;
    private final ExecutorService writer = Executors.newSingleThreadExecutor(r -> {
        var t = new Thread(r, "game-recorder");
        t.setDaemon(true);
        return t;
    });

    public JdbcGameRecorder(DataSource dataSource,
                            TransactionTemplate tx,
                            ObjectMapper objectMapper,
                            @Value("${labyrinth.instance-id:}") String instanceId,
                            @Value("${server.name:Random Server}") String serverName,
                            @Value("${server.port:8080}") int serverPort) {
        this.dataSource = dataSource;
        this.jdbc = JdbcClient.create(dataSource);
        this.tx = tx;
        this.objectMapper = objectMapper;
        this.serverName = serverName;
        this.instanceId = (instanceId == null || instanceId.isBlank())
                ? hostname() + ":" + serverPort
                : instanceId;
        log.info("Game recorder active, instance id '{}'", this.instanceId);
    }

    public String getInstanceId() {
        return instanceId;
    }

    @Override
    public void gameStarted(GameRecords.GameStarted g) {
        submit("game started " + g.gameId(), () -> jdbc.sql("""
                        INSERT INTO game (id, instance_id, server_name, status, started_at,
                                          board_width, board_height, treasure_card_count, bonus_count,
                                          turn_time_s, game_duration_s, player_count)
                        VALUES (?, ?, ?, 'IN_GAME', ?, ?, ?, ?, ?, ?, ?, ?)
                        """)
                .params(g.gameId(), instanceId, serverName, g.startedAt(),
                        g.boardWidth(), g.boardHeight(), g.treasureCardCount(), g.bonusCount(),
                        g.turnTimeSeconds(), g.gameDurationSeconds(), g.playerCount())
                .update());
    }

    @Override
    public void gameFinished(GameRecords.GameFinished r) {
        submit("game finished " + r.gameId(), () -> tx.executeWithoutResult(status -> {
            jdbc.sql("UPDATE game SET status = 'FINISHED', ended_at = ? WHERE id = ?")
                    .params(r.endedAt(), r.gameId())
                    .update();
            for (var p : r.players()) {
                jdbc.sql("""
                                INSERT INTO player_result (game_id, player_id, username, is_bot, placement, score,
                                                           treasures_collected, steps_taken, tiles_pushed,
                                                           achievements, ended_game)
                                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                                """)
                        .params(r.gameId(), p.playerId(), p.username(), p.bot(), p.placement(), p.score(),
                                p.treasuresCollected(), p.stepsTaken(), p.tilesPushed(),
                                String.join(",", p.achievements()), p.endedGame())
                        .update();
            }
            deleteSnapshot(r.gameId());
        }));
    }

    @Override
    public void gameAborted(UUID gameId, OffsetDateTime endedAt) {
        submit("game aborted " + gameId, () -> abort(gameId, endedAt));
    }

    @Override
    public Optional<GameSnapshot> takeOverRunningGame() {
        try {
            return writer.submit(this::findAndAbortRunningGames).get(30, TimeUnit.SECONDS);
        } catch (Exception ex) {
            log.error("Could not look for running games of instance '{}': {}", instanceId, ex.getMessage(), ex);
            return Optional.empty();
        }
    }

    private Optional<GameSnapshot> findAndAbortRunningGames() {
        var rows = jdbc.sql("""
                        SELECT g.id, CAST(s.state AS VARCHAR) AS state
                        FROM game g LEFT JOIN game_snapshot s ON s.game_id = g.id
                        WHERE g.instance_id = ? AND g.status = 'IN_GAME'
                        ORDER BY g.started_at DESC""")
                .param(instanceId)
                .query((rs, i) -> new RunningGame(rs.getObject("id", UUID.class), rs.getString("state")))
                .list();

        GameSnapshot found = null;
        for (var row : rows) {
            if (found == null && row.state() != null) {
                try {
                    var snapshot = objectMapper.readValue(row.state(), GameSnapshot.class);
                    if (snapshot.formatVersion() == GameSnapshot.FORMAT_VERSION) {
                        found = snapshot;
                        continue;
                    }
                    log.warn("Snapshot of game {} has format {}, cannot continue it", row.id(), snapshot.formatVersion());
                } catch (Exception ex) {
                    log.warn("Snapshot of game {} is unreadable: {}", row.id(), ex.getMessage());
                }
            }
            log.info("Marking orphaned game {} as aborted", row.id());
            abort(row.id(), OffsetDateTime.now());
        }
        return Optional.ofNullable(found);
    }

    private record RunningGame(UUID id, String state) {
    }

    private void abort(UUID gameId, OffsetDateTime endedAt) {
        tx.executeWithoutResult(status -> {
            jdbc.sql("UPDATE game SET status = 'ABORTED', ended_at = ? WHERE id = ? AND status = 'IN_GAME'")
                    .params(endedAt, gameId)
                    .update();
            deleteSnapshot(gameId);
        });
    }

    // An ended game is never restored, so its snapshot is no longer needed
    private void deleteSnapshot(UUID gameId) {
        jdbc.sql("DELETE FROM game_snapshot WHERE game_id = ?").param(gameId).update();
    }

    @Override
    public void gameSnapshot(GameSnapshot snapshot) {
        if (pendingSnapshot.getAndSet(snapshot) != null) {
            return; // queued write will pick up the newer snapshot
        }
        submit("snapshot " + snapshot.gameId(), this::writePendingSnapshot);
    }

    private void writePendingSnapshot() {
        var s = pendingSnapshot.getAndSet(null);
        if (s == null) {
            return;
        }
        String json;
        try {
            json = objectMapper.writeValueAsString(s);
        } catch (Exception ex) {
            throw new IllegalStateException("Snapshot could not be serialized", ex);
        }
        var jsonExpr = jsonParameter();
        // H2 has no ON CONFLICT ... DO UPDATE; single writer thread, so update-then-insert is race-free
        int updated = jdbc.sql("UPDATE game_snapshot SET version = ?, updated_at = ?, state = " + jsonExpr
                        + " WHERE game_id = ? AND version < ?")
                .params(s.version(), s.takenAt(), json, s.gameId(), s.version())
                .update();
        if (updated == 0) {
            // only for running games, so a late snapshot cannot bring back the one deleted at game end
            jdbc.sql("INSERT INTO game_snapshot (game_id, version, updated_at, state) SELECT ?, ?, ?, " + jsonExpr
                            + " WHERE NOT EXISTS (SELECT 1 FROM game_snapshot WHERE game_id = ?)"
                            + " AND EXISTS (SELECT 1 FROM game WHERE id = ? AND status = 'IN_GAME')")
                    .params(s.gameId(), s.version(), s.takenAt(), json, s.gameId(), s.gameId())
                    .update();
        }
    }

    // H2 stores CAST(? AS JSONB) as a JSON string instead of an object
    private String jsonParameter() {
        if (jsonParameter == null) {
            String product;
            try {
                product = JdbcUtils.extractDatabaseMetaData(dataSource, DatabaseMetaData::getDatabaseProductName);
            } catch (Exception ex) {
                throw new IllegalStateException("Database type could not be determined", ex);
            }
            jsonParameter = "H2".equalsIgnoreCase(product) ? "? FORMAT JSON" : "CAST(? AS jsonb)";
        }
        return jsonParameter;
    }

    private void submit(String what, Runnable write) {
        try {
            writer.execute(() -> {
                try {
                    write.run();
                    log.debug("Recorded {}", what);
                } catch (Exception ex) {
                    log.error("Could not record {}: {}", what, ex.getMessage(), ex);
                }
            });
        } catch (Exception ex) {
            log.error("Could not queue {}: {}", what, ex.getMessage());
        }
    }

    /** Waits for queued writes, e.g. on shutdown, so a finished game is not lost. */
    @PreDestroy
    public void shutdown() throws InterruptedException {
        writer.shutdown();
        if (!writer.awaitTermination(10, TimeUnit.SECONDS)) {
            log.warn("Game recorder did not finish pending writes within 10 s");
        }
    }

    /** For tests: blocks until all writes queued so far are done. */
    void flush() throws Exception {
        writer.submit(() -> { }).get(10, TimeUnit.SECONDS);
    }

    private static String hostname() {
        try {
            return InetAddress.getLocalHost().getHostName();
        } catch (Exception ex) {
            return "unknown-host";
        }
    }
}
