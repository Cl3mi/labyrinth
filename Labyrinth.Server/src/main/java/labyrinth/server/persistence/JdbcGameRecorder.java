package labyrinth.server.persistence;

import jakarta.annotation.PreDestroy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionTemplate;

import java.net.InetAddress;
import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

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

    private final JdbcClient jdbc;
    private final TransactionTemplate tx;
    private final String instanceId;
    private final String serverName;
    private final ExecutorService writer = Executors.newSingleThreadExecutor(r -> {
        var t = new Thread(r, "game-recorder");
        t.setDaemon(true);
        return t;
    });

    public JdbcGameRecorder(JdbcClient jdbc,
                            TransactionTemplate tx,
                            @Value("${labyrinth.instance-id:}") String instanceId,
                            @Value("${server.name:Random Server}") String serverName,
                            @Value("${server.port:8080}") int serverPort) {
        this.jdbc = jdbc;
        this.tx = tx;
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
        }));
    }

    @Override
    public void gameAborted(UUID gameId, OffsetDateTime endedAt) {
        submit("game aborted " + gameId, () -> jdbc.sql(
                        "UPDATE game SET status = 'ABORTED', ended_at = ? WHERE id = ? AND status = 'IN_GAME'")
                .params(endedAt, gameId)
                .update());
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
