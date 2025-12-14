package labyrinth.server.game.util;

import labyrinth.server.game.abstractions.IGameTimer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.TaskScheduler;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.concurrent.ScheduledFuture;

public class GameTimer implements IGameTimer {

    private static final Logger log = LoggerFactory.getLogger(GameTimer.class);

    private final TaskScheduler taskScheduler;
    private ScheduledFuture<?> currentTask;

    public GameTimer(TaskScheduler taskScheduler) {
        this.taskScheduler = taskScheduler;
    }

    @Override
    public synchronized void start(int seconds, Runnable onTimeout) {
        stop();
        this.currentTask = taskScheduler.schedule(() -> {
            try {
                onTimeout.run();
            } catch (Exception e) {
                log.error("Error in timer callback", e);
            }
        }, Instant.now().plusSeconds(seconds));
    }

    @Override
    public synchronized void stop() {
        if (currentTask != null) {
            currentTask.cancel(false);
        }
    }

    @Override
    public OffsetDateTime getExpirationTime() {
        if (currentTask == null) {
            return null;
        }
        var scheduledTime = currentTask.getDelay(java.util.concurrent.TimeUnit.MILLISECONDS);
        if (scheduledTime <= 0) {
            return null;
        }
        return OffsetDateTime.now().plusNanos(scheduledTime * 1_000_000);
    }
}

