package labyrinth.client.messaging;

import labyrinth.client.models.LabyrinthApplication;
import labyrinth.client.util.Logger;
import labyrinth.client.models.LabyrinthApplication.ClientIdentityStore;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Manages automatic reconnection attempts with exponential backoff.
 *
 * Features:
 * - Exponential backoff: 1s, 2s, 4s, 8s, 16s
 * - Configurable max attempts (default: 5)
 * - Cancellable reconnection
 * - UI status updates
 * - Manual reconnect fallback dialog
 */
public class ReconnectionManager {

    private static final Logger log = Logger.getLogger(ReconnectionManager.class);
    private final GameClient client;
    private final LabyrinthApplication application;
    private final ScheduledExecutorService scheduler;
    private final AtomicInteger attemptCount;
    private final AtomicBoolean cancelled;
    private volatile ScheduledFuture<?> currentReconnectTask;


    private static final int MAX_RECONNECT_ATTEMPTS = 5;
    private static final int BASE_DELAY_MS = 1000; // 1 second
    private static final int MAX_DELAY_MS = 16000; // 16 seconds

    public ReconnectionManager(GameClient client, LabyrinthApplication application) {
        this.client = client;
        this.application = application;
        this.scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "ReconnectionScheduler");
            t.setDaemon(true);
            return t;
        });
        this.attemptCount = new AtomicInteger(0);
        this.cancelled = new AtomicBoolean(false);
    }

    /**
     * Starts automatic reconnection sequence.
     * Resets attempt counter and schedules first attempt immediately.
     */
    public void startAutoReconnect() {
        cancelReconnection();

        attemptCount.set(0);
        cancelled.set(false);

        log.info("[ReconnectionManager] Starting auto-reconnect sequence");

        currentReconnectTask = scheduler.schedule(
            this::performReconnectAttempt,
            500,
            TimeUnit.MILLISECONDS
        );
    }

    /**
     * Cancels ongoing reconnection attempts.
     */
    public void cancelReconnection() {
        cancelled.set(true);

        if (currentReconnectTask != null && !currentReconnectTask.isDone()) {
            currentReconnectTask.cancel(false);
            log.info("[ReconnectionManager] Reconnection cancelled");
        }
    }

    /**
     * Resets the reconnection state (called on successful connection).
     */
    public void reset() {
        attemptCount.set(0);
        cancelled.set(false);

        if (currentReconnectTask != null && !currentReconnectTask.isDone()) {
            currentReconnectTask.cancel(false);
        }
    }

    /**
     * Shuts down the scheduler (called on application exit).
     */
    public void shutdown() {
        cancelReconnection();
        scheduler.shutdown();

        try {
            if (!scheduler.awaitTermination(2, TimeUnit.SECONDS)) {
                scheduler.shutdownNow();
            }
        } catch (InterruptedException e) {
            scheduler.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    /**
     * Performs a single reconnection attempt.
     */
    private void performReconnectAttempt() {
        if (cancelled.get()) {
            log.info("[ReconnectionManager] Attempt cancelled");
            return;
        }

        int currentAttempt = attemptCount.incrementAndGet();
        log.info("[ReconnectionManager] Reconnection attempt " + currentAttempt + "/" + MAX_RECONNECT_ATTEMPTS);

        String token = ClientIdentityStore.loadToken();

        if (token == null || token.isBlank()) {
            log.info("[ReconnectionManager] No token available - cannot auto-reconnect");
            application.showManualReconnectDialog();
            return;
        }

        boolean success = client.attemptReconnect(token);

        if (!success) {
            handleReconnectFailure(currentAttempt);
        }

    }

    /**
     * Handles a failed reconnection attempt.
     * Either schedules next attempt or shows manual reconnect dialog.
     */
    private void handleReconnectFailure(int attemptNumber) {
        if (cancelled.get()) {
            return;
        }

        if (attemptNumber >= MAX_RECONNECT_ATTEMPTS) {
            log.info("[ReconnectionManager] Max reconnect attempts reached - showing manual dialog");
            application.showManualReconnectDialog();
        } else {
            int delayMs = calculateDelay(attemptNumber);
            int delaySeconds = delayMs / 1000;

            log.info("[ReconnectionManager] Scheduling next attempt in " + delaySeconds + "s");

            application.updateReconnectionStatus(attemptNumber, MAX_RECONNECT_ATTEMPTS, delaySeconds);

            currentReconnectTask = scheduler.schedule(
                this::performReconnectAttempt,
                delayMs,
                TimeUnit.MILLISECONDS
            );
        }
    }

    /**
     * Calculates exponential backoff delay.
     * Formula: min(BASE_DELAY_MS * 2^(attempt - 1), MAX_DELAY_MS)
     *
     * Attempt 1: 1000ms (1s)
     * Attempt 2: 2000ms (2s)
     * Attempt 3: 4000ms (4s)
     * Attempt 4: 8000ms (8s)
     * Attempt 5: 16000ms (16s)
     */
    private int calculateDelay(int attemptNumber) {
        if (attemptNumber <= 0) {
            return BASE_DELAY_MS;
        }

        long delay = BASE_DELAY_MS * (1L << (attemptNumber - 1));
        return (int) Math.min(delay, MAX_DELAY_MS);
    }
}
