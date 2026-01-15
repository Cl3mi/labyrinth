package labyrinth.client.ai;

import labyrinth.client.messaging.GameClient;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.CurrentTurnInfo;
import labyrinth.contracts.models.TurnState;

import javax.swing.SwingUtilities;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Controller for AI move execution.
 * Supports toggle mode - when enabled, AI automatically makes moves for the local player.
 *
 * Note: Server-side AI handles disconnected players and AI fillers.
 */
public class AiController {

    private final GameClient client;
    private final AiStrategy strategy;
    private final ExecutorService executor;

    private String localPlayerId;
    private final AtomicBoolean aiTurnInProgress = new AtomicBoolean(false);
    private final AtomicBoolean aiModeEnabled = new AtomicBoolean(false);

    // Callback for UI feedback
    private Runnable onAiThinkingStart;
    private Runnable onAiThinkingEnd;
    private Runnable onAiModeChanged;

    public AiController(GameClient client) {
        this.client = client;
        this.strategy = new SimpleAiStrategy();
        this.executor = Executors.newSingleThreadExecutor(r -> {
            Thread t = new Thread(r, "AI-Controller");
            t.setDaemon(true);
            return t;
        });
    }

    /**
     * Sets the local player ID for determining AI triggers.
     */
    public void setLocalPlayerId(String playerId) {
        this.localPlayerId = playerId;
    }

    /**
     * Sets callbacks for AI thinking state changes.
     */
    public void setThinkingCallbacks(Runnable onStart, Runnable onEnd) {
        this.onAiThinkingStart = onStart;
        this.onAiThinkingEnd = onEnd;
    }

    /**
     * Sets callback for AI mode toggle changes.
     */
    public void setOnAiModeChanged(Runnable callback) {
        this.onAiModeChanged = callback;
    }

    /**
     * Toggles AI mode on/off.
     * When enabled, AI will automatically make moves when it's the local player's turn.
     */
    public void toggleAiMode() {
        boolean newState = !aiModeEnabled.get();
        aiModeEnabled.set(newState);
        System.out.println("[AI Controller] AI mode " + (newState ? "ENABLED" : "DISABLED"));

        if (onAiModeChanged != null) {
            SwingUtilities.invokeLater(onAiModeChanged);
        }
    }

    /**
     * Checks if AI mode is currently enabled.
     */
    public boolean isAiModeEnabled() {
        return aiModeEnabled.get();
    }

    /**
     * Sets AI mode directly.
     */
    public void setAiModeEnabled(boolean enabled) {
        aiModeEnabled.set(enabled);
        System.out.println("[AI Controller] AI mode set to " + (enabled ? "ENABLED" : "DISABLED"));

        if (onAiModeChanged != null) {
            SwingUtilities.invokeLater(onAiModeChanged);
        }
    }

    /**
     * Called when game state is updated.
     * If AI mode is enabled and it's the local player's turn, triggers AI automatically.
     */
    public void onGameStateUpdate(Board board, List<Player> players, CurrentTurnInfo turnInfo) {
        if (!aiModeEnabled.get()) {
            return; // AI mode not enabled
        }

        if (board == null || players == null || turnInfo == null) {
            return;
        }

        // Check if it's the local player's turn
        if (!localPlayerId.equals(turnInfo.getCurrentPlayerId())) {
            return;
        }

        // Find the local player
        Player localPlayer = findPlayerById(localPlayerId, players);
        if (localPlayer == null) {
            return;
        }

        // Trigger AI if not already in progress
        if (aiTurnInProgress.compareAndSet(false, true)) {
            System.out.println("[AI Controller] Auto-triggering AI for: " + localPlayer.getName());
            executeAiTurn(board, localPlayer, turnInfo.getState() == TurnState.WAITING_FOR_PUSH);
        }
    }

    /**
     * Checks if AI is currently thinking.
     */
    public boolean isAiThinking() {
        return aiTurnInProgress.get();
    }

    private void executeAiTurn(Board board, Player player, boolean needsShift) {
        executor.submit(() -> {
            try {
                // Notify UI that AI is thinking
                notifyThinkingStart();

                System.out.println("[AI] Starting turn for: " + player.getName());

                if (needsShift) {
                    // Phase 1: Compute and execute shift
                    executeShiftPhase(board, player);

                    // Wait for server to process
                    Thread.sleep(300);
                }

                // Phase 2: Execute move
                executeMovePhase(board, player);

                System.out.println("[AI] Turn completed for: " + player.getName());

            } catch (Exception e) {
                System.err.println("[AI] Error during AI turn: " + e.getMessage());
                e.printStackTrace();
            } finally {
                aiTurnInProgress.set(false);
                notifyThinkingEnd();
            }
        });
    }

    private void executeShiftPhase(Board board, Player player) throws InterruptedException {
        // Compute best move
        AiDecision decision = strategy.computeBestMove(board, player);

        if (decision == null) {
            System.out.println("[AI] No valid move found, using fallback");
            // Fallback: push row 1 to the right
            client.sendPushTile(1, labyrinth.contracts.models.Direction.RIGHT);
            return;
        }

        // Apply rotations first
        if (decision.rotations() > 0) {
            System.out.println("[AI] Rotating extra tile " + decision.rotations() + " times");
            for (int i = 0; i < decision.rotations(); i++) {
                client.sendRotateTile();
                Thread.sleep(100);
            }
        }

        // Small delay for natural feel
        Thread.sleep(150);

        // Execute shift
        System.out.println("[AI] Shifting " + (decision.shift().isRow() ? "row" : "column") +
                " " + decision.shift().index() + " " + decision.shift().direction());
        client.sendPushTile(decision.shift().index(), decision.shift().direction());
    }

    private void executeMovePhase(Board board, Player player) throws InterruptedException {
        // Recompute best move after shift (board state has changed)
        AiDecision decision = strategy.computeBestMove(board, player);
        Position targetPos = decision != null ? decision.targetPosition() : player.getCurrentPosition();

        if (targetPos == null) {
            targetPos = player.getCurrentPosition();
        }

        Thread.sleep(150);

        System.out.println("[AI] Moving to " + targetPos.getRow() + "/" + targetPos.getColumn());
        client.sendMovePawn(targetPos.getRow(), targetPos.getColumn());
    }

    private Player findPlayerById(String playerId, List<Player> players) {
        if (playerId == null || players == null) return null;
        return players.stream()
                .filter(p -> playerId.equals(p.getId()))
                .findFirst()
                .orElse(null);
    }

    private void notifyThinkingStart() {
        if (onAiThinkingStart != null) {
            SwingUtilities.invokeLater(onAiThinkingStart);
        }
    }

    private void notifyThinkingEnd() {
        if (onAiThinkingEnd != null) {
            SwingUtilities.invokeLater(onAiThinkingEnd);
        }
    }

    /**
     * Shuts down the AI controller.
     */
    public void shutdown() {
        executor.shutdownNow();
    }
}
