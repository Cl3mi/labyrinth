package labyrinth.client.ai;

import labyrinth.client.messaging.GameClient;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
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
    private final AtomicBoolean stopped = new AtomicBoolean(false);

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
     * Stops the AI controller completely.
     * Called when game ends to prevent the AI from continuing to send moves.
     */
    public void stop() {
        System.out.println("[AI Controller] STOPPING - game ended");
        stopped.set(true);
        aiModeEnabled.set(false);
        aiTurnInProgress.set(false);

        // Clear stored state to prevent re-triggering
        latestBoard = null;
        latestPlayers = null;
        latestTurnInfo = null;

        if (onAiModeChanged != null) {
            SwingUtilities.invokeLater(onAiModeChanged);
        }
    }

    /**
     * Resets the stopped flag. Called when starting a new game.
     */
    public void reset() {
        System.out.println("[AI Controller] RESET - ready for new game");
        stopped.set(false);
    }

    // Store latest state for re-triggering
    private volatile Board latestBoard;
    private volatile List<Player> latestPlayers;
    private volatile CurrentTurnInfo latestTurnInfo;

    /**
     * Called when game state is updated.
     * If AI mode is enabled and it's the local player's turn, triggers AI automatically.
     */
    public void onGameStateUpdate(Board board, List<Player> players, CurrentTurnInfo turnInfo) {
        // Check if stopped (game ended)
        if (stopped.get()) {
            System.out.println("[AI Controller] Ignoring state update - AI is stopped");
            return;
        }

        // Always store latest state
        this.latestBoard = board;
        this.latestPlayers = players;
        this.latestTurnInfo = turnInfo;

        if (!aiModeEnabled.get()) {
            return; // AI mode not enabled
        }

        if (board == null || players == null || turnInfo == null) {
            System.out.println("[AI Controller] Skipping - null state");
            return;
        }

        // Check if it's the local player's turn
        if (localPlayerId == null || !localPlayerId.equals(turnInfo.getCurrentPlayerId())) {
            System.out.println("[AI Controller] Not my turn. Local: " + localPlayerId + ", Current: " + turnInfo.getCurrentPlayerId());
            return;
        }

        // Find the local player
        Player localPlayer = findPlayerById(localPlayerId, players);
        if (localPlayer == null) {
            System.out.println("[AI Controller] Local player not found in players list");
            return;
        }

        // Trigger AI if not already in progress
        if (aiTurnInProgress.compareAndSet(false, true)) {
            System.out.println("[AI Controller] Auto-triggering AI for: " + localPlayer.getName() + " (State: " + turnInfo.getState() + ")");
            executeAiTurn(board, localPlayer, turnInfo.getState() == TurnState.WAITING_FOR_PUSH);
        } else {
            System.out.println("[AI Controller] AI already in progress, skipping");
        }
    }

    /**
     * Re-triggers AI with latest stored state.
     * Called after AI completes a move to check if it's still our turn.
     */
    private void retriggerIfStillMyTurn() {
        // Check if stopped (game ended) or AI mode disabled
        if (stopped.get() || !aiModeEnabled.get()) return;

        // Small delay to let server state update arrive
        executor.submit(() -> {
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                return;
            }

            // Re-check stopped flag after sleep (game may have ended while waiting)
            if (stopped.get()) {
                System.out.println("[AI Controller] Re-trigger cancelled - AI is stopped");
                return;
            }

            Board board = latestBoard;
            List<Player> players = latestPlayers;
            CurrentTurnInfo turnInfo = latestTurnInfo;

            if (board == null || players == null || turnInfo == null) return;
            if (localPlayerId == null || !localPlayerId.equals(turnInfo.getCurrentPlayerId())) return;

            Player localPlayer = findPlayerById(localPlayerId, players);
            if (localPlayer == null) return;

            // Check if we need to do something (still our turn, e.g., need to move after push)
            if (aiTurnInProgress.compareAndSet(false, true)) {
                System.out.println("[AI Controller] Re-triggering AI - still my turn (State: " + turnInfo.getState() + ")");
                executeAiTurn(board, localPlayer, turnInfo.getState() == TurnState.WAITING_FOR_PUSH);
            }
        });
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
                // Check if stopped before starting
                if (stopped.get()) {
                    System.out.println("[AI] Turn cancelled - AI is stopped");
                    return;
                }

                // Notify UI that AI is thinking
                notifyThinkingStart();

                System.out.println("[AI] Starting turn for: " + player.getName() + " (needsShift: " + needsShift + ")");

                if (needsShift) {
                    // Phase 1: Compute and execute shift
                    executeShiftPhase(board, player);

                    // Wait for server to process and send new state
                    Thread.sleep(400);

                    // After shift, we need to move - but server will send new state
                    // Set flag to false so new state update can trigger move
                    aiTurnInProgress.set(false);
                    notifyThinkingEnd();

                    System.out.println("[AI] Shift done, waiting for server state update for move phase");
                    return; // Let onGameStateUpdate handle the move phase
                }

                // Phase 2: Execute move (only if no shift needed, i.e., already in WAITING_FOR_MOVE state)
                executeMovePhase(board, player, latestPlayers);

                System.out.println("[AI] Turn completed for: " + player.getName());

            } catch (Exception e) {
                System.err.println("[AI] Error during AI turn: " + e.getMessage());
                e.printStackTrace();
            } finally {
                aiTurnInProgress.set(false);
                notifyThinkingEnd();

                // Check if it's still our turn (e.g., game might loop back to us)
                retriggerIfStillMyTurn();
            }
        });
    }

    // Store the current decision for bonus handling in move phase
    private volatile AiDecision currentDecision;

    private void executeShiftPhase(Board board, Player player) throws InterruptedException {
        // Compute best move with all players info
        AiDecision decision;
        if (strategy instanceof SimpleAiStrategy simpleStrategy) {
            decision = simpleStrategy.computeBestMove(board, player, latestPlayers);
        } else {
            decision = strategy.computeBestMove(board, player);
        }

        if (decision == null) {
            System.out.println("[AI] No valid move found, using fallback");
            // Fallback: push row 1 to the right
            client.sendPushTile(1, labyrinth.contracts.models.Direction.RIGHT);
            currentDecision = null;
            return;
        }

        // Store decision for move phase (bonus handling)
        currentDecision = decision;

        // Check if BEAM or SWAP is used - these REPLACE the push entirely
        if (decision.bonusAction() != null) {
            BonusType bonusType = decision.bonusAction().bonusType();

            if (bonusType == BonusType.BEAM) {
                // BEAM replaces push - teleport to target position
                Position beamTarget = decision.bonusAction().targetPosition();
                System.out.println("[AI] Using BEAM (replaces push) to " + beamTarget.getRow() + "/" + beamTarget.getColumn());
                client.sendUseBeam(beamTarget.getRow(), beamTarget.getColumn());
                return; // No shift needed - BEAM replaces it
            }

            if (bonusType == BonusType.SWAP) {
                // SWAP replaces push - swap with target player
                String targetPlayerId = decision.bonusAction().targetPlayerId();
                System.out.println("[AI] Using SWAP (replaces push) with player " + targetPlayerId);
                client.sendUseSwap(targetPlayerId);
                return; // No shift needed - SWAP replaces it
            }
        }

        // Normal push flow (or with PUSH_TWICE/PUSH_FIXED modifiers)

        // Check if we need to activate PUSH_TWICE before the shift
        if (decision.bonusAction() != null && decision.bonusAction().bonusType() == BonusType.PUSH_TWICE) {
            System.out.println("[AI] Activating PUSH_TWICE bonus");
            client.sendUsePushTwice();
            Thread.sleep(300);
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

        // Execute shift (only if we have a shift operation)
        if (decision.shift() != null) {
            System.out.println("[AI] Shifting " + (decision.shift().isRow() ? "row" : "column") +
                    " " + decision.shift().index() + " " + decision.shift().direction());
            client.sendPushTile(decision.shift().index(), decision.shift().direction());
        }

        // Handle PUSH_TWICE second shift
        if (decision.bonusAction() != null && decision.bonusAction().bonusType() == BonusType.PUSH_TWICE) {
            Thread.sleep(400); // Wait for first shift to complete

            SimulationResult.BonusAction bonus = decision.bonusAction();
            ShiftOperation secondOp = bonus.secondPush();

            // Apply rotations for second push
            if (bonus.secondPushRotations() > 0) {
                System.out.println("[AI] Rotating for second push " + bonus.secondPushRotations() + " times");
                for (int i = 0; i < bonus.secondPushRotations(); i++) {
                    client.sendRotateTile();
                    Thread.sleep(100);
                }
            }

            Thread.sleep(150);

            System.out.println("[AI] Executing second push: " + (secondOp.isRow() ? "row" : "column") +
                    " " + secondOp.index() + " " + secondOp.direction());
            client.sendPushTile(secondOp.index(), secondOp.direction());
        }

        // Handle PUSH_FIXED bonus - this IS the push (not after a normal push)
        if (decision.bonusAction() != null && decision.bonusAction().bonusType() == BonusType.PUSH_FIXED) {
            SimulationResult.BonusAction bonus = decision.bonusAction();
            ShiftOperation fixedOp = bonus.pushFixedOp();

            System.out.println("[AI] Using PUSH_FIXED: " + (fixedOp.isRow() ? "row" : "column") +
                    " " + fixedOp.index() + " " + fixedOp.direction());
            client.sendUsePushFixed(fixedOp.index(), fixedOp.direction());
        }
    }

    private void executeMovePhase(Board board, Player player, List<Player> allPlayers) throws InterruptedException {
        AiDecision decision = currentDecision;

        // Note: BEAM and SWAP are now handled in shift phase (they replace the push)
        // So we just need to move here

        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        java.util.Set<Position> reachable = sim.getReachableUnblockedPositions();
        Position targetPos = sim.getTargetPosition();
        Position currentPos = sim.getPlayerPosition();
        java.util.Set<Position> blockedPositions = sim.getOtherPlayerPositions();

        System.out.println("[AI] Move phase: " + reachable.size() + " reachable positions (excl. " +
                blockedPositions.size() + " blocked), target: " +
                (targetPos != null ? targetPos.getRow() + "/" + targetPos.getColumn() : "none"));

        Position bestMove = null;

        if (decision != null && decision.targetPosition() != null) {
            Position decisionTarget = decision.targetPosition();
            if (reachable.contains(decisionTarget)) {
                bestMove = decisionTarget;
                System.out.println("[AI] Using decision target position");
            }
        }

        if (bestMove == null) {
            if (targetPos != null && reachable.contains(targetPos)) {
                bestMove = targetPos;
                System.out.println("[AI] Target is directly reachable!");
            } else if (targetPos != null) {
                if (blockedPositions.contains(targetPos)) {
                    System.out.println("[AI] Target is blocked by another player!");
                }
                bestMove = findBestMoveWithSteps(reachable, targetPos, currentPos);
                if (bestMove != null) {
                    int dist = BoardSimulator.manhattanDistance(bestMove, targetPos);
                    int steps = BoardSimulator.manhattanDistance(currentPos, bestMove);
                    System.out.println("[AI] Moving closer to target, distance: " + dist + ", steps: " + steps);
                }
            } else if (!reachable.isEmpty()) {
                bestMove = findFarthestPosition(reachable, currentPos);
                System.out.println("[AI] No target, maximizing steps");
            }
        }

        if (bestMove == null) {
            bestMove = player.getCurrentPosition();
            System.out.println("[AI] No better move found, staying in place");
        }

        Thread.sleep(150);

        System.out.println("[AI] Moving to " + bestMove.getRow() + "/" + bestMove.getColumn());
        client.sendMovePawn(bestMove.getRow(), bestMove.getColumn());

        currentDecision = null;
    }

    private Position findBestMoveWithSteps(java.util.Set<Position> reachable, Position target, Position current) {
        Position best = null;
        double bestScore = Double.NEGATIVE_INFINITY;
        final double DISTANCE_WEIGHT = 3.0;
        final double STEPS_WEIGHT = 1.0;

        for (Position pos : reachable) {
            int distToTarget = BoardSimulator.manhattanDistance(pos, target);
            int steps = BoardSimulator.manhattanDistance(current, pos);
            double score = -distToTarget * DISTANCE_WEIGHT + steps * STEPS_WEIGHT;
            if (score > bestScore) {
                bestScore = score;
                best = pos;
            }
        }
        return best;
    }

    private Position findFarthestPosition(java.util.Set<Position> reachable, Position current) {
        Position farthest = null;
        int maxDist = -1;
        for (Position pos : reachable) {
            int dist = BoardSimulator.manhattanDistance(pos, current);
            if (dist > maxDist) {
                maxDist = dist;
                farthest = pos;
            }
        }
        return farthest != null ? farthest : reachable.iterator().next();
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
