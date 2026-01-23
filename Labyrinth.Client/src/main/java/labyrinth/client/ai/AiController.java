package labyrinth.client.ai;

import labyrinth.client.messaging.GameClient;
import labyrinth.client.util.Logger;
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

    private static final Logger log = Logger.getLogger(AiController.class);
    private final GameClient client;
    private final AiStrategy strategy;
    private final ExecutorService executor;

    private String localPlayerId;
    private final AtomicBoolean aiTurnInProgress = new AtomicBoolean(false);
    private final AtomicBoolean aiModeEnabled = new AtomicBoolean(false);
    private final AtomicBoolean stopped = new AtomicBoolean(false);

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
        log.info("[AI Controller] AI mode " + (newState ? "ENABLED" : "DISABLED"));

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
        log.info("[AI Controller] AI mode set to " + (enabled ? "ENABLED" : "DISABLED"));

        if (onAiModeChanged != null) {
            SwingUtilities.invokeLater(onAiModeChanged);
        }
    }

    /**
     * Stops the AI controller completely.
     * Called when game ends to prevent the AI from continuing to send moves.
     */
    public void stop() {
        log.info("[AI Controller] STOPPING - game ended");
        stopped.set(true);
        aiModeEnabled.set(false);
        aiTurnInProgress.set(false);

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
        log.info("[AI Controller] RESET - ready for new game");
        stopped.set(false);
    }

    private volatile Board latestBoard;
    private volatile List<Player> latestPlayers;
    private volatile CurrentTurnInfo latestTurnInfo;

    /**
     * Called when game state is updated.
     * If AI mode is enabled and it's the local player's turn, triggers AI automatically.
     */
    public void onGameStateUpdate(Board board, List<Player> players, CurrentTurnInfo turnInfo) {
        if (stopped.get()) {
            log.info("[AI Controller] Ignoring state update - AI is stopped");
            return;
        }

        this.latestBoard = board;
        this.latestPlayers = players;
        this.latestTurnInfo = turnInfo;

        if (!aiModeEnabled.get()) {
            return; 
        }

        if (board == null || players == null || turnInfo == null) {
            log.info("[AI Controller] Skipping - null state");
            return;
        }

        if (localPlayerId == null || !localPlayerId.equals(turnInfo.getCurrentPlayerId())) {
            log.info("[AI Controller] Not my turn. Local: " + localPlayerId + ", Current: " + turnInfo.getCurrentPlayerId());
            return;
        }

        Player localPlayer = findPlayerById(localPlayerId, players);
        if (localPlayer == null) {
            log.info("[AI Controller] Local player not found in players list");
            return;
        }

        if (aiTurnInProgress.compareAndSet(false, true)) {
            log.info("[AI Controller] Auto-triggering AI for: " + localPlayer.getName() + " (State: " + turnInfo.getState() + ")");
            executeAiTurn(board, localPlayer, turnInfo.getState() == TurnState.WAITING_FOR_PUSH);
        } else {
            log.info("[AI Controller] AI already in progress, skipping");
        }
    }

    /**
     * Re-triggers AI with latest stored state.
     * Called after AI completes a move to check if it's still our turn.
     */
    private void retriggerIfStillMyTurn() {
        if (stopped.get() || !aiModeEnabled.get()) return;

        executor.submit(() -> {
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                return;
            }

            if (stopped.get()) {
                log.info("[AI Controller] Re-trigger cancelled - AI is stopped");
                return;
            }

            Board board = latestBoard;
            List<Player> players = latestPlayers;
            CurrentTurnInfo turnInfo = latestTurnInfo;

            if (board == null || players == null || turnInfo == null) return;
            if (localPlayerId == null || !localPlayerId.equals(turnInfo.getCurrentPlayerId())) return;

            Player localPlayer = findPlayerById(localPlayerId, players);
            if (localPlayer == null) return;

            if (aiTurnInProgress.compareAndSet(false, true)) {
                log.info("[AI Controller] Re-triggering AI - still my turn (State: " + turnInfo.getState() + ")");
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
                if (stopped.get()) {
                    log.info("[AI] Turn cancelled - AI is stopped");
                    return;
                }

                notifyThinkingStart();

                log.info("[AI] Starting turn for: " + player.getName() + " (needsShift: " + needsShift + ")");

                if (needsShift) {
                    executeShiftPhase(board, player);

                    Thread.sleep(400);

                    aiTurnInProgress.set(false);
                    notifyThinkingEnd();

                    log.info("[AI] Shift done, waiting for server state update for move phase");
                    return; 
                }

                executeMovePhase(board, player, latestPlayers);

                log.info("[AI] Turn completed for: " + player.getName());

            } catch (Exception e) {
                log.error("[AI] Error during AI turn: " + e.getMessage());
                e.printStackTrace();
            } finally {
                aiTurnInProgress.set(false);
                notifyThinkingEnd();

                retriggerIfStillMyTurn();
            }
        });
    }

    private volatile AiDecision currentDecision;

    private void executeShiftPhase(Board board, Player player) throws InterruptedException {
        AiDecision decision;
        if (strategy instanceof SimpleAiStrategy simpleStrategy) {
            decision = simpleStrategy.computeBestMove(board, player, latestPlayers);
        } else {
            decision = strategy.computeBestMove(board, player);
        }

        if (decision == null) {
            log.info("[AI] No valid move found, using fallback");
            client.sendPushTile(1, labyrinth.contracts.models.Direction.RIGHT);
            currentDecision = null;
            return;
        }

        currentDecision = decision;

        if (decision.bonusAction() != null) {
            BonusType bonusType = decision.bonusAction().bonusType();

            if (bonusType == BonusType.BEAM) {
                Position beamTarget = decision.bonusAction().targetPosition();
                log.info("[AI] Using BEAM (replaces push) to " + beamTarget.getRow() + "/" + beamTarget.getColumn());
                client.sendUseBeam(beamTarget.getRow(), beamTarget.getColumn());
                return; 
            }

            if (bonusType == BonusType.SWAP) {
                String targetPlayerId = decision.bonusAction().targetPlayerId();
                log.info("[AI] Using SWAP (replaces push) with player " + targetPlayerId);
                client.sendUseSwap(targetPlayerId);
                return; 
            }
        }

        if (decision.bonusAction() != null && decision.bonusAction().bonusType() == BonusType.PUSH_TWICE) {
            log.info("[AI] Activating PUSH_TWICE bonus");
            client.sendUsePushTwice();
            Thread.sleep(300);
        }

        if (decision.rotations() > 0) {
            log.info("[AI] Rotating extra tile " + decision.rotations() + " times");
            for (int i = 0; i < decision.rotations(); i++) {
                client.sendRotateTile();
                Thread.sleep(100);
            }
        }

        Thread.sleep(150);

        if (decision.shift() != null) {
            log.info("[AI] Shifting " + (decision.shift().isRow() ? "row" : "column") +
                    " " + decision.shift().index() + " " + decision.shift().direction());
            client.sendPushTile(decision.shift().index(), decision.shift().direction());
        }

        if (decision.bonusAction() != null && decision.bonusAction().bonusType() == BonusType.PUSH_TWICE) {
            Thread.sleep(400); 

            SimulationResult.BonusAction bonus = decision.bonusAction();
            ShiftOperation secondOp = bonus.secondPush();

            if (bonus.secondPushRotations() > 0) {
                log.info("[AI] Rotating for second push " + bonus.secondPushRotations() + " times");
                for (int i = 0; i < bonus.secondPushRotations(); i++) {
                    client.sendRotateTile();
                    Thread.sleep(100);
                }
            }

            Thread.sleep(150);

            log.info("[AI] Executing second push: " + (secondOp.isRow() ? "row" : "column") +
                    " " + secondOp.index() + " " + secondOp.direction());
            client.sendPushTile(secondOp.index(), secondOp.direction());
        }

        if (decision.bonusAction() != null && decision.bonusAction().bonusType() == BonusType.PUSH_FIXED) {
            SimulationResult.BonusAction bonus = decision.bonusAction();
            ShiftOperation fixedOp = bonus.pushFixedOp();

            log.info("[AI] Using PUSH_FIXED: " + (fixedOp.isRow() ? "row" : "column") +
                    " " + fixedOp.index() + " " + fixedOp.direction());
            client.sendUsePushFixed(fixedOp.index(), fixedOp.direction());
        }
    }

    private void executeMovePhase(Board board, Player player, List<Player> allPlayers) throws InterruptedException {
        AiDecision decision = currentDecision;

        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        java.util.Set<Position> reachable = sim.getReachableUnblockedPositions();
        Position targetPos = sim.getTargetPosition();
        Position currentPos = sim.getPlayerPosition();
        java.util.Set<Position> blockedPositions = sim.getOtherPlayerPositions();

        log.info("[AI] Move phase: " + reachable.size() + " reachable positions (excl. " +
                blockedPositions.size() + " blocked), target: " +
                (targetPos != null ? targetPos.getRow() + "/" + targetPos.getColumn() : "none"));

        Position bestMove = null;

        if (decision != null && decision.targetPosition() != null) {
            Position decisionTarget = decision.targetPosition();
            if (reachable.contains(decisionTarget)) {
                bestMove = decisionTarget;
                log.info("[AI] Using decision target position");
            }
        }

        if (bestMove == null) {
            if (targetPos != null && reachable.contains(targetPos)) {
                bestMove = targetPos;
                log.info("[AI] Target is directly reachable!");
            } else if (targetPos != null) {
                if (blockedPositions.contains(targetPos)) {
                    log.info("[AI] Target is blocked by another player!");
                }
                bestMove = findBestMoveWithSteps(reachable, targetPos, currentPos);
                if (bestMove != null) {
                    int dist = BoardSimulator.manhattanDistance(bestMove, targetPos);
                    int steps = BoardSimulator.manhattanDistance(currentPos, bestMove);
                    log.info("[AI] Moving closer to target, distance: " + dist + ", steps: " + steps);
                }
            } else if (!reachable.isEmpty()) {
                bestMove = findFarthestPosition(reachable, currentPos);
                log.info("[AI] No target, maximizing steps");
            }
        }

        if (bestMove == null) {
            bestMove = player.getCurrentPosition();
            log.info("[AI] No better move found, staying in place");
        }

        Thread.sleep(150);

        log.info("[AI] Moving to " + bestMove.getRow() + "/" + bestMove.getColumn());
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
