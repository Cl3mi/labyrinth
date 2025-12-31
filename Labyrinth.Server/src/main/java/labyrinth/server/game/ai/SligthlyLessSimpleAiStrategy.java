package labyrinth.server.game.ai;

import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.Position;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class SligthlyLessSimpleAiStrategy implements AiStrategy {

    private final java.util.Random random = new java.util.Random();

    @Override
    public void performTurn(Game game, Player realPlayer) {
        System.out.println("AI performing turn for " + realPlayer.getUsername());

        // The "Async/Await" Pipeline
        calculateBestMoveAsync(game, realPlayer)
                .thenCompose(result -> delay(randomDelay(), result))    // await delay
                .thenApply(result   -> executeShift(game, realPlayer, result)) // synchronous logic
                .thenCompose(result -> delay(randomDelay(), result))    // await delay
                .thenAccept(result  -> executeMove(game, realPlayer, result))  // synchronous logic
                .exceptionally(ex -> {
                    ex.printStackTrace();
                    return null;
                });
    }

    // --- Step 1: Calculation (Async) ---
    private CompletableFuture<SimulationResult> calculateBestMoveAsync(Game game, Player player) {
        return CompletableFuture.supplyAsync(() -> {
            TreasureCard targetCard = player.getAssignedTreasureCards().isEmpty()
                    ? null
                    : player.getCurrentTreasureCard();
            return findBestMove(game, player, targetCard);
        });
    }

    // --- Step 2: Shift Logic (Synchronous) ---
    private SimulationResult executeShift(Game game, Player player, SimulationResult result) {
        if (result == null) {
            forceRandomShift(game, player);
            return null;
        }

        System.out.println("AI executing shift: " + result.shiftType + " index " + result.shiftIndex);

        var shiftResult = switch (result.shiftType) {
            case UP    -> game.shift(result.shiftIndex, Direction.UP, player);
            case DOWN  -> game.shift(result.shiftIndex, Direction.DOWN, player);
            case LEFT  -> game.shift(result.shiftIndex, Direction.LEFT, player);
            case RIGHT -> game.shift(result.shiftIndex, Direction.RIGHT, player);
        };

        if (!shiftResult.shiftSuccess()) {
            forceRandomShift(game, player);
        }

        return result; // Pass result to next step
    }

    // --- Step 3: Move Logic (Synchronous) ---
    private void executeMove(Game game, Player player, SimulationResult result) {
        Position current = game.getCurrentPositionOfPlayer(player);

        if (result != null && result.targetPosition != null) {
            System.out.println("AI moving to: " + result.targetPosition.row() + "/" + result.targetPosition.column());

            boolean success = game.movePlayerToTile(
                    result.targetPosition.row(),
                    result.targetPosition.column(),
                    player
            ).moveSuccess();

            if (!success) {
                // Fallback: stay put (explicitly move to current pos to trigger turn end if required by engine)
                game.movePlayerToTile(current.row(), current.column(), player);
            }
        } else {
            // No valid move found, stay put
            game.movePlayerToTile(current.row(), current.column(), player);
        }
    }

    // --- Helpers ---

    /** * Simulates 'await Task.Delay(ms)' but passes the data through.
     */
    private <T> CompletableFuture<T> delay(int millis, T dataToPass) {
        return CompletableFuture.runAsync(() -> {},
                CompletableFuture.delayedExecutor(millis, TimeUnit.MILLISECONDS)
        ).thenApply(v -> dataToPass);
    }

    private int randomDelay() {
        return 100 + random.nextInt(100);
    }

    private void forceRandomShift(Game game, Player player) {
        try {
            if (!game.shift(1, Direction.RIGHT, player).shiftSuccess()) {
                game.shift(0, Direction.DOWN, player);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // --- Simulation Logic (Unchanged, just cleaner formatting) ---

    private SimulationResult findBestMove(Game game, Player realPlayer, TreasureCard targetCard) {
        return runAllSimulations(game, realPlayer, targetCard);
    }

    private SimulationResult runAllSimulations(Game game, Player realPlayer, TreasureCard targetCard) {
        Board realBoard = game.getBoard();
        SimulationResult bestResult = null;
        List<ShiftOp> ops = new ArrayList<>();

        // Generate Candidates
        for (int c = 0; c < realBoard.getWidth(); c++) {
            if (!realBoard.colContainsFixedTile(c)) {
                ops.add(new ShiftOp(ShiftType.UP, c));
                ops.add(new ShiftOp(ShiftType.DOWN, c));
            }
        }
        for (int r = 0; r < realBoard.getHeight(); r++) {
            if (!realBoard.rowContainsFixedTile(r)) {
                ops.add(new ShiftOp(ShiftType.LEFT, r));
                ops.add(new ShiftOp(ShiftType.RIGHT, r));
            }
        }

        // Evaluate Candidates
        for (ShiftOp op : ops) {
            SimulationResult res = simulate(game, realPlayer, targetCard, op);
            if (res != null && (bestResult == null || compareResults(res, bestResult) > 0)) {
                bestResult = res;
            }
        }
        return bestResult;
    }

    private int compareResults(SimulationResult a, SimulationResult b) {
        if (a.score != b.score) return Integer.compare(a.score, b.score);
        return Integer.compare(b.distanceToTarget, a.distanceToTarget); // Lower distance is better
    }

    private SimulationResult simulate(Game game, Player realPlayer, TreasureCard targetCard, ShiftOp op) {
        Board clonedBoard = game.getBoard().copy();

        // Find player on clone
        Player clonedMe = clonedBoard.getPlayers().stream()
                .filter(p -> p.getId().equals(realPlayer.getId()))
                .findFirst().orElse(null);

        if (clonedMe == null) return null;

        // Apply Shift
        boolean shifted = switch (op.type) {
            case UP    -> clonedBoard.shiftColumnUp(op.index, false);
            case DOWN  -> clonedBoard.shiftColumnDown(op.index, false);
            case LEFT  -> clonedBoard.shiftRowLeft(op.index, false);
            case RIGHT -> clonedBoard.shiftRowRight(op.index, false);
        };
        if (!shifted) return null;

        // Calculate Reachability
        Set<Tile> reachable = clonedBoard.getReachableTiles(clonedMe);

        // Find Target
        Position targetPos = null;
        if (targetCard != null) {
            targetPos = findTreasurePosition(clonedBoard, targetCard);
        }

        // Score Move
        if (targetPos != null) {
            Tile targetTile = clonedBoard.getTileAt(targetPos);
            if (reachable.contains(targetTile)) {
                return new SimulationResult(op.type, op.index, 100, 0, targetPos);
            }

            // Find closest tile
            int minDist = Integer.MAX_VALUE;
            Position bestPos = null;
            for (Tile rTile : reachable) {
                Position rPos = clonedBoard.getPositionOfTile(rTile);
                int dist = Math.abs(rPos.row() - targetPos.row()) + Math.abs(rPos.column() - targetPos.column());
                if (dist < minDist) {
                    minDist = dist;
                    bestPos = rPos;
                }
            }
            return new SimulationResult(op.type, op.index, 10, minDist, bestPos);
        }

        // No target, current position
        Position cur = clonedBoard.getPositionOfTile(clonedMe.getCurrentTile());
        return new SimulationResult(op.type, op.index, 0, 0, cur);
    }

    private Position findTreasurePosition(Board board, TreasureCard card) {
        for (int r = 0; r < board.getHeight(); r++) {
            for (int c = 0; c < board.getWidth(); c++) {
                Tile t = board.getTileAt(r, c);
                if (card.equals(t.getTreasureCard())) return new Position(r, c);
            }
        }
        return null;
    }

    private record ShiftOp(ShiftType type, int index) {}
    private enum ShiftType { UP, DOWN, LEFT, RIGHT }

    private static class SimulationResult {
        ShiftType shiftType;
        int shiftIndex;
        int score;
        int distanceToTarget;
        Position targetPosition;

        public SimulationResult(ShiftType t, int i, int s, int d, Position p) {
            this.shiftType = t;
            this.shiftIndex = i;
            this.score = s;
            this.distanceToTarget = d;
            this.targetPosition = p;
        }
    }
}