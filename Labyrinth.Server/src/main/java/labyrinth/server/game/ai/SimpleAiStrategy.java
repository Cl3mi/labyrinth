package labyrinth.server.game.ai;

import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.Position;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class SimpleAiStrategy implements AiStrategy {

    private final java.util.Random random = new java.util.Random();

    @Override
    public void performTurn(Game game, Player realPlayer) {
        System.out.println("AI performing turn for " + realPlayer.getUsername());

        java.util.concurrent.CompletableFuture.supplyAsync(() -> {
                    TreasureCard targetCard = realPlayer.getAssignedTreasureCards().isEmpty() ? null
                            : realPlayer.getCurrentTreasureCard();
                    return findBestMove(game, realPlayer, targetCard);
                }).thenCompose(result -> delay(randomDelay()).thenApply(v -> result))
                .thenCompose(bestResult -> {
                    // Execute Shift
                    if (bestResult != null) {
                        System.out.println(
                                "AI executing shift: " + bestResult.shiftType + " index " + bestResult.shiftIndex);
                        var shiftResult = switch (bestResult.shiftType) {
                            case UP -> game.shift(bestResult.shiftIndex, Direction.UP, realPlayer);
                            case DOWN -> game.shift(bestResult.shiftIndex, Direction.DOWN, realPlayer);
                            case LEFT -> game.shift(bestResult.shiftIndex, Direction.LEFT, realPlayer);
                            case RIGHT -> game.shift(bestResult.shiftIndex, Direction.RIGHT, realPlayer);
                        };
                        if (!shiftResult.shiftSuccess())
                            forceRandomShift(game, realPlayer);
                    } else {
                        forceRandomShift(game, realPlayer);
                    }

                    // Pass result to next stage
                    return delay(randomDelay()).thenApply(v -> bestResult);
                })
                .thenAccept(bestResult -> {
                    // Execute Move
                    if (bestResult != null && bestResult.targetPosition != null) {
                        System.out.println("AI moving to: " + bestResult.targetPosition.row() + "/"
                                + bestResult.targetPosition.column());
                        var moveSuccess = game.movePlayerToTile(bestResult.targetPosition.row(), bestResult.targetPosition.column(),
                                realPlayer).moveSuccess();

                        if (!moveSuccess) {
                            Position current = game.getCurrentPositionOfPlayer(realPlayer);
                            game.movePlayerToTile(current.row(), current.column(), realPlayer);
                        }
                    } else {
                        // Stay put / fallback
                        Position current = game.getCurrentPositionOfPlayer(realPlayer);
                        game.movePlayerToTile(current.row(), current.column(), realPlayer);
                    }

                })
                .exceptionally(ex -> {
                    ex.printStackTrace();
                    return null;
                });
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

    private java.util.concurrent.CompletableFuture<Void> delay(int millis) {
        return java.util.concurrent.CompletableFuture.runAsync(() -> {
                },
                java.util.concurrent.CompletableFuture.delayedExecutor(millis,
                        java.util.concurrent.TimeUnit.MILLISECONDS));
    }

    private int randomDelay() {
        return 100 + random.nextInt(100);
    }

    private SimulationResult findBestMove(Game game, Player realPlayer, TreasureCard targetCard) {
        return runAllSimulations(game, realPlayer, targetCard);
    }

    private SimulationResult runAllSimulations(Game game, Player realPlayer, TreasureCard targetCard) {
        Board realBoard = game.getBoard();
        SimulationResult bestResult = null;

        // We define a list of candidate operations
        List<ShiftOp> ops = new ArrayList<>();
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

        for (ShiftOp op : ops) {
            SimulationResult res = simulate(game, realPlayer, targetCard, op);
            if (res == null)
                continue;

            if (bestResult == null || compareResults(res, bestResult) > 0) {
                bestResult = res;
            }
        }

        return bestResult;
    }

    private int compareResults(SimulationResult a, SimulationResult b) {
        if (a.score != b.score) {
            return Integer.compare(a.score, b.score);
        }
        // Lower distance is better
        return Integer.compare(b.distanceToTarget, a.distanceToTarget);
    }

    private SimulationResult simulate(Game game, Player realPlayer, TreasureCard targetCard, ShiftOp op) {
        // 1. Clone Game State (Board + Players internal copy)
        Board clonedBoard = game.getBoard().copy();

        // Find "me" in the cloned board
        Player clonedMe = clonedBoard.getPlayers().stream()
                .filter(p -> p.getId().equals(realPlayer.getId()))
                .findFirst()
                .orElse(null);

        if (clonedMe == null) {
            return null;
        }

        // 2. Perform Shift on Clone
        boolean shifted = switch (op.type) {
            case UP -> clonedBoard.shiftColumnUp(op.index, false);
            case DOWN -> clonedBoard.shiftColumnDown(op.index, false);
            case LEFT -> clonedBoard.shiftRowLeft(op.index, false);
            case RIGHT -> clonedBoard.shiftRowRight(op.index, false);
        };

        if (!shifted)
            return null; // Should have been filtered by containsFixedTile check but safe to double check

        // 3. Calculate Reachability
        Set<Tile> reachable = clonedBoard.getReachableTiles(clonedMe);

        // 4. Find best tile reachable
        // If targetCard is null, just pick anything (maybe center?)
        // If targetCard is set, find its position

        Position targetPosInSim = null;
        if (targetCard != null) {
            // Find where the treasure is on the CLONED board
            for (int r = 0; r < clonedBoard.getHeight(); r++) {
                for (int c = 0; c < clonedBoard.getWidth(); c++) {
                    Tile t = clonedBoard.getTileAt(r, c);
                    if (t.getTreasureCard() != null && t.getTreasureCard().equals(targetCard)) {
                        targetPosInSim = new Position(r, c);
                        break;
                    }
                }
                if (targetPosInSim != null)
                    break;
            }
        }

        int score = 0;
        int minDist = Integer.MAX_VALUE;
        Position bestPosForOp = null;

        if (targetPosInSim != null) {
            // Check if we can reach it
            Tile targetTile = clonedBoard.getTileAt(targetPosInSim);
            if (reachable.contains(targetTile)) {
                score = 100; // Can reach treasure!
                minDist = 0;
                bestPosForOp = targetPosInSim;
            } else {
                // Measure distance from all reachable tiles to target
                score = 10;
                for (Tile rTile : reachable) {
                    Position rPos = clonedBoard.getPositionOfTile(rTile);
                    int dist = Math.abs(rPos.row() - targetPosInSim.row())
                            + Math.abs(rPos.column() - targetPosInSim.column());
                    if (dist < minDist) {
                        minDist = dist;
                        bestPosForOp = rPos;
                    }
                }
            }
        } else {
            score = 0;
            minDist = 0;
            // No target, just move to current
            Position cur = clonedBoard.getPositionOfTile(clonedMe.getCurrentTile());
            bestPosForOp = cur;
        }

        return new SimulationResult(op.type, op.index, score, minDist, bestPosForOp);
    }

    private record ShiftOp(ShiftType type, int index) {
    }

    private enum ShiftType {
        UP, DOWN, LEFT, RIGHT
    }

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
