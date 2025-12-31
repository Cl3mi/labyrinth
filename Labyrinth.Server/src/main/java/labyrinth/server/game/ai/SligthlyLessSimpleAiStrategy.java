package labyrinth.server.game.ai;

import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.Position;
import labyrinth.server.game.results.MovePlayerToTileResult;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

public class SligthlyLessSimpleAiStrategy implements AiStrategy {

    private final java.util.Random random = new java.util.Random();

    // Callback to broadcast game state after AI moves
    private Runnable broadcastCallback = null;

    // Callback to handle move results (game over, treasure collected, etc.)
    private Consumer<MovePlayerToTileResult> moveResultCallback = null;

    @Override
    public void setBroadcastCallback(Runnable callback) {
        this.broadcastCallback = callback;
    }

    @Override
    public void setMoveResultCallback(Consumer<MovePlayerToTileResult> callback) {
        this.moveResultCallback = callback;
    }

    @Override
    public void performTurn(Game game, Player realPlayer) {
        System.out.println("=== AI START: " + realPlayer.getUsername() + " ===");

        // Führe den gesamten Zug in einem einzigen async Thread aus
        CompletableFuture.runAsync(() -> {
            try {
                executeTurnSynchronously(game, realPlayer);
            } catch (Exception e) {
                System.err.println("AI ERROR for " + realPlayer.getUsername() + ": " + e.getMessage());
                e.printStackTrace();
            }
        });
    }

    /**
     * Führt den gesamten Zug synchron aus (in einem separaten Thread)
     */
    private void executeTurnSynchronously(Game game, Player player) {
        System.out.println("AI " + player.getUsername() + " - Starting turn execution");

        // 1. Berechne den besten Zug
        TreasureCard targetCard = player.getCurrentTreasureCard();
        System.out.println("AI " + player.getUsername() + " - Target treasure: " + (targetCard != null ? targetCard.getTreasureName() : "NONE"));

        SimulationResult result = findBestMove(game, player, targetCard);
        System.out.println("AI " + player.getUsername() + " - Simulation result: " + (result != null ? "FOUND" : "NULL"));

        // 2. Kurze Pause (damit es natürlicher aussieht)
        sleep(150);

        // 3. Führe Shift aus
        boolean shiftSuccess = executeShift(game, player, result);
        System.out.println("AI " + player.getUsername() + " - Shift success: " + shiftSuccess);

        // 4. Broadcast nach Shift
        broadcast();

        // 5. Kurze Pause
        sleep(150);

        // 6. Führe Move aus
        executeMove(game, player, result);
        System.out.println("AI " + player.getUsername() + " - Move completed");

        // 7. Broadcast nach Move
        broadcast();

        System.out.println("=== AI END: " + player.getUsername() + " ===");
    }

    private boolean executeShift(Game game, Player player, SimulationResult result) {
        if (result == null) {
            System.out.println("AI " + player.getUsername() + " - No result, forcing random shift");
            return forceRandomShift(game, player);
        }

        System.out.println("AI " + player.getUsername() + " - Executing shift: " + result.shiftType + " at index " + result.shiftIndex);

        try {
            var shiftResult = switch (result.shiftType) {
                case UP    -> game.shift(result.shiftIndex, Direction.UP, player);
                case DOWN  -> game.shift(result.shiftIndex, Direction.DOWN, player);
                case LEFT  -> game.shift(result.shiftIndex, Direction.LEFT, player);
                case RIGHT -> game.shift(result.shiftIndex, Direction.RIGHT, player);
            };

            if (!shiftResult.shiftSuccess()) {
                System.out.println("AI " + player.getUsername() + " - Planned shift failed, forcing random");
                return forceRandomShift(game, player);
            }

            return true;
        } catch (Exception e) {
            System.err.println("AI " + player.getUsername() + " - Shift exception: " + e.getMessage());
            return forceRandomShift(game, player);
        }
    }

    private void executeMove(Game game, Player player, SimulationResult result) {
        Position current = game.getCurrentPositionOfPlayer(player);
        System.out.println("AI " + player.getUsername() + " - Current position: " + current.row() + "/" + current.column());

        Position targetPosition = (result != null && result.targetPosition != null)
                ? result.targetPosition
                : current;

        System.out.println("AI " + player.getUsername() + " - Target position: " + targetPosition.row() + "/" + targetPosition.column());

        try {
            MovePlayerToTileResult moveResult = game.movePlayerToTile(
                    targetPosition.row(),
                    targetPosition.column(),
                    player
            );

            if (!moveResult.moveSuccess()) {
                System.out.println("AI " + player.getUsername() + " - Move failed, staying at current");
                game.movePlayerToTile(current.row(), current.column(), player);
            } else {
                // Notify about the move result (treasure collected, game over, etc.)
                if (moveResultCallback != null) {
                    System.out.println("AI " + player.getUsername() + " - Notifying move result: gameOver=" + moveResult.gameOver() + ", treasureCollected=" + moveResult.treasureCollected());
                    moveResultCallback.accept(moveResult);
                }
            }
        } catch (Exception e) {
            // Wenn das Spiel beendet ist (z.B. weil wir gerade gewonnen haben), ist das OK
            if (e.getMessage() != null && e.getMessage().contains("FINISHED")) {
                System.out.println("AI " + player.getUsername() + " - Game has ended (player may have won!)");
                // Wichtig: Trotzdem das GameOver-Event triggern!
                if (moveResultCallback != null) {
                    System.out.println("AI " + player.getUsername() + " - Triggering GameOver event manually");
                    // Erstelle ein "Dummy" MoveResult das gameOver=true signalisiert
                    moveResultCallback.accept(new MovePlayerToTileResult(true, 0, true, true, false));
                }
            } else {
                System.err.println("AI " + player.getUsername() + " - Move exception: " + e.getMessage());
                try {
                    game.movePlayerToTile(current.row(), current.column(), player);
                } catch (Exception e2) {
                    // Ignorieren - Spiel ist wahrscheinlich beendet
                    System.out.println("AI " + player.getUsername() + " - Cannot make fallback move (game likely ended)");
                }
            }
        }
    }

    private void broadcast() {
        if (broadcastCallback != null) {
            try {
                System.out.println("AI - Broadcasting game state...");
                broadcastCallback.run();
                System.out.println("AI - Broadcast completed");
            } catch (Exception e) {
                System.err.println("AI - Broadcast failed: " + e.getMessage());
                e.printStackTrace();
            }
        } else {
            System.out.println("AI - No broadcast callback set!");
        }
    }

    private void sleep(int millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private boolean forceRandomShift(Game game, Player player) {
        int[] indices = {1, 3, 5};
        Direction[] directions = {Direction.RIGHT, Direction.DOWN, Direction.LEFT, Direction.UP};

        for (int index : indices) {
            for (Direction dir : directions) {
                try {
                    var result = game.shift(index, dir, player);
                    if (result.shiftSuccess()) {
                        System.out.println("AI " + player.getUsername() + " - Random shift succeeded: " + dir + " at " + index);
                        return true;
                    }
                } catch (Exception ignored) {}
            }
        }
        System.err.println("AI " + player.getUsername() + " - Could not find any valid shift!");
        return false;
    }

    // === SIMULATION LOGIC ===

    private SimulationResult findBestMove(Game game, Player realPlayer, TreasureCard targetCard) {
        Board realBoard = game.getBoard();
        SimulationResult bestResult = null;
        List<ShiftOp> ops = new ArrayList<>();

        // Generate shift candidates
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

        System.out.println("AI - Evaluating " + ops.size() + " shift candidates");

        // Evaluate each candidate
        for (ShiftOp op : ops) {
            SimulationResult res = simulate(game, realPlayer, targetCard, op);
            if (res != null && (bestResult == null || compareResults(res, bestResult) > 0)) {
                bestResult = res;
            }
        }

        // Fallback if no result found
        if (bestResult == null && !ops.isEmpty()) {
            ShiftOp fallbackOp = ops.get(random.nextInt(ops.size()));
            Position currentPos = game.getCurrentPositionOfPlayer(realPlayer);
            bestResult = new SimulationResult(fallbackOp.type, fallbackOp.index, 0, 0, currentPos);
            System.out.println("AI - Using fallback shift");
        }

        return bestResult;
    }

    private int compareResults(SimulationResult a, SimulationResult b) {
        if (a.score != b.score) return Integer.compare(a.score, b.score);
        return Integer.compare(b.distanceToTarget, a.distanceToTarget);
    }

    private SimulationResult simulate(Game game, Player realPlayer, TreasureCard targetCard, ShiftOp op) {
        Board clonedBoard = game.getBoard().copy();

        Player clonedMe = clonedBoard.getPlayers().stream()
                .filter(p -> p.getId().equals(realPlayer.getId()))
                .findFirst().orElse(null);

        if (clonedMe == null) return null;

        boolean shifted = switch (op.type) {
            case UP    -> clonedBoard.shiftColumnUp(op.index, false);
            case DOWN  -> clonedBoard.shiftColumnDown(op.index, false);
            case LEFT  -> clonedBoard.shiftRowLeft(op.index, false);
            case RIGHT -> clonedBoard.shiftRowRight(op.index, false);
        };
        if (!shifted) return null;

        Set<Tile> reachable = clonedBoard.getReachableTiles(clonedMe);

        Position targetPos = null;
        if (targetCard != null) {
            targetPos = findTreasurePosition(clonedBoard, targetCard);
        }

        if (targetPos != null) {
            Tile targetTile = clonedBoard.getTileAt(targetPos);
            if (reachable.contains(targetTile)) {
                return new SimulationResult(op.type, op.index, 100, 0, targetPos);
            }

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

        // No target - move to random reachable position
        Position cur = clonedBoard.getPositionOfTile(clonedMe.getCurrentTile());
        if (!reachable.isEmpty()) {
            List<Tile> reachableList = new ArrayList<>(reachable);
            Tile randomTile = reachableList.get(random.nextInt(reachableList.size()));
            Position randomPos = clonedBoard.getPositionOfTile(randomTile);
            return new SimulationResult(op.type, op.index, 1, 0, randomPos);
        }

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

        SimulationResult(ShiftType t, int i, int s, int d, Position p) {
            this.shiftType = t;
            this.shiftIndex = i;
            this.score = s;
            this.distanceToTarget = d;
            this.targetPosition = p;
        }
    }
}