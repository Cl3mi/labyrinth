package labyrinth.server.game.ai;

import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.Position;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class AdvancedAiStrategy implements AiStrategy {

    private final Random random = new Random();

    @Override
    public void performTurn(Game game, Player realPlayer) {
        System.out.println("Advanced AI performing turn for " + realPlayer.getUsername());

        CompletableFuture.supplyAsync(() -> calculateBestTurn(game, realPlayer))
                .thenCompose(result -> delay(randomDelay()).thenApply(v -> result))
                .thenCompose(bestTurn -> {
                    // Execute Shift
                    if (bestTurn != null) {
                        System.out.println("Advanced AI shift: " + bestTurn.shiftType + " " + bestTurn.shiftIndex);
                        applyShift(game, realPlayer, bestTurn.shiftType, bestTurn.shiftIndex);
                    } else {
                        forceRandomShift(game, realPlayer);
                    }
                    return delay(randomDelay()).thenApply(v -> bestTurn);
                })
                .thenAccept(bestTurn -> {
                    // Update AI Memory
                    if (bestTurn != null) {
                        realPlayer.setLastShiftDescription(bestTurn.shiftType + ":" + bestTurn.shiftIndex);
                    }

                    Position beforePos = game.getCurrentPositionOfPlayer(realPlayer); // Unused but kept for reference
                                                                                      // logic if needed

                    // Execute Move
                    if (bestTurn != null && bestTurn.moveTarget != null) {
                        System.out.println("Advanced AI move: " + bestTurn.moveTarget);
                        game.movePlayerToTile(bestTurn.moveTarget.row(), bestTurn.moveTarget.column(), realPlayer);
                    } else {
                        // Stay
                        Position curr = game.getCurrentPositionOfPlayer(realPlayer);
                        game.movePlayerToTile(curr.row(), curr.column(), realPlayer);
                    }

                    Position afterPos = game.getCurrentPositionOfPlayer(realPlayer);

                    // Check Stuck
                    if (afterPos.equals(realPlayer.getLastTurnPosition())) {
                        realPlayer.setTurnsStuck(realPlayer.getTurnsStuck() + 1);
                    } else {
                        realPlayer.setTurnsStuck(0);
                    }
                    realPlayer.setLastTurnPosition(afterPos);
                })
                .exceptionally(ex -> {
                    ex.printStackTrace();
                    return null;
                });
    }

    private TurnResult calculateBestTurn(Game game, Player me) {
        Board board = game.getBoard();
        List<ShiftOp> possibleShifts = getPossibleShifts(board);

        TurnResult bestTurn = null;
        double maxScore = Double.NEGATIVE_INFINITY;

        // Player next opponent
        Player opponent = getNextOpponent(game, me);
        TreasureCard myTarget = getTargetCard(me);
        TreasureCard opTarget = (opponent != null) ? getTargetCard(opponent) : null;

        Position currentPos = game.getCurrentPositionOfPlayer(me);
        boolean isStuck = me.getTurnsStuck() > 2;

        for (ShiftOp shift : possibleShifts) {
            // Prune: Don't undo immediate last move if stuck score is low
            if (!isStuck && isUndoMove(shift, me.getLastShiftDescription())) {
                continue;
            }

            // 1. Simulate My Shift
            Board simBoard = board.copy();
            Player simMe = getPlayerOnBoard(simBoard, me);
            if (simMe == null)
                continue;

            if (!executeSimulatedShift(simBoard, shift))
                continue;

            // 2. Find My Best Move on this board
            MoveResult myBestMove = findBestMoveForPlayer(simBoard, simMe, myTarget);

            // 3. Score My State
            double myScore = myBestMove.score;
            if (myBestMove.collectedTreasure) {
                myScore += 1000;
            }

            // Stuck / Boredom Logic
            if (isStuck) {
                // If stuck, we prioritize changing effective position
                int movedDist = distance(currentPos, myBestMove.targetPos);
                if (movedDist > 0) {
                    myScore += (movedDist * 100); // Incentive to MOVE
                    myScore += 500; // Bonus for ANY change
                }
            }

            // 4. Lookahead: Simulate Opponent's Best Response from this new state
            double opponentBestScore = 0;
            if (opponent != null) {
                // Update simMe
                if (myBestMove.targetPos != null) {
                    Tile targetTile = simBoard.getTileAt(myBestMove.targetPos);
                    simMe.setCurrentTile(targetTile);
                }
                opponentBestScore = calculateOpponentMaxScore(simBoard, opponent, opTarget);
            }

            // 5. Total Utility = My Score - (Weight * Opponent Score)
            double totalScore = myScore - (0.8 * opponentBestScore);

            // Avoid repeating same shift forever if it leads nowhere
            if (isStuck && isRepeatMove(shift, me.getLastShiftDescription())) {
                totalScore -= 200;
            }

            if (totalScore > maxScore) {
                maxScore = totalScore;
                bestTurn = new TurnResult(shift.type, shift.index, myBestMove.targetPos, totalScore);
            }
        }

        return bestTurn;
    }

    private double calculateOpponentMaxScore(Board currentBoard, Player realOpponent, TreasureCard opTarget) {
        // We need to find the opponent on the CURRENT simulated board
        Player simOpponent = getPlayerOnBoard(currentBoard, realOpponent);
        if (simOpponent == null)
            return 0; // Should not happen

        List<ShiftOp> ops = getPossibleShifts(currentBoard);
        double maxOpScore = Double.NEGATIVE_INFINITY;

        // Optimization: Don't check ALL shifts if too slow?
        // 12 shifts * BFS is fine.
        for (ShiftOp op : ops) {
            Board opBoard = currentBoard.copy(); // Copy again for depth 2
            Player opSimPlayer = getPlayerOnBoard(opBoard, realOpponent);
            if (opSimPlayer == null)
                continue;

            if (!executeSimulatedShift(opBoard, op))
                continue;

            MoveResult res = findBestMoveForPlayer(opBoard, opSimPlayer, opTarget);
            double score = res.score;
            if (res.collectedTreasure)
                score += 1000;

            if (score > maxOpScore) {
                maxOpScore = score;
            }
        }
        return maxOpScore;
    }

    private MoveResult findBestMoveForPlayer(Board board, Player player, TreasureCard target) {
        Set<Tile> reachable = board.getReachableTiles(player);

        // Find target position on this board
        Position targetPos = null;
        if (target != null) {
            // Scan board for card
            // Optimization: Maybe cache treasure locations? For now scan is fast enough.
            for (int r = 0; r < board.getHeight(); r++) {
                for (int c = 0; c < board.getWidth(); c++) {
                    Tile t = board.getTileAt(r, c);
                    if (t.getTreasureCard() != null && t.getTreasureCard().equals(target)) {
                        targetPos = new Position(r, c);
                        break;
                    }
                }
                if (targetPos != null)
                    break;
            }
        }

        if (targetPos != null) {
            Tile targetTile = board.getTileAt(targetPos);
            if (reachable.contains(targetTile)) {
                return new MoveResult(targetPos, 1000, true);
            }
            // Not reachable: find closest
            int minDist = Integer.MAX_VALUE;
            Position bestPos = null;
            for (Tile t : reachable) {
                Position p = board.getPositionOfTile(t);
                int dist = distance(p, targetPos);
                if (dist < minDist) {
                    minDist = dist;
                    bestPos = p;
                }
            }
            // Score based on proximity (Max board size ~14 distance)
            // Score 0-100 based on closeness
            return new MoveResult(bestPos, 100 - (minDist * 5), false);
        } else {
            // No target (all collected? or starting). Just maximize mobility or random?
            // Score = 0
            Position current = board.getPositionOfTile(player.getCurrentTile());
            return new MoveResult(current, 0, false);
        }
    }

    // Helpers

    private int distance(Position a, Position b) {
        return Math.abs(a.row() - b.row()) + Math.abs(a.column() - b.column());
    }

    private Player getNextOpponent(Game game, Player me) {
        List<Player> players = game.getPlayers();
        int myIdx = players.indexOf(me);
        if (myIdx == -1)
            return null;
        int nextIdx = (myIdx + 1) % players.size();
        if (nextIdx == myIdx)
            return null; // Solo game?
        return players.get(nextIdx);
    }

    private TreasureCard getTargetCard(Player p) {
        if (p.getAssignedTreasureCards().isEmpty())
            return null;
        return p.getAssignedTreasureCards().getFirst();
    }

    private Player getPlayerOnBoard(Board board, Player original) {
        return board.getPlayers().stream()
                .filter(p -> p.getId().equals(original.getId()))
                .findFirst()
                .orElse(null);
    }

    private List<ShiftOp> getPossibleShifts(Board board) {
        List<ShiftOp> ops = new ArrayList<>();
        for (int c = 0; c < board.getWidth(); c++) {
            if (!board.colContainsFixedTile(c)) {
                ops.add(new ShiftOp(ShiftType.UP, c));
                ops.add(new ShiftOp(ShiftType.DOWN, c));
            }
        }
        for (int r = 0; r < board.getHeight(); r++) {
            if (!board.rowContainsFixedTile(r)) {
                ops.add(new ShiftOp(ShiftType.LEFT, r));
                ops.add(new ShiftOp(ShiftType.RIGHT, r));
            }
        }
        return ops;
    }

    private boolean executeSimulatedShift(Board board, ShiftOp op) {
        return switch (op.type) {
            case UP -> board.shiftColumnUp(op.index, false);
            case DOWN -> board.shiftColumnDown(op.index, false);
            case LEFT -> board.shiftRowLeft(op.index, false);
            case RIGHT -> board.shiftRowRight(op.index, false);
        };
    }

    private boolean applyShift(Game game, Player p, ShiftType type, int index) {
        return switch (type) {
            case UP -> game.shift(index, Direction.UP, null, p);
            case DOWN -> game.shift(index, Direction.DOWN, null, p);
            case LEFT -> game.shift(index, Direction.LEFT, null, p);
            case RIGHT -> game.shift(index, Direction.RIGHT, null, p);
        };
    }

    private boolean isUndoMove(ShiftOp op, String lastDesc) {
        if (lastDesc == null)
            return false;
        // Format: "TYPE:INDEX"
        String[] parts = lastDesc.split(":");
        if (parts.length < 2)
            return false;
        ShiftType lastType = ShiftType.valueOf(parts[0]);
        int lastIndex = Integer.parseInt(parts[1]);

        if (lastIndex != op.index)
            return false;

        return switch (lastType) {
            case UP -> op.type == ShiftType.DOWN;
            case DOWN -> op.type == ShiftType.UP;
            case LEFT -> op.type == ShiftType.RIGHT;
            case RIGHT -> op.type == ShiftType.LEFT;
        };
    }

    private boolean isRepeatMove(ShiftOp op, String lastDesc) {
        if (lastDesc == null)
            return false;
        String cur = op.type + ":" + op.index;
        return cur.equals(lastDesc);
    }

    private void forceRandomShift(Game game, Player player) {
        try {
            if (!applyShift(game, player, ShiftType.RIGHT, 1)) {
                applyShift(game, player, ShiftType.DOWN, 0);
            }
        } catch (Exception e) {
        }
    }

    private CompletableFuture<Void> delay(int millis) {
        return CompletableFuture.runAsync(() -> {
        },
                CompletableFuture.delayedExecutor(millis, TimeUnit.MILLISECONDS));
    }

    private int randomDelay() {
        return 500 + random.nextInt(500);
    }

    // Structs
    private record ShiftOp(ShiftType type, int index) {
    }

    private enum ShiftType {
        UP, DOWN, LEFT, RIGHT
    }

    private record TurnResult(ShiftType shiftType, int shiftIndex, Position moveTarget, double score) {
    }

    private record MoveResult(Position targetPos, double score, boolean collectedTreasure) {
    }
}
