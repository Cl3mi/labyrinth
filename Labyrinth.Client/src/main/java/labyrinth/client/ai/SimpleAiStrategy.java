package labyrinth.client.ai;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.PushActionInfo;

import java.util.*;

/**
 * Simple AI strategy ported from the server's SligthlyLessSimpleAiStrategy.
 * Uses simulation and scoring to find the best move.
 */
public class SimpleAiStrategy implements AiStrategy {

    private final Random random = new Random();

    @Override
    public AiDecision computeBestMove(Board board, Player player) {
        SimulationResult bestResult = null;

        // Generate all valid shift candidates
        List<ShiftOperation> candidates = generateShiftCandidates(board);

        System.out.println("[AI] Evaluating " + candidates.size() + " shift candidates");

        // Evaluate each shift with different rotations
        for (ShiftOperation op : candidates) {
            for (int rotation = 0; rotation < 4; rotation++) {
                SimulationResult result = simulate(board, player, op, rotation);
                if (result != null && (bestResult == null || compareResults(result, bestResult) > 0)) {
                    bestResult = result;
                }
            }
        }

        // Fallback if no result found
        if (bestResult == null && !candidates.isEmpty()) {
            ShiftOperation fallbackOp = candidates.get(random.nextInt(candidates.size()));
            Position currentPos = player.getCurrentPosition();
            bestResult = new SimulationResult(fallbackOp, 0, SimulationResult.SCORE_NO_MOVE, 0, currentPos);
            System.out.println("[AI] Using fallback shift");
        }

        if (bestResult != null) {
            System.out.println("[AI] Best move: " + bestResult.shift().direction() + " at " +
                    bestResult.shift().index() + ", score=" + bestResult.score() +
                    ", rotations=" + bestResult.rotations());
        }

        return AiDecision.from(bestResult);
    }

    /**
     * Generates all valid shift operations for the board.
     * Excludes outer rows/columns (0 and height-1/width-1), rows/columns with fixed tiles,
     * and the forbidden reverse push (undoing the previous player's push).
     */
    private List<ShiftOperation> generateShiftCandidates(Board board) {
        List<ShiftOperation> ops = new ArrayList<>();
        int width = board.getWidth();
        int height = board.getHeight();

        // Get the forbidden reverse push
        ShiftOperation forbiddenOp = getForbiddenReversePush(board.getLastPush());
        if (forbiddenOp != null) {
            System.out.println("[AI] Forbidden reverse push: " + forbiddenOp.direction() + " at " + forbiddenOp.index());
        }

        // Row shifts (LEFT/RIGHT) - exclude outer rows
        for (int row = 1; row < height - 1; row++) {
            if (!rowContainsFixedTile(board, row)) {
                ShiftOperation leftOp = ShiftOperation.row(row, Direction.LEFT);
                ShiftOperation rightOp = ShiftOperation.row(row, Direction.RIGHT);

                if (!isEqualShiftOp(leftOp, forbiddenOp)) {
                    ops.add(leftOp);
                }
                if (!isEqualShiftOp(rightOp, forbiddenOp)) {
                    ops.add(rightOp);
                }
            }
        }

        // Column shifts (UP/DOWN) - exclude outer columns
        for (int col = 1; col < width - 1; col++) {
            if (!colContainsFixedTile(board, col)) {
                ShiftOperation upOp = ShiftOperation.column(col, Direction.UP);
                ShiftOperation downOp = ShiftOperation.column(col, Direction.DOWN);

                if (!isEqualShiftOp(upOp, forbiddenOp)) {
                    ops.add(upOp);
                }
                if (!isEqualShiftOp(downOp, forbiddenOp)) {
                    ops.add(downOp);
                }
            }
        }

        return ops;
    }

    /**
     * Returns the forbidden reverse push operation based on the last push.
     * In Labyrinth, you cannot simply reverse the previous player's push.
     */
    private ShiftOperation getForbiddenReversePush(PushActionInfo lastPush) {
        if (lastPush == null || lastPush.getRowOrColIndex() == null || lastPush.getDirection() == null) {
            return null;
        }

        int index = lastPush.getRowOrColIndex();
        Direction lastDir = lastPush.getDirection();

        // The forbidden operation is the reverse direction on the same row/column
        Direction reverseDir = switch (lastDir) {
            case LEFT -> Direction.RIGHT;
            case RIGHT -> Direction.LEFT;
            case UP -> Direction.DOWN;
            case DOWN -> Direction.UP;
        };

        // Determine if it was a row or column shift
        boolean isRowShift = (lastDir == Direction.LEFT || lastDir == Direction.RIGHT);

        if (isRowShift) {
            return ShiftOperation.row(index, reverseDir);
        } else {
            return ShiftOperation.column(index, reverseDir);
        }
    }

    private boolean isEqualShiftOp(ShiftOperation a, ShiftOperation b) {
        if (a == null || b == null) return false;
        return a.index() == b.index() &&
               a.direction() == b.direction() &&
               a.isRow() == b.isRow();
    }

    private boolean rowContainsFixedTile(Board board, int rowIndex) {
        for (int c = 0; c < board.getWidth(); c++) {
            if (Boolean.TRUE.equals(board.getTiles()[rowIndex][c].getIsFixed())) {
                return true;
            }
        }
        return false;
    }

    private boolean colContainsFixedTile(Board board, int colIndex) {
        for (int r = 0; r < board.getHeight(); r++) {
            if (Boolean.TRUE.equals(board.getTiles()[r][colIndex].getIsFixed())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Simulates a shift operation and evaluates the result.
     */
    private SimulationResult simulate(Board board, Player player, ShiftOperation op, int rotations) {
        // Create a copy of the board for simulation
        BoardSimulator sim = new BoardSimulator(board, player);

        // Apply rotations to the extra tile
        for (int i = 0; i < rotations; i++) {
            sim.rotateExtraTile();
        }

        // Apply the shift
        if (!sim.applyShift(op)) {
            return null; // Shift failed (blocked by fixed tiles)
        }

        // Get reachable positions after shift
        Set<Position> reachable = sim.getReachablePositions();

        // Get target position (treasure or home)
        Position targetPos = sim.getTargetPosition();
        boolean goingHome = sim.isGoingHome();

        if (targetPos != null) {
            // Check if target is directly reachable
            if (reachable.contains(targetPos)) {
                if (goingHome) {
                    System.out.println("[AI] HOME TILE IS REACHABLE! Score=100");
                }
                return new SimulationResult(op, rotations, SimulationResult.SCORE_TARGET_REACHABLE, 0, targetPos);
            }

            // Find closest reachable position to target
            int minDist = Integer.MAX_VALUE;
            Position bestPos = null;
            for (Position rPos : reachable) {
                int dist = manhattanDistance(rPos, targetPos);
                if (dist < minDist) {
                    minDist = dist;
                    bestPos = rPos;
                }
            }

            if (bestPos != null) {
                return new SimulationResult(op, rotations, SimulationResult.SCORE_MOVING_CLOSER, minDist, bestPos);
            }
        }

        // Fallback: move to any reachable position
        Position currentPos = sim.getPlayerPosition();
        if (!reachable.isEmpty()) {
            List<Position> reachableList = new ArrayList<>(reachable);
            Position randomPos = reachableList.get(random.nextInt(reachableList.size()));
            return new SimulationResult(op, rotations, SimulationResult.SCORE_FALLBACK, 0, randomPos);
        }

        return new SimulationResult(op, rotations, SimulationResult.SCORE_NO_MOVE, 0, currentPos);
    }

    /**
     * Compares two simulation results.
     * Higher score wins; if tied, lower distance wins.
     */
    private int compareResults(SimulationResult a, SimulationResult b) {
        if (a.score() != b.score()) {
            return Integer.compare(a.score(), b.score());
        }
        // Lower distance is better
        return Integer.compare(b.distanceToTarget(), a.distanceToTarget());
    }

    private int manhattanDistance(Position a, Position b) {
        return Math.abs(a.getRow() - b.getRow()) + Math.abs(a.getColumn() - b.getColumn());
    }
}
