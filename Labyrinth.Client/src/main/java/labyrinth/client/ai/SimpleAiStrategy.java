package labyrinth.client.ai;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.PushActionInfo;

import java.util.*;

/**
 * Improved AI strategy with the following priority:
 * 1. If all treasures collected and can reach home -> finish game
 * 2. Try to reach treasure with any valid push + rotation
 * 3. If treasure blocked, try to reach a bonus tile
 * 4. If can't reach bonus, try using owned bonuses to reach treasure
 * 5. Move as close to treasure as possible
 *
 * Special handling:
 * - Avoids tiles occupied by other players
 * - When going home, always prefers finishing over collecting bonuses
 * - Simulates all rotations but avoids duplicate board states
 */
public class SimpleAiStrategy implements AiStrategy {

    private final Random random = new Random();
    private List<Player> allPlayers;

    @Override
    public AiDecision computeBestMove(Board board, Player player) {
        return computeBestMove(board, player, null);
    }

    /**
     * Computes the best move with knowledge of all players (for blocking detection).
     */
    public AiDecision computeBestMove(Board board, Player player, List<Player> allPlayers) {
        this.allPlayers = allPlayers;
        SimulationResult bestResult = null;

        // Generate all valid shift candidates
        List<ShiftOperation> candidates = generateShiftCandidates(board);

        System.out.println("[AI] Evaluating " + candidates.size() + " shift candidates");
        System.out.println("[AI] Going home: " + (player.getCurrentTargetTreasure() == null));
        System.out.println("[AI] Available bonuses: " + player.getAvailableBonuses());

        // Check if going home (all treasures collected)
        boolean goingHome = player.getCurrentTargetTreasure() == null;

        // Phase 1: Try to reach target (treasure or home) directly
        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                // Skip duplicate rotations (some tiles look the same after rotation)
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) {
                    continue;
                }
                evaluatedRotations.add(rotationKey);

                SimulationResult result = simulateBasicMove(board, player, op, rotation, goingHome);
                if (result != null && (bestResult == null || compareResults(result, bestResult) > 0)) {
                    bestResult = result;
                }
            }
        }

        // If we can reach the target directly, use that move
        if (bestResult != null && (bestResult.score() >= SimulationResult.SCORE_TARGET_REACHABLE)) {
            if (goingHome && bestResult.score() >= SimulationResult.SCORE_FINISH_GAME) {
                System.out.println("[AI] Can FINISH THE GAME!");
            } else {
                System.out.println("[AI] Can reach target directly!");
            }
            return AiDecision.from(bestResult);
        }

        // Phase 2: If going home, skip bonus hunting - just get closer
        if (goingHome) {
            System.out.println("[AI] Going home - not hunting for bonuses");
            // Best result already has the closest move, use it
            if (bestResult != null) {
                System.out.println("[AI] Moving closer to home");
                return AiDecision.from(bestResult);
            }
        }

        // Phase 3: Try to reach a bonus tile (when not going home)
        if (!goingHome) {
            SimulationResult bonusResult = findBestBonusMove(board, player, candidates);
            if (bonusResult != null && bonusResult.score() == SimulationResult.SCORE_BONUS_REACHABLE) {
                System.out.println("[AI] Moving to collect a bonus");
                // Only prefer bonus if we can't get closer to target
                if (bestResult == null || bestResult.score() < SimulationResult.SCORE_MOVING_CLOSER) {
                    return AiDecision.from(bonusResult);
                }
                // If we can get closer to target, compare distances
                if (bonusResult.distanceToTarget() < bestResult.distanceToTarget()) {
                    return AiDecision.from(bonusResult);
                }
            }
        }

        // Phase 4: Try using owned bonuses to reach treasure
        if (!goingHome && !player.getAvailableBonuses().isEmpty()) {
            SimulationResult bonusUseResult = tryUsingBonusesToReachTarget(board, player, candidates);
            if (bonusUseResult != null && bonusUseResult.score() >= SimulationResult.SCORE_TARGET_WITH_BONUS) {
                System.out.println("[AI] Using bonus to reach target!");
                return AiDecision.from(bonusUseResult);
            }
        }

        // Phase 5: Fallback - use best result from Phase 1 (moving closer or any move)
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
     * Generates a key representing the effective rotation state of a tile.
     * Tiles with rotational symmetry will have the same key for equivalent rotations.
     */
    private int getRotationKey(labyrinth.contracts.models.Tile tile, int rotation) {
        Direction[] entrances = tile.getEntrances();
        if (entrances == null || entrances.length == 0) {
            return 0;
        }

        // Create a bitmask of entrances after rotation
        int mask = 0;
        for (Direction d : entrances) {
            Direction rotated = rotateDirection(d, rotation);
            mask |= (1 << rotated.ordinal());
        }
        return mask;
    }

    private Direction rotateDirection(Direction d, int rotations) {
        Direction result = d;
        for (int i = 0; i < rotations; i++) {
            result = switch (result) {
                case UP -> Direction.RIGHT;
                case RIGHT -> Direction.DOWN;
                case DOWN -> Direction.LEFT;
                case LEFT -> Direction.UP;
            };
        }
        return result;
    }

    /**
     * Generates all valid shift operations for the board.
     */
    private List<ShiftOperation> generateShiftCandidates(Board board) {
        List<ShiftOperation> ops = new ArrayList<>();
        int width = board.getWidth();
        int height = board.getHeight();

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

    private ShiftOperation getForbiddenReversePush(PushActionInfo lastPush) {
        if (lastPush == null || lastPush.getRowOrColIndex() == null || lastPush.getDirection() == null) {
            return null;
        }

        int index = lastPush.getRowOrColIndex();
        Direction lastDir = lastPush.getDirection();

        Direction reverseDir = switch (lastDir) {
            case LEFT -> Direction.RIGHT;
            case RIGHT -> Direction.LEFT;
            case UP -> Direction.DOWN;
            case DOWN -> Direction.UP;
        };

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
     * Simulates a basic move (push + walk) and evaluates the result.
     */
    private SimulationResult simulateBasicMove(Board board, Player player, ShiftOperation op, int rotations, boolean goingHome) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        // Apply rotations to the extra tile
        for (int i = 0; i < rotations; i++) {
            sim.rotateExtraTile();
        }

        // Apply the shift
        if (!sim.applyShift(op)) {
            return null;
        }

        // Get reachable positions (excluding blocked positions)
        Set<Position> reachable = sim.getReachableUnblockedPositions();
        Position targetPos = sim.getTargetPosition();

        if (targetPos != null) {
            // Check if target is directly reachable and not blocked
            if (reachable.contains(targetPos)) {
                int score = goingHome ? SimulationResult.SCORE_FINISH_GAME : SimulationResult.SCORE_TARGET_REACHABLE;
                if (goingHome) {
                    System.out.println("[AI] HOME TILE IS REACHABLE! Score=" + score);
                }
                return new SimulationResult(op, rotations, score, 0, targetPos);
            }

            // Check if target is blocked by another player
            if (sim.isPositionBlocked(targetPos)) {
                // Target is blocked - find alternative
                Set<Position> allReachable = sim.getReachablePositions();
                if (allReachable.contains(targetPos)) {
                    // We could reach target but it's blocked - get as close as possible
                    Position bestAlt = findClosestUnblockedPosition(reachable, targetPos, sim.getOtherPlayerPositions());
                    if (bestAlt != null) {
                        int dist = manhattanDistance(bestAlt, targetPos);
                        return new SimulationResult(op, rotations, SimulationResult.SCORE_MOVING_CLOSER, dist, bestAlt);
                    }
                }
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
     * Finds the best move that reaches a bonus tile.
     */
    private SimulationResult findBestBonusMove(Board board, Player player, List<ShiftOperation> candidates) {
        SimulationResult bestBonusResult = null;
        Position targetPos = null;

        // First, find where the treasure is to calculate distance
        BoardSimulator tempSim = new BoardSimulator(board, player, allPlayers);
        targetPos = tempSim.getTargetPosition();

        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) {
                    continue;
                }
                evaluatedRotations.add(rotationKey);

                BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

                for (int i = 0; i < rotation; i++) {
                    sim.rotateExtraTile();
                }

                if (!sim.applyShift(op)) {
                    continue;
                }

                // Find reachable bonus positions
                Map<Position, BonusType> reachableBonuses = sim.findReachableBonuses();
                if (!reachableBonuses.isEmpty()) {
                    // Find the bonus closest to the target
                    Position bestBonusPos = null;
                    int bestDistance = Integer.MAX_VALUE;

                    for (Position bonusPos : reachableBonuses.keySet()) {
                        int dist = targetPos != null ? manhattanDistance(bonusPos, targetPos) : 0;
                        if (dist < bestDistance) {
                            bestDistance = dist;
                            bestBonusPos = bonusPos;
                        }
                    }

                    if (bestBonusPos != null) {
                        SimulationResult result = new SimulationResult(
                            op, rotation, SimulationResult.SCORE_BONUS_REACHABLE, bestDistance, bestBonusPos
                        );

                        if (bestBonusResult == null || result.distanceToTarget() < bestBonusResult.distanceToTarget()) {
                            bestBonusResult = result;
                        }
                    }
                }
            }
        }

        return bestBonusResult;
    }

    /**
     * Tries using available bonuses to reach the target.
     */
    private SimulationResult tryUsingBonusesToReachTarget(Board board, Player player, List<ShiftOperation> candidates) {
        List<BonusType> bonuses = player.getAvailableBonuses();
        SimulationResult bestResult = null;

        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) {
                    continue;
                }
                evaluatedRotations.add(rotationKey);

                // Try BEAM bonus
                if (bonuses.contains(BonusType.BEAM)) {
                    SimulationResult beamResult = tryBeamBonus(board, player, op, rotation);
                    if (beamResult != null && (bestResult == null || compareResults(beamResult, bestResult) > 0)) {
                        bestResult = beamResult;
                    }
                }

                // Try SWAP bonus
                if (bonuses.contains(BonusType.SWAP)) {
                    SimulationResult swapResult = trySwapBonus(board, player, op, rotation);
                    if (swapResult != null && (bestResult == null || compareResults(swapResult, bestResult) > 0)) {
                        bestResult = swapResult;
                    }
                }

                // Try PUSH_FIXED bonus
                if (bonuses.contains(BonusType.PUSH_FIXED)) {
                    SimulationResult pushFixedResult = tryPushFixedBonus(board, player, op, rotation);
                    if (pushFixedResult != null && (bestResult == null || compareResults(pushFixedResult, bestResult) > 0)) {
                        bestResult = pushFixedResult;
                    }
                }

                // Try PUSH_TWICE bonus
                if (bonuses.contains(BonusType.PUSH_TWICE)) {
                    SimulationResult pushTwiceResult = tryPushTwiceBonus(board, player, op, rotation, candidates);
                    if (pushTwiceResult != null && (bestResult == null || compareResults(pushTwiceResult, bestResult) > 0)) {
                        bestResult = pushTwiceResult;
                    }
                }
            }
        }

        return bestResult;
    }

    /**
     * Tries using BEAM bonus to reach target after a shift.
     * BEAM allows teleporting to any reachable position, then walking further.
     */
    private SimulationResult tryBeamBonus(Board board, Player player, ShiftOperation op, int rotation) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        for (int i = 0; i < rotation; i++) {
            sim.rotateExtraTile();
        }

        if (!sim.applyShift(op)) {
            return null;
        }

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        // Get all positions reachable from current position
        Set<Position> directlyReachable = sim.getReachablePositions();

        // For each reachable position, see if we can reach target from there
        for (Position beamDest : directlyReachable) {
            if (sim.isPositionBlocked(beamDest)) continue;

            // Simulate being at beamDest
            BoardSimulator simFromBeam = sim.copy();
            simFromBeam.simulateBeam(beamDest);

            // Check reachability from beam destination
            Set<Position> reachableFromBeam = simFromBeam.getReachableUnblockedPositions();
            if (reachableFromBeam.contains(targetPos)) {
                return new SimulationResult(
                    op, rotation, SimulationResult.SCORE_TARGET_WITH_BONUS, 0, targetPos,
                    SimulationResult.BonusAction.beam(beamDest)
                );
            }
        }

        return null;
    }

    /**
     * Tries using SWAP bonus to reach target.
     * SWAP lets us swap positions with another player.
     */
    private SimulationResult trySwapBonus(Board board, Player player, ShiftOperation op, int rotation) {
        if (allPlayers == null || allPlayers.size() <= 1) return null;

        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        for (int i = 0; i < rotation; i++) {
            sim.rotateExtraTile();
        }

        if (!sim.applyShift(op)) {
            return null;
        }

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        // Try swapping with each other player
        for (Player other : allPlayers) {
            if (other.getId().equals(player.getId())) continue;
            if (other.getCurrentPosition() == null) continue;

            // Simulate swap
            BoardSimulator simAfterSwap = sim.copy();
            Position otherPos = other.getCurrentPosition();

            // After shift, we need to find other player's new position
            // For simplicity, check if swapping would put us closer or on target
            simAfterSwap.simulateSwap(otherPos);

            Set<Position> reachableAfterSwap = simAfterSwap.getReachableUnblockedPositions();
            if (reachableAfterSwap.contains(targetPos)) {
                return new SimulationResult(
                    op, rotation, SimulationResult.SCORE_TARGET_WITH_BONUS, 0, targetPos,
                    SimulationResult.BonusAction.swap(other.getId(), otherPos)
                );
            }
        }

        return null;
    }

    /**
     * Tries using PUSH_FIXED bonus to reach target.
     * PUSH_FIXED allows pushing a row/column that has fixed tiles.
     */
    private SimulationResult tryPushFixedBonus(Board board, Player player, ShiftOperation op, int rotation) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        for (int i = 0; i < rotation; i++) {
            sim.rotateExtraTile();
        }

        if (!sim.applyShift(op)) {
            return null;
        }

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        // Get possible PUSH_FIXED operations
        List<ShiftOperation> pushFixedCandidates = sim.getPushFixedCandidates();

        for (ShiftOperation fixedOp : pushFixedCandidates) {
            for (int fixedRotation = 0; fixedRotation < 4; fixedRotation++) {
                BoardSimulator simAfterFixed = sim.copy();

                for (int i = 0; i < fixedRotation; i++) {
                    simAfterFixed.rotateExtraTile();
                }

                simAfterFixed.applyShiftIgnoringFixed(fixedOp);

                Set<Position> reachable = simAfterFixed.getReachableUnblockedPositions();
                // Recalculate target position as it may have moved
                Position newTargetPos = simAfterFixed.getTargetPosition();

                if (newTargetPos != null && reachable.contains(newTargetPos)) {
                    return new SimulationResult(
                        op, rotation, SimulationResult.SCORE_TARGET_WITH_BONUS, 0, newTargetPos,
                        SimulationResult.BonusAction.pushFixed(fixedOp, fixedRotation)
                    );
                }
            }
        }

        return null;
    }

    /**
     * Tries using PUSH_TWICE bonus to reach target.
     * PUSH_TWICE allows doing two push operations.
     */
    private SimulationResult tryPushTwiceBonus(Board board, Player player, ShiftOperation firstOp, int firstRotation, List<ShiftOperation> allCandidates) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        for (int i = 0; i < firstRotation; i++) {
            sim.rotateExtraTile();
        }

        if (!sim.applyShift(firstOp)) {
            return null;
        }

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        // Generate second push candidates (excluding reverse of first push)
        ShiftOperation forbiddenSecond = getReversePush(firstOp);

        for (ShiftOperation secondOp : allCandidates) {
            if (isEqualShiftOp(secondOp, forbiddenSecond)) continue;

            for (int secondRotation = 0; secondRotation < 4; secondRotation++) {
                BoardSimulator simAfterSecond = sim.copy();

                for (int i = 0; i < secondRotation; i++) {
                    simAfterSecond.rotateExtraTile();
                }

                if (!simAfterSecond.applyShift(secondOp)) {
                    continue;
                }

                Set<Position> reachable = simAfterSecond.getReachableUnblockedPositions();
                Position newTargetPos = simAfterSecond.getTargetPosition();

                if (newTargetPos != null && reachable.contains(newTargetPos)) {
                    return new SimulationResult(
                        firstOp, firstRotation, SimulationResult.SCORE_TARGET_WITH_BONUS, 0, newTargetPos,
                        SimulationResult.BonusAction.pushTwice(secondOp, secondRotation)
                    );
                }
            }
        }

        return null;
    }

    private ShiftOperation getReversePush(ShiftOperation op) {
        Direction reverseDir = switch (op.direction()) {
            case LEFT -> Direction.RIGHT;
            case RIGHT -> Direction.LEFT;
            case UP -> Direction.DOWN;
            case DOWN -> Direction.UP;
        };

        if (op.isRow()) {
            return ShiftOperation.row(op.index(), reverseDir);
        } else {
            return ShiftOperation.column(op.index(), reverseDir);
        }
    }

    /**
     * Finds the closest unblocked position to the target.
     */
    private Position findClosestUnblockedPosition(Set<Position> reachable, Position target, Set<Position> blocked) {
        Position best = null;
        int minDist = Integer.MAX_VALUE;

        for (Position pos : reachable) {
            if (blocked.contains(pos)) continue;
            int dist = manhattanDistance(pos, target);
            if (dist < minDist) {
                minDist = dist;
                best = pos;
            }
        }

        return best;
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
