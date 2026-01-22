package labyrinth.client.ai;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.PushActionInfo;

import java.util.*;

/**
 * AI strategy with the following priority:
 * 1. If all treasures collected and can reach home -> finish game
 * 2. Try to reach treasure with any valid push + rotation
 * 3. If treasure unreachable, try to reach a bonus tile
 * 4. Try using owned bonuses to reach treasure
 * 5. Move as close to treasure as possible while maximizing steps taken
 *
 * Special handling:
 * - Avoids tiles occupied by other players
 * - When going home, always prefers finishing over collecting bonuses
 * - Simulates all rotations but avoids duplicate board states
 */
public class SimpleAiStrategy implements AiStrategy {

    private static final double DISTANCE_WEIGHT = 3.0;
    private static final double STEPS_WEIGHT = 1.0;

    private final Random random = new Random();
    private List<Player> allPlayers;

    @Override
    public AiDecision computeBestMove(Board board, Player player) {
        return computeBestMove(board, player, null);
    }

    public AiDecision computeBestMove(Board board, Player player, List<Player> allPlayers) {
        this.allPlayers = allPlayers;
        SimulationResult bestResult = null;

        List<ShiftOperation> candidates = generateShiftCandidates(board);

        System.out.println("[AI] Evaluating " + candidates.size() + " shift candidates");
        System.out.println("[AI] Going home: " + (player.getCurrentTargetTreasure() == null));
        System.out.println("[AI] Available bonuses: " + player.getAvailableBonuses());

        boolean goingHome = player.getCurrentTargetTreasure() == null;

        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
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

        if (bestResult != null && bestResult.score() >= SimulationResult.SCORE_TARGET_REACHABLE) {
            if (goingHome && bestResult.score() >= SimulationResult.SCORE_FINISH_GAME) {
                System.out.println("[AI] Can FINISH THE GAME!");
            } else {
                System.out.println("[AI] Can reach target directly!");
            }
            return AiDecision.from(bestResult);
        }

        if (goingHome) {
            System.out.println("[AI] Going home - not hunting for bonuses");
            if (bestResult != null) {
                System.out.println("[AI] Moving closer to home");
                return AiDecision.from(bestResult);
            }
        }

        if (!goingHome) {
            SimulationResult bonusResult = findBestBonusMove(board, player, candidates);
            if (bonusResult != null && bonusResult.score() == SimulationResult.SCORE_BONUS_REACHABLE) {
                System.out.println("[AI] Moving to collect a bonus");
                if (bestResult == null || bestResult.score() < SimulationResult.SCORE_MOVING_CLOSER) {
                    return AiDecision.from(bonusResult);
                }
                if (bonusResult.distanceToTarget() < bestResult.distanceToTarget()) {
                    return AiDecision.from(bonusResult);
                }
            }
        }

        if (!goingHome && !player.getAvailableBonuses().isEmpty()) {
            SimulationResult bonusUseResult = tryUsingBonusesToReachTarget(board, player, candidates);
            if (bonusUseResult != null && bonusUseResult.score() >= SimulationResult.SCORE_TARGET_WITH_BONUS) {
                System.out.println("[AI] Using bonus to reach target!");
                return AiDecision.from(bonusUseResult);
            }
        }

        if (bestResult == null && !candidates.isEmpty()) {
            ShiftOperation fallbackOp = candidates.get(random.nextInt(candidates.size()));
            Position currentPos = player.getCurrentPosition();
            bestResult = new SimulationResult(fallbackOp, 0, SimulationResult.SCORE_NO_MOVE, 0, currentPos, 0);
            System.out.println("[AI] Using fallback shift");
        }

        if (bestResult != null) {
            System.out.println("[AI] Best move: " + bestResult.shift().direction() + " at " +
                    bestResult.shift().index() + ", score=" + bestResult.score() +
                    ", rotations=" + bestResult.rotations() + ", steps=" + bestResult.stepsToMove());
        }

        return AiDecision.from(bestResult);
    }

    /**
     * Generates a rotation key based on entrance bitmask to detect equivalent rotations.
     */
    private int getRotationKey(labyrinth.contracts.models.Tile tile, int rotation) {
        Direction[] entrances = tile.getEntrances();
        if (entrances == null || entrances.length == 0) {
            return 0;
        }

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

    private List<ShiftOperation> generateShiftCandidates(Board board) {
        List<ShiftOperation> ops = new ArrayList<>();
        int width = board.getWidth();
        int height = board.getHeight();

        ShiftOperation forbiddenOp = getForbiddenReversePush(board.getLastPush());
        if (forbiddenOp != null) {
            System.out.println("[AI] Forbidden reverse push: " + forbiddenOp.direction() + " at " + forbiddenOp.index());
        }

        for (int row = 1; row < height - 1; row++) {
            if (!rowContainsFixedTile(board, row)) {
                ShiftOperation leftOp = ShiftOperation.row(row, Direction.LEFT);
                ShiftOperation rightOp = ShiftOperation.row(row, Direction.RIGHT);
                if (!isEqualShiftOp(leftOp, forbiddenOp)) ops.add(leftOp);
                if (!isEqualShiftOp(rightOp, forbiddenOp)) ops.add(rightOp);
            }
        }

        for (int col = 1; col < width - 1; col++) {
            if (!colContainsFixedTile(board, col)) {
                ShiftOperation upOp = ShiftOperation.column(col, Direction.UP);
                ShiftOperation downOp = ShiftOperation.column(col, Direction.DOWN);
                if (!isEqualShiftOp(upOp, forbiddenOp)) ops.add(upOp);
                if (!isEqualShiftOp(downOp, forbiddenOp)) ops.add(downOp);
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
        return isRowShift ? ShiftOperation.row(index, reverseDir) : ShiftOperation.column(index, reverseDir);
    }

    private boolean isEqualShiftOp(ShiftOperation a, ShiftOperation b) {
        if (a == null || b == null) return false;
        return a.index() == b.index() && a.direction() == b.direction() && a.isRow() == b.isRow();
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

    private SimulationResult simulateBasicMove(Board board, Player player, ShiftOperation op, int rotations, boolean goingHome) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        for (int i = 0; i < rotations; i++) {
            sim.rotateExtraTile();
        }

        if (!sim.applyShift(op)) {
            return null;
        }

        Set<Position> reachable = sim.getReachableUnblockedPositions();
        Position targetPos = sim.getTargetPosition();
        Position currentPos = sim.getPlayerPosition();

        if (targetPos != null) {
            if (reachable.contains(targetPos)) {
                int score = goingHome ? SimulationResult.SCORE_FINISH_GAME : SimulationResult.SCORE_TARGET_REACHABLE;
                int steps = manhattanDistance(currentPos, targetPos);
                if (goingHome) {
                    System.out.println("[AI] HOME TILE IS REACHABLE! Score=" + score);
                }
                return new SimulationResult(op, rotations, score, 0, targetPos, steps);
            }

            if (sim.isPositionBlocked(targetPos)) {
                Set<Position> allReachable = sim.getReachablePositions();
                if (allReachable.contains(targetPos)) {
                    Position bestAlt = findBestPositionWithSteps(reachable, targetPos, currentPos, sim.getOtherPlayerPositions());
                    if (bestAlt != null) {
                        int dist = manhattanDistance(bestAlt, targetPos);
                        int steps = manhattanDistance(currentPos, bestAlt);
                        return new SimulationResult(op, rotations, SimulationResult.SCORE_MOVING_CLOSER, dist, bestAlt, steps);
                    }
                }
            }

            Position bestPos = findBestPositionWithSteps(reachable, targetPos, currentPos, Collections.emptySet());
            if (bestPos != null) {
                int dist = manhattanDistance(bestPos, targetPos);
                int steps = manhattanDistance(currentPos, bestPos);
                return new SimulationResult(op, rotations, SimulationResult.SCORE_MOVING_CLOSER, dist, bestPos, steps);
            }
        }

        if (!reachable.isEmpty()) {
            Position farthestPos = findFarthestPosition(reachable, currentPos);
            int steps = manhattanDistance(currentPos, farthestPos);
            return new SimulationResult(op, rotations, SimulationResult.SCORE_FALLBACK, 0, farthestPos, steps);
        }

        return new SimulationResult(op, rotations, SimulationResult.SCORE_NO_MOVE, 0, currentPos, 0);
    }

    /**
     * Finds the best position balancing distance to target and steps taken.
     * Uses weighted scoring: lower distance to target is better, but more steps is also valuable.
     */
    private Position findBestPositionWithSteps(Set<Position> reachable, Position target, Position current, Set<Position> blocked) {
        Position best = null;
        double bestScore = Double.NEGATIVE_INFINITY;

        for (Position pos : reachable) {
            if (blocked.contains(pos)) continue;

            int distToTarget = manhattanDistance(pos, target);
            int steps = manhattanDistance(current, pos);

            // Score: prioritize getting closer to target, but reward taking more steps
            // Negative distance (closer is better) + positive steps bonus
            double score = -distToTarget * DISTANCE_WEIGHT + steps * STEPS_WEIGHT;

            if (score > bestScore) {
                bestScore = score;
                best = pos;
            }
        }

        return best;
    }

    /**
     * Finds the position farthest from current position (maximizes steps when no target).
     */
    private Position findFarthestPosition(Set<Position> reachable, Position current) {
        Position farthest = null;
        int maxDist = -1;

        for (Position pos : reachable) {
            int dist = manhattanDistance(pos, current);
            if (dist > maxDist) {
                maxDist = dist;
                farthest = pos;
            }
        }

        return farthest != null ? farthest : reachable.iterator().next();
    }

    private SimulationResult findBestBonusMove(Board board, Player player, List<ShiftOperation> candidates) {
        SimulationResult bestBonusResult = null;

        BoardSimulator tempSim = new BoardSimulator(board, player, allPlayers);
        Position targetPos = tempSim.getTargetPosition();

        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) continue;
                evaluatedRotations.add(rotationKey);

                BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
                for (int i = 0; i < rotation; i++) {
                    sim.rotateExtraTile();
                }

                if (!sim.applyShift(op)) continue;

                Map<Position, BonusType> reachableBonuses = sim.findReachableBonuses();
                if (!reachableBonuses.isEmpty()) {
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
                        int steps = manhattanDistance(sim.getPlayerPosition(), bestBonusPos);
                        SimulationResult result = new SimulationResult(
                            op, rotation, SimulationResult.SCORE_BONUS_REACHABLE, bestDistance, bestBonusPos, steps
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

    private SimulationResult tryUsingBonusesToReachTarget(Board board, Player player, List<ShiftOperation> candidates) {
        List<BonusType> bonuses = player.getAvailableBonuses();
        SimulationResult bestResult = null;

        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) continue;
                evaluatedRotations.add(rotationKey);

                if (bonuses.contains(BonusType.BEAM)) {
                    SimulationResult beamResult = tryBeamBonus(board, player, op, rotation);
                    if (beamResult != null && (bestResult == null || compareResults(beamResult, bestResult) > 0)) {
                        bestResult = beamResult;
                    }
                }

                if (bonuses.contains(BonusType.SWAP)) {
                    SimulationResult swapResult = trySwapBonus(board, player, op, rotation);
                    if (swapResult != null && (bestResult == null || compareResults(swapResult, bestResult) > 0)) {
                        bestResult = swapResult;
                    }
                }

                if (bonuses.contains(BonusType.PUSH_FIXED)) {
                    SimulationResult pushFixedResult = tryPushFixedBonus(board, player, op, rotation);
                    if (pushFixedResult != null && (bestResult == null || compareResults(pushFixedResult, bestResult) > 0)) {
                        bestResult = pushFixedResult;
                    }
                }

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

    private SimulationResult tryBeamBonus(Board board, Player player, ShiftOperation op, int rotation) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        for (int i = 0; i < rotation; i++) sim.rotateExtraTile();
        if (!sim.applyShift(op)) return null;

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        Set<Position> directlyReachable = sim.getReachablePositions();

        for (Position beamDest : directlyReachable) {
            if (sim.isPositionBlocked(beamDest)) continue;

            BoardSimulator simFromBeam = sim.copy();
            simFromBeam.simulateBeam(beamDest);

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

    private SimulationResult trySwapBonus(Board board, Player player, ShiftOperation op, int rotation) {
        if (allPlayers == null || allPlayers.size() <= 1) return null;

        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        for (int i = 0; i < rotation; i++) sim.rotateExtraTile();
        if (!sim.applyShift(op)) return null;

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        for (Player other : allPlayers) {
            if (other.getId().equals(player.getId())) continue;
            if (other.getCurrentPosition() == null) continue;

            BoardSimulator simAfterSwap = sim.copy();
            Position otherPos = other.getCurrentPosition();
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

    private SimulationResult tryPushFixedBonus(Board board, Player player, ShiftOperation op, int rotation) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        for (int i = 0; i < rotation; i++) sim.rotateExtraTile();
        if (!sim.applyShift(op)) return null;

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        List<ShiftOperation> pushFixedCandidates = sim.getPushFixedCandidates();

        for (ShiftOperation fixedOp : pushFixedCandidates) {
            for (int fixedRotation = 0; fixedRotation < 4; fixedRotation++) {
                BoardSimulator simAfterFixed = sim.copy();
                for (int i = 0; i < fixedRotation; i++) simAfterFixed.rotateExtraTile();
                simAfterFixed.applyShiftIgnoringFixed(fixedOp);

                Set<Position> reachable = simAfterFixed.getReachableUnblockedPositions();
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

    private SimulationResult tryPushTwiceBonus(Board board, Player player, ShiftOperation firstOp, int firstRotation, List<ShiftOperation> allCandidates) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        for (int i = 0; i < firstRotation; i++) sim.rotateExtraTile();
        if (!sim.applyShift(firstOp)) return null;

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        ShiftOperation forbiddenSecond = getReversePush(firstOp);

        for (ShiftOperation secondOp : allCandidates) {
            if (isEqualShiftOp(secondOp, forbiddenSecond)) continue;

            for (int secondRotation = 0; secondRotation < 4; secondRotation++) {
                BoardSimulator simAfterSecond = sim.copy();
                for (int i = 0; i < secondRotation; i++) simAfterSecond.rotateExtraTile();
                if (!simAfterSecond.applyShift(secondOp)) continue;

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
        return op.isRow() ? ShiftOperation.row(op.index(), reverseDir) : ShiftOperation.column(op.index(), reverseDir);
    }

    /**
     * Compares two simulation results. Higher score wins; if tied, prefers lower distance then more steps.
     */
    private int compareResults(SimulationResult a, SimulationResult b) {
        if (a.score() != b.score()) {
            return Integer.compare(a.score(), b.score());
        }
        if (a.distanceToTarget() != b.distanceToTarget()) {
            return Integer.compare(b.distanceToTarget(), a.distanceToTarget());
        }
        return Integer.compare(a.stepsToMove(), b.stepsToMove());
    }

    private int manhattanDistance(Position a, Position b) {
        return Math.abs(a.getRow() - b.getRow()) + Math.abs(a.getColumn() - b.getColumn());
    }
}
