package labyrinth.client.ai;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.PushActionInfo;
import labyrinth.contracts.models.Treasure;

import java.util.*;

/**
 * AI strategy with the following priority:
 * 1. If all treasures collected and can reach home -> finish game
 * 2. Try to reach treasure with any valid push + rotation
 * 3. Use SWAP if opponent stands on our treasure
 * 4. Use BEAM/SWAP to collect two treasures in one turn
 * 5. Try to reach a bonus tile
 * 6. Try using owned bonuses to reach treasure
 * 7. Try to block opponents or push treasure closer
 * 8. Move as close to treasure as possible while maximizing steps
 */
public class SimpleAiStrategy implements AiStrategy {

    private static final double DISTANCE_WEIGHT = 3.0;
    private static final double STEPS_WEIGHT = 1.0;

    private final Random random = new Random();
    private List<Player> allPlayers;
    private Player currentPlayer;

    @Override
    public AiDecision computeBestMove(Board board, Player player) {
        return computeBestMove(board, player, null);
    }

    public AiDecision computeBestMove(Board board, Player player, List<Player> allPlayers) {
        this.allPlayers = allPlayers;
        this.currentPlayer = player;
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
                if (evaluatedRotations.contains(rotationKey)) continue;
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

        // Check if opponent stands on our treasure - use SWAP to steal it
        if (!goingHome && player.getAvailableBonuses().contains(BonusType.SWAP)) {
            SimulationResult swapStealResult = trySwapToStealTreasure(board, player);
            if (swapStealResult != null) {
                System.out.println("[AI] Using SWAP to steal treasure from opponent!");
                return AiDecision.from(swapStealResult);
            }
        }

        // Try BEAM/SWAP to collect two treasures in one turn
        if (!goingHome && !player.getAvailableBonuses().isEmpty()) {
            SimulationResult doubleTreasureResult = tryDoubleTreasureCollection(board, player);
            if (doubleTreasureResult != null && doubleTreasureResult.score() >= SimulationResult.SCORE_DOUBLE_TREASURE) {
                System.out.println("[AI] Can collect TWO treasures with bonus!");
                return AiDecision.from(doubleTreasureResult);
            }
        }

        // Try to reach a bonus tile - ALWAYS prefer bonus over just moving closer
        if (!goingHome) {
            SimulationResult bonusResult = findBestBonusMove(board, player, candidates);
            if (bonusResult != null && bonusResult.score() == SimulationResult.SCORE_BONUS_REACHABLE) {
                // Always collect bonus if we can't reach treasure directly
                System.out.println("[AI] Collecting bonus (always preferred over moving closer)");
                return AiDecision.from(bonusResult);
            }
        }

        // Try using bonuses to reach target
        if (!goingHome && !player.getAvailableBonuses().isEmpty()) {
            SimulationResult bonusUseResult = tryUsingBonusesToReachTarget(board, player, candidates);
            if (bonusUseResult != null && bonusUseResult.score() >= SimulationResult.SCORE_TARGET_WITH_BONUS) {
                System.out.println("[AI] Using bonus to reach target!");
                return AiDecision.from(bonusUseResult);
            }
        }

        // Try to block opponent or push treasure closer
        if (!goingHome && bestResult != null && bestResult.score() <= SimulationResult.SCORE_MOVING_CLOSER) {
            SimulationResult strategicResult = findStrategicMove(board, player, candidates, bestResult);
            if (strategicResult != null && compareResults(strategicResult, bestResult) > 0) {
                System.out.println("[AI] Found strategic move (block/push treasure)!");
                return AiDecision.from(strategicResult);
            }
        }

        if (bestResult == null && !candidates.isEmpty()) {
            ShiftOperation fallbackOp = candidates.get(random.nextInt(candidates.size()));
            Position currentPos = player.getCurrentPosition();
            bestResult = new SimulationResult(fallbackOp, 0, SimulationResult.SCORE_NO_MOVE, 0, currentPos, 0);
            System.out.println("[AI] Using fallback shift");
        }

        // STUCK DETECTION: If the AI would stay on the same tile, try to move far away
        if (bestResult != null) {
            Position currentPos = player.getCurrentPosition();
            Position targetMovePos = bestResult.targetMovePosition();

            if (targetMovePos != null && targetMovePos.equals(currentPos)) {
                System.out.println("[AI] STUCK DETECTED - would stay on same tile, searching for escape move");
                SimulationResult escapeResult = findEscapeMove(board, player, candidates);
                if (escapeResult != null && escapeResult.stepsToMove() > 0) {
                    System.out.println("[AI] Found escape move - moving " + escapeResult.stepsToMove() + " steps away");
                    bestResult = escapeResult;
                }
            }
        }

        if (bestResult != null && bestResult.shift() != null) {
            System.out.println("[AI] Best move: " + bestResult.shift().direction() + " at " +
                    bestResult.shift().index() + ", score=" + bestResult.score() +
                    ", rotations=" + bestResult.rotations() + ", steps=" + bestResult.stepsToMove());
        }

        return AiDecision.from(bestResult);
    }

    /**
     * Find a move that gets the AI as far away as possible from current position.
     * Used when stuck detection triggers.
     */
    private SimulationResult findEscapeMove(Board board, Player player, List<ShiftOperation> candidates) {
        SimulationResult bestEscape = null;
        int maxDistance = 0;

        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) continue;
                evaluatedRotations.add(rotationKey);

                BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
                for (int i = 0; i < rotation; i++) sim.rotateExtraTile();
                if (!sim.applyShift(op)) continue;

                Set<Position> reachable = sim.getReachableUnblockedPositions();
                Position currentPos = sim.getPlayerPosition();

                // Find the farthest reachable position
                for (Position pos : reachable) {
                    int distance = manhattanDistance(currentPos, pos);
                    if (distance > maxDistance) {
                        maxDistance = distance;
                        bestEscape = new SimulationResult(op, rotation, SimulationResult.SCORE_FALLBACK, 0, pos, distance);
                    }
                }
            }
        }

        return bestEscape;
    }

    /**
     * Try to use SWAP when an opponent is standing on our target treasure.
     * SWAP replaces the push - no shift is performed.
     */
    private SimulationResult trySwapToStealTreasure(Board board, Player player) {
        if (allPlayers == null || allPlayers.size() <= 1) return null;

        Treasure targetTreasure = player.getCurrentTargetTreasure();
        if (targetTreasure == null) return null;

        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        // Check if any opponent is standing on our treasure (no shift - current board state)
        for (Player other : allPlayers) {
            if (other.getId().equals(player.getId())) continue;
            if (other.getCurrentPosition() == null) continue;

            Position otherPos = other.getCurrentPosition();

            if (otherPos.equals(targetPos)) {
                // Opponent is on our treasure! SWAP with them
                // After swap we'll be on the treasure, then we can still move
                System.out.println("[AI] Opponent " + other.getName() + " is on our treasure - using SWAP!");
                return new SimulationResult(
                    null, 0, SimulationResult.SCORE_TARGET_REACHABLE, 0, targetPos,
                    SimulationResult.BonusAction.swap(other.getId(), otherPos)
                );
            }
        }

        return null;
    }

    /**
     * Try to collect two treasures using BEAM or SWAP (teleport to one, walk to another).
     * BEAM/SWAP replace the push - no shift is performed.
     */
    private SimulationResult tryDoubleTreasureCollection(Board board, Player player) {
        List<BonusType> bonuses = player.getAvailableBonuses();
        if (!bonuses.contains(BonusType.BEAM) && !bonuses.contains(BonusType.SWAP)) return null;

        if (player.getRemainingTreasureCount() < 2) return null;

        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        Position currentTreasurePos = sim.getTargetPosition();
        if (currentTreasurePos == null) return null;

        // Try BEAM: teleport to treasure tile, then walk to next treasure
        if (bonuses.contains(BonusType.BEAM)) {
            // Beam directly to current treasure
            if (!sim.isPositionBlocked(currentTreasurePos)) {
                BoardSimulator simFromBeam = sim.copy();
                simFromBeam.simulateBeam(currentTreasurePos);

                Set<Position> reachableFromBeam = simFromBeam.getReachableUnblockedPositions();
                Position nextTreasurePos = findNextTreasurePosition(simFromBeam, player);

                if (nextTreasurePos != null && reachableFromBeam.contains(nextTreasurePos)) {
                    System.out.println("[AI] Can BEAM to treasure and walk to next one!");
                    return new SimulationResult(
                        null, 0, SimulationResult.SCORE_DOUBLE_TREASURE, 0, nextTreasurePos,
                        SimulationResult.BonusAction.beam(currentTreasurePos)
                    );
                }
            }
        }

        // Try SWAP: swap with player on treasure, then walk to next
        if (bonuses.contains(BonusType.SWAP) && allPlayers != null) {
            for (Player other : allPlayers) {
                if (other.getId().equals(player.getId())) continue;
                if (other.getCurrentPosition() == null) continue;

                Position otherPos = other.getCurrentPosition();
                if (!otherPos.equals(currentTreasurePos)) continue;

                // Swap puts us on treasure
                BoardSimulator simAfterSwap = sim.copy();
                simAfterSwap.simulateSwap(otherPos);

                Set<Position> reachableAfterSwap = simAfterSwap.getReachableUnblockedPositions();
                Position nextTreasurePos = findNextTreasurePosition(simAfterSwap, player);

                if (nextTreasurePos != null && reachableAfterSwap.contains(nextTreasurePos)) {
                    System.out.println("[AI] Can SWAP to treasure and walk to next one!");
                    return new SimulationResult(
                        null, 0, SimulationResult.SCORE_DOUBLE_TREASURE, 0, nextTreasurePos,
                        SimulationResult.BonusAction.swap(other.getId(), otherPos)
                    );
                }
            }
        }

        return null;
    }

    /**
     * Find strategic moves: block opponents or push treasure closer.
     */
    private SimulationResult findStrategicMove(Board board, Player player, List<ShiftOperation> candidates, SimulationResult currentBest) {
        SimulationResult bestStrategic = null;

        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) continue;
                evaluatedRotations.add(rotationKey);

                BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
                for (int i = 0; i < rotation; i++) sim.rotateExtraTile();
                if (!sim.applyShift(op)) continue;

                // Check if this push moves our treasure closer to us
                SimulationResult pushTreasureResult = evaluateTreasurePush(sim, op, rotation, player);
                if (pushTreasureResult != null && (bestStrategic == null || compareResults(pushTreasureResult, bestStrategic) > 0)) {
                    bestStrategic = pushTreasureResult;
                }

                // Check if this push blocks an opponent
                SimulationResult blockResult = evaluateOpponentBlock(sim, op, rotation, player);
                if (blockResult != null && (bestStrategic == null || compareResults(blockResult, bestStrategic) > 0)) {
                    bestStrategic = blockResult;
                }
            }
        }

        return bestStrategic;
    }

    /**
     * Evaluate if a push moves our treasure closer to us.
     */
    private SimulationResult evaluateTreasurePush(BoardSimulator sim, ShiftOperation op, int rotation, Player player) {
        Position targetPos = sim.getTargetPosition();
        Position playerPos = sim.getPlayerPosition();
        if (targetPos == null) return null;

        int newDistance = manhattanDistance(playerPos, targetPos);

        // Compare with original distance (before push)
        Position origTarget = sim.getTargetTreasurePosition();
        if (origTarget == null) return null;

        int origDistance = manhattanDistance(player.getCurrentPosition(), origTarget);

        // If push moved treasure closer, this is good
        if (newDistance < origDistance) {
            Set<Position> reachable = sim.getReachableUnblockedPositions();
            Position bestMove = findBestPositionWithSteps(reachable, targetPos, playerPos, Collections.emptySet());
            if (bestMove != null) {
                int distAfterMove = manhattanDistance(bestMove, targetPos);
                int steps = manhattanDistance(playerPos, bestMove);
                // Give bonus score for pushing treasure closer
                int score = SimulationResult.SCORE_MOVING_CLOSER + (origDistance - newDistance);
                return new SimulationResult(op, rotation, score, distAfterMove, bestMove, steps);
            }
        }

        return null;
    }

    /**
     * Evaluate if a push blocks an opponent from reaching their treasure.
     */
    private SimulationResult evaluateOpponentBlock(BoardSimulator sim, ShiftOperation op, int rotation, Player player) {
        if (allPlayers == null || allPlayers.size() <= 1) return null;

        Position playerPos = sim.getPlayerPosition();
        Position targetPos = sim.getTargetPosition();

        for (Player opponent : allPlayers) {
            if (opponent.getId().equals(player.getId())) continue;
            if (opponent.getCurrentPosition() == null) continue;
            if (opponent.getCurrentTargetTreasure() == null) continue;

            // Find opponent's treasure position after our shift
            Position opponentTreasurePos = findTreasurePosition(sim, opponent.getCurrentTargetTreasure());
            if (opponentTreasurePos == null) continue;

            // Simulate opponent's position after shift
            Position opponentPosAfterShift = simulatePlayerPositionAfterShift(
                opponent.getCurrentPosition(), op, sim.getWidth(), sim.getHeight());

            // Check if opponent could reach their treasure before our move
            // This is a simplified check - just see if they're close
            int opponentDistBefore = manhattanDistance(opponent.getCurrentPosition(), opponentTreasurePos);
            int opponentDistAfter = manhattanDistance(opponentPosAfterShift, opponentTreasurePos);

            // If we pushed opponent away from their treasure, that's good
            if (opponentDistAfter > opponentDistBefore) {
                Set<Position> reachable = sim.getReachableUnblockedPositions();
                Position bestMove = targetPos != null ?
                    findBestPositionWithSteps(reachable, targetPos, playerPos, Collections.emptySet()) :
                    findFarthestPosition(reachable, playerPos);

                if (bestMove != null) {
                    int dist = targetPos != null ? manhattanDistance(bestMove, targetPos) : 0;
                    int steps = manhattanDistance(playerPos, bestMove);
                    // Give bonus for blocking opponent
                    int blockBonus = opponentDistAfter - opponentDistBefore;
                    int score = SimulationResult.SCORE_MOVING_CLOSER + blockBonus;
                    return new SimulationResult(op, rotation, score, dist, bestMove, steps);
                }
            }
        }

        return null;
    }

    private Position findTreasurePosition(BoardSimulator sim, Treasure treasure) {
        if (treasure == null) return null;
        for (int r = 0; r < sim.getHeight(); r++) {
            for (int c = 0; c < sim.getWidth(); c++) {
                BoardSimulator.SimTile tile = sim.getTileAt(new Position(r, c));
                if (tile != null && tile.getTreasure() != null &&
                    tile.getTreasure().getId() == treasure.getId()) {
                    return new Position(r, c);
                }
            }
        }
        return null;
    }

    private Position findNextTreasurePosition(BoardSimulator sim, Player player) {
        // This is a simplification - we'd need server info about next treasure
        // For now, just look for any treasure on the board that's not our current target
        Treasure currentTarget = player.getCurrentTargetTreasure();

        for (int r = 0; r < sim.getHeight(); r++) {
            for (int c = 0; c < sim.getWidth(); c++) {
                BoardSimulator.SimTile tile = sim.getTileAt(new Position(r, c));
                if (tile != null && tile.getTreasure() != null) {
                    if (currentTarget == null || tile.getTreasure().getId() != currentTarget.getId()) {
                        return new Position(r, c);
                    }
                }
            }
        }
        return null;
    }

    private Position simulatePlayerPositionAfterShift(Position pos, ShiftOperation op, int width, int height) {
        if (op.isRow() && pos.getRow() == op.index()) {
            if (op.direction() == Direction.RIGHT) {
                return new Position(pos.getRow(), (pos.getColumn() + 1) % width);
            } else {
                return new Position(pos.getRow(), (pos.getColumn() - 1 + width) % width);
            }
        } else if (!op.isRow() && pos.getColumn() == op.index()) {
            if (op.direction() == Direction.DOWN) {
                return new Position((pos.getRow() + 1) % height, pos.getColumn());
            } else {
                return new Position((pos.getRow() - 1 + height) % height, pos.getColumn());
            }
        }
        return pos;
    }

    private int getRotationKey(labyrinth.contracts.models.Tile tile, int rotation) {
        Direction[] entrances = tile.getEntrances();
        if (entrances == null || entrances.length == 0) return 0;

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

    private Position findBestPositionWithSteps(Set<Position> reachable, Position target, Position current, Set<Position> blocked) {
        Position best = null;
        double bestScore = Double.NEGATIVE_INFINITY;

        for (Position pos : reachable) {
            if (blocked.contains(pos)) continue;

            int distToTarget = manhattanDistance(pos, target);
            int steps = manhattanDistance(current, pos);
            double score = -distToTarget * DISTANCE_WEIGHT + steps * STEPS_WEIGHT;

            if (score > bestScore) {
                bestScore = score;
                best = pos;
            }
        }

        return best;
    }

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

        // BEAM and SWAP replace the push - try them without any shift
        if (bonuses.contains(BonusType.BEAM)) {
            SimulationResult beamResult = tryBeamBonus(board, player);
            if (beamResult != null && (bestResult == null || compareResults(beamResult, bestResult) > 0)) {
                bestResult = beamResult;
            }
        }

        if (bonuses.contains(BonusType.SWAP)) {
            SimulationResult swapResult = trySwapBonus(board, player);
            if (swapResult != null && (bestResult == null || compareResults(swapResult, bestResult) > 0)) {
                bestResult = swapResult;
            }
        }

        // PUSH_FIXED and PUSH_TWICE modify push behavior - iterate through candidates
        for (ShiftOperation op : candidates) {
            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int rotation = 0; rotation < 4; rotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), rotation);
                if (evaluatedRotations.contains(rotationKey)) continue;
                evaluatedRotations.add(rotationKey);

                if (bonuses.contains(BonusType.PUSH_FIXED)) {
                    SimulationResult pushFixedResult = tryPushFixedBonus(board, player, rotation);
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

    /**
     * BEAM replaces the push - teleport anywhere on the board, then walk.
     */
    private SimulationResult tryBeamBonus(Board board, Player player) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        // BEAM can teleport to any tile on the board
        for (int r = 0; r < sim.getHeight(); r++) {
            for (int c = 0; c < sim.getWidth(); c++) {
                Position beamDest = new Position(r, c);
                if (sim.isPositionBlocked(beamDest)) continue;

                BoardSimulator simFromBeam = sim.copy();
                simFromBeam.simulateBeam(beamDest);

                Set<Position> reachableFromBeam = simFromBeam.getReachableUnblockedPositions();
                if (reachableFromBeam.contains(targetPos)) {
                    // No shift operation - BEAM replaces push
                    return new SimulationResult(
                        null, 0, SimulationResult.SCORE_TARGET_WITH_BONUS, 0, targetPos,
                        SimulationResult.BonusAction.beam(beamDest)
                    );
                }
            }
        }

        return null;
    }

    /**
     * SWAP replaces the push - swap with another player, then walk.
     */
    private SimulationResult trySwapBonus(Board board, Player player) {
        if (allPlayers == null || allPlayers.size() <= 1) return null;

        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        for (Player other : allPlayers) {
            if (other.getId().equals(player.getId())) continue;
            if (other.getCurrentPosition() == null) continue;

            Position otherPos = other.getCurrentPosition();

            BoardSimulator simAfterSwap = sim.copy();
            simAfterSwap.simulateSwap(otherPos);

            Set<Position> reachableAfterSwap = simAfterSwap.getReachableUnblockedPositions();
            if (reachableAfterSwap.contains(targetPos)) {
                // No shift operation - SWAP replaces push
                return new SimulationResult(
                    null, 0, SimulationResult.SCORE_TARGET_WITH_BONUS, 0, targetPos,
                    SimulationResult.BonusAction.swap(other.getId(), otherPos)
                );
            }
        }

        return null;
    }

    /**
     * PUSH_FIXED allows pushing rows/columns with fixed tiles.
     */
    private SimulationResult tryPushFixedBonus(Board board, Player player, int rotation) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        for (int i = 0; i < rotation; i++) sim.rotateExtraTile();

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        // Get rows/columns that have fixed tiles (normally blocked)
        List<ShiftOperation> pushFixedCandidates = sim.getPushFixedCandidates();

        for (ShiftOperation fixedOp : pushFixedCandidates) {
            BoardSimulator simAfterFixed = sim.copy();
            simAfterFixed.applyShiftIgnoringFixed(fixedOp);

            Set<Position> reachable = simAfterFixed.getReachableUnblockedPositions();
            Position newTargetPos = simAfterFixed.getTargetPosition();

            if (newTargetPos != null && reachable.contains(newTargetPos)) {
                return new SimulationResult(
                    fixedOp, rotation, SimulationResult.SCORE_TARGET_WITH_BONUS, 0, newTargetPos,
                    SimulationResult.BonusAction.pushFixed(fixedOp, rotation)
                );
            }
        }

        return null;
    }

    /**
     * PUSH_TWICE allows two consecutive pushes.
     */
    private SimulationResult tryPushTwiceBonus(Board board, Player player, ShiftOperation firstOp, int firstRotation, List<ShiftOperation> allCandidates) {
        BoardSimulator sim = new BoardSimulator(board, player, allPlayers);
        for (int i = 0; i < firstRotation; i++) sim.rotateExtraTile();
        if (!sim.applyShift(firstOp)) return null;

        Position targetPos = sim.getTargetPosition();
        if (targetPos == null) return null;

        ShiftOperation forbiddenSecond = getReversePush(firstOp);

        for (ShiftOperation secondOp : allCandidates) {
            if (isEqualShiftOp(secondOp, forbiddenSecond)) continue;

            Set<Integer> evaluatedRotations = new HashSet<>();
            for (int secondRotation = 0; secondRotation < 4; secondRotation++) {
                int rotationKey = getRotationKey(board.getExtraTile(), secondRotation);
                if (evaluatedRotations.contains(rotationKey)) continue;
                evaluatedRotations.add(rotationKey);

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
