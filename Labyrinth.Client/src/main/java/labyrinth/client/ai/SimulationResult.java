package labyrinth.client.ai;

import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;

/**
 * Result of a board simulation for AI decision making.
 */
public record SimulationResult(
    ShiftOperation shift,
    int rotations,
    int score,
    int distanceToTarget,
    Position targetMovePosition,
    BonusAction bonusAction
) {
    // Score priorities (higher = better)
    public static final int SCORE_FINISH_GAME = 1000;       // Win the game by returning home
    public static final int SCORE_TARGET_REACHABLE = 100;   // Treasure is reachable
    public static final int SCORE_BONUS_REACHABLE = 50;     // Bonus tile is reachable (when can't reach target)
    public static final int SCORE_TARGET_WITH_BONUS = 90;   // Can reach target using a bonus
    public static final int SCORE_MOVING_CLOSER = 10;       // Getting closer to target
    public static final int SCORE_FALLBACK = 1;             // Any movement
    public static final int SCORE_NO_MOVE = 0;              // No valid move

    /**
     * Constructor without bonus action (backward compatible).
     */
    public SimulationResult(ShiftOperation shift, int rotations, int score, int distanceToTarget, Position targetMovePosition) {
        this(shift, rotations, score, distanceToTarget, targetMovePosition, null);
    }

    /**
     * Represents a bonus action to be executed.
     */
    public record BonusAction(
        BonusType bonusType,
        Position targetPosition,       // For BEAM: teleport destination
        String targetPlayerId,         // For SWAP: player to swap with
        ShiftOperation pushFixedOp,    // For PUSH_FIXED: the shift operation
        int pushFixedRotations,        // For PUSH_FIXED: rotations before shift
        ShiftOperation secondPush,     // For PUSH_TWICE: second push operation
        int secondPushRotations        // For PUSH_TWICE: rotations for second push
    ) {
        /**
         * Creates a BEAM bonus action.
         */
        public static BonusAction beam(Position targetPosition) {
            return new BonusAction(BonusType.BEAM, targetPosition, null, null, 0, null, 0);
        }

        /**
         * Creates a SWAP bonus action.
         */
        public static BonusAction swap(String targetPlayerId, Position resultingPosition) {
            return new BonusAction(BonusType.SWAP, resultingPosition, targetPlayerId, null, 0, null, 0);
        }

        /**
         * Creates a PUSH_FIXED bonus action.
         */
        public static BonusAction pushFixed(ShiftOperation op, int rotations) {
            return new BonusAction(BonusType.PUSH_FIXED, null, null, op, rotations, null, 0);
        }

        /**
         * Creates a PUSH_TWICE bonus action.
         */
        public static BonusAction pushTwice(ShiftOperation secondOp, int secondRotations) {
            return new BonusAction(BonusType.PUSH_TWICE, null, null, null, 0, secondOp, secondRotations);
        }
    }
}
