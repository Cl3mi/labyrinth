package labyrinth.client.ai;

import labyrinth.client.models.Position;

/**
 * Result of a board simulation for AI decision making.
 */
public record SimulationResult(
    ShiftOperation shift,
    int rotations,
    int score,
    int distanceToTarget,
    Position targetMovePosition
) {
    public static final int SCORE_TARGET_REACHABLE = 100;
    public static final int SCORE_MOVING_CLOSER = 10;
    public static final int SCORE_FALLBACK = 1;
    public static final int SCORE_NO_MOVE = 0;
}
