package labyrinth.client.ai;

import labyrinth.client.models.Position;

/**
 * Represents an AI decision for a move.
 */
public record AiDecision(
    ShiftOperation shift,
    int rotations,
    Position targetPosition,
    SimulationResult.BonusAction bonusAction
) {
    /**
     * Constructor without bonus action (backward compatible).
     */
    public AiDecision(ShiftOperation shift, int rotations, Position targetPosition) {
        this(shift, rotations, targetPosition, null);
    }

    /**
     * Creates an AiDecision from a simulation result.
     */
    public static AiDecision from(SimulationResult result) {
        if (result == null) return null;
        return new AiDecision(result.shift(), result.rotations(), result.targetMovePosition(), result.bonusAction());
    }
}
