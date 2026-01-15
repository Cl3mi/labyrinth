package labyrinth.client.ai;

import labyrinth.client.models.Position;

/**
 * Represents an AI decision for a move.
 */
public record AiDecision(
    ShiftOperation shift,
    int rotations,
    Position targetPosition
) {
    /**
     * Creates an AiDecision from a simulation result.
     */
    public static AiDecision from(SimulationResult result) {
        if (result == null) return null;
        return new AiDecision(result.shift(), result.rotations(), result.targetMovePosition());
    }
}
