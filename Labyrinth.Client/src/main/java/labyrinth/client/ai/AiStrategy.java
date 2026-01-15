package labyrinth.client.ai;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;

/**
 * Interface for AI strategy implementations.
 */
public interface AiStrategy {

    /**
     * Computes the best move for the given player on the given board.
     *
     * @param board the current board state
     * @param player the player to compute the move for
     * @return the best move decision, or null if no valid move found
     */
    AiDecision computeBestMove(Board board, Player player);
}
