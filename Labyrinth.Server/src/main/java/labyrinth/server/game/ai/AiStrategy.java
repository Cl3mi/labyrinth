package labyrinth.server.game.ai;

import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.results.MovePlayerToTileResult;

import java.util.function.Consumer;

public interface AiStrategy {
    void performTurn(Game game, Player player);

    /**
     * Sets the callback that will be invoked after each AI action to broadcast
     * the updated game state to all clients.
     *
     * @param callback the callback to invoke after AI moves
     */
    default void setBroadcastCallback(Runnable callback) {
        // Default implementation does nothing - override in concrete strategies
    }

    /**
     * Sets the callback for handling move results (treasures collected, game over, etc.)
     * This allows the AI to trigger events like GameOverEvent when a bot wins.
     *
     * @param callback the callback that receives the MovePlayerToTileResult
     */
    default void setMoveResultCallback(Consumer<MovePlayerToTileResult> callback) {
        // Default implementation does nothing - override in concrete strategies
    }
}