package labyrinth.server.game.abstractions;

import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.records.GameConfig;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.function.Consumer;

/**
 * Interface for managing turn state and player rotation in the game.
 * Encapsulates the state machine for whose turn it is and what phase of the turn they are in.
 *
 * <p>This interface enables dependency injection and testing by decoupling
 * the Game class from the concrete TurnController implementation.</p>
 */
public interface ITurnController {

    /**
     * Gets the current player based on the player list.
     *
     * @param players the list of players
     * @return the current player
     */
    Player getCurrentPlayer(List<Player> players);

    /**
     * Gets the current player index.
     *
     * @return the current player index
     */
    int getCurrentPlayerIndex();

    /**
     * Gets the current move state.
     *
     * @return the current move state
     */
    MoveState getCurrentMoveState();

    /**
     * Sets the move state.
     *
     * @param moveState the new move state
     */
    void setMoveState(MoveState moveState);

    /**
     * Checks if a bonus has been used this turn.
     *
     * @return true if a bonus was used this turn, false otherwise
     */
    boolean isBonusUsedThisTurn();

    /**
     * Marks that a bonus has been used this turn.
     */
    void markBonusUsed();

    /**
     * Advances to the next player and handles AI turns.
     *
     * @param players        the list of players
     * @param roomState      the current room state (must be IN_GAME)
     * @param gameConfig     the game configuration
     * @param aiTurnExecutor callback to execute AI turn if next player is AI
     */
    void advanceToNextPlayer(
            List<Player> players,
            RoomState roomState,
            GameConfig gameConfig,
            Consumer<Player> aiTurnExecutor
    );

    /**
     * Guards that the current move state matches the expected state.
     *
     * @param board    the board (to check freeRoam mode)
     * @param expected the expected move state
     * @throws IllegalStateException if state doesn't match and not in freeRoam
     */
    void guardForMoveState(Board board, MoveState expected);

    /**
     * Guards that the room state matches the expected state.
     *
     * @param current  the current room state
     * @param expected the expected room state
     * @throws IllegalStateException if states don't match
     */
    void guardForRoomState(RoomState current, RoomState expected);

    /**
     * Guards that the player is the current player whose turn it is.
     *
     * @param board        the board (to check freeRoam mode)
     * @param players      the list of players
     * @param playerToMove the player attempting to move
     * @throws IllegalStateException if it's not this player's turn and not in freeRoam
     */
    void guardForPlayer(Board board, List<Player> players, Player playerToMove);

    /**
     * Resets the turn controller for a new game.
     */
    void reset();

    /**
     * Stops the turn timer.
     */
    void stopTimer();

    /**
     * Gets the turn timer's expiration time.
     *
     * @return the turn end time
     */
    OffsetDateTime getTurnEndTime();
}
