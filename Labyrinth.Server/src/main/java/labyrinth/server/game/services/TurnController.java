package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.abstractions.ITurnController;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.GameLogType;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.LastShift;
import lombok.Getter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Manages turn state and player rotation in the game.
 * Encapsulates the state machine for whose turn it is and what phase of the
 * turn they are in.
 */
public class TurnController implements ITurnController {

    private static final Logger log = LoggerFactory.getLogger(TurnController.class);

    /**
     * -- GETTER --
     *  Gets the current player index.
     */
    @Getter
    private int currentPlayerIndex = 0;
    /**
     * -- GETTER --
     *  Gets the current move state.
     */
    @Getter
    private MoveState currentMoveState = MoveState.PLACE_TILE;
    /**
     * -- GETTER --
     *  Checks if a bonus has been used this turn.
     */
    @Getter
    private boolean bonusUsedThisTurn = false;

    /**
     * Tracks the last shift performed. Used to prevent reversing the previous shift.
     */
    private LastShift lastShift = null;

    private final IGameTimer turnTimer;
    private final GameLogger gameLogger;

    private Consumer<Player> onNextPlayerCallback;

    public TurnController(IGameTimer turnTimer,
                          GameLogger gameLogger) {
        this.turnTimer = turnTimer;
        this.gameLogger = gameLogger;
    }

    public void setOnNextPlayer(Consumer<Player> callback) {
        this.onNextPlayerCallback = callback;
    }

    /**
     * Gets the current player based on the player list.
     */
    public Player getCurrentPlayer(List<Player> players) {
        return players.get(currentPlayerIndex);
    }

    /**
     * Sets the move state.
     */
    public void setMoveState(MoveState moveState) {
        this.currentMoveState = moveState;
    }

    /**
     * Marks that a bonus has been used this turn.
     */
    public void markBonusUsed() {
        this.bonusUsedThisTurn = true;
    }

    /**
     * Advances to the next player and handles AI turns.
     *
     * @param players        the list of players
     * @param roomState      the current room state (must be IN_GAME)
     * @param gameConfig     the game configuration
     * @param aiTurnExecutor callback to execute AI turn if next player is AI
     */
    public void advanceToNextPlayer(
            List<Player> players,
            RoomState roomState,
            GameConfig gameConfig,
            Consumer<Player> aiTurnExecutor
    ) {
        guardForRoomState(roomState, RoomState.IN_GAME);

        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }

        Player nextPlayer = getCurrentPlayer(players);
        beginTurn(nextPlayer, "New Player to move: " + nextPlayer.getUsername(), players, roomState, gameConfig, aiTurnExecutor);
    }


    private void beginTurn(
            Player player,
            String logMessage,
            List<Player> players,
            RoomState roomState,
            GameConfig gameConfig,
            Consumer<Player> aiTurnExecutor
    ) {
        guardForRoomState(roomState, RoomState.IN_GAME);
        // Ensure any previous timer is stopped
        turnTimer.stop();

        gameLogger.log(GameLogType.NEXT_TURN, logMessage, player, null);
        currentMoveState = MoveState.PLACE_TILE;
        bonusUsedThisTurn = false;

        if (onNextPlayerCallback != null) {
            onNextPlayerCallback.accept(player);
        }

        if (player.shouldMoveBePerformedByAi()) {
            aiTurnExecutor.accept(player);
        } else {
            turnTimer.start(gameConfig.turnTimeInSeconds(), () -> {
                log.warn("[TurnController] Turn timer exceeded for player: {}. Advancing to next player.", player.getUsername());
                gameLogger.log(GameLogType.NEXT_TURN, "Turn timer exceeded - auto-advancing", player, null);
                advanceToNextPlayer(players, roomState, gameConfig, aiTurnExecutor);
            });
        }
    }

    /**
     * Starts the turn handling for the current player (without advancing the index).
     * Used when a game starts to begin the first player's timer or trigger AI.
     */
    @Override
    public void startTurn(
            List<Player> players,
            RoomState roomState,
            GameConfig gameConfig,
            Consumer<Player> aiTurnExecutor
    ) {
        Player currentPlayer = getCurrentPlayer(players);
        beginTurn(currentPlayer, "Starting turn for player: " + currentPlayer.getUsername(), players, roomState, gameConfig, aiTurnExecutor);
    }

    /**
     * Guards that the current move state matches the expected state.
     *
     * @param board    the board (to check freeRoam mode)
     * @param expected the expected move state
     * @throws IllegalStateException if state doesn't match and not in freeRoam
     */
    public void guardForMoveState(Board board, MoveState expected) {
        if (board.isFreeRoam()) {
            return;
        }
        if (this.currentMoveState != expected) {
            throw new IllegalStateException(
                    "Illegal move state. Expected " + expected + " but was " + currentMoveState);
        }
    }

    /**
     * Guards that the room state matches the expected state.
     *
     * @param current  the current room state
     * @param expected the expected room state
     * @throws IllegalStateException if states don't match
     */
    public void guardForRoomState(RoomState current, RoomState expected) {
        if (current != expected) {
            throw new IllegalStateException("Illegal room state. Expected " + expected + " but was " + current);
        }
    }

    /**
     * Guards that the player is the current player whose turn it is.
     *
     * @param board        the board (to check freeRoam mode)
     * @param players      the list of players
     * @param playerToMove the player attempting to move
     * @throws IllegalStateException if it's not this player's turn and not in
     *                               freeRoam
     */
    public void guardForPlayer(Board board, List<Player> players, Player playerToMove) {
        if (board.isFreeRoam()) {
            return;
        }
        Player expectedPlayer = players.get(currentPlayerIndex);
        if (!expectedPlayer.equals(playerToMove)) {
            throw new IllegalStateException("Illegal player. Expected " + expectedPlayer.getId()
                    + " but got " + playerToMove.getId());
        }
    }

    /**
     * Resets the turn controller for a new game.
     */
    public void reset() {
        currentPlayerIndex = 0;
        currentMoveState = MoveState.PLACE_TILE;
        bonusUsedThisTurn = false;
        lastShift = null;
        stopTimer();
    }

    /**
     * Stops the turn timer.
     */
    public void stopTimer() {
        turnTimer.stop();
    }

    /**
     * Gets the turn timer's expiration time.
     */
    public OffsetDateTime getTurnEndTime() {
        return turnTimer.getExpirationTime();
    }

    /**
     * Gets the last shift performed, if any.
     *
     * @return an Optional containing the last shift, or empty if no shift has been performed
     */
    @Override
    public Optional<LastShift> getLastShift() {
        return Optional.ofNullable(lastShift);
    }

    /**
     * Records a shift operation.
     *
     * @param index     the row or column index that was shifted
     * @param direction the direction of the shift
     */
    @Override
    public void recordShift(int index, Direction direction) {
        this.lastShift = new LastShift(index, direction);
    }

    /**
     * Checks if the proposed shift would reverse the last shift.
     *
     * @param index     the proposed shift index
     * @param direction the proposed shift direction
     * @return true if this shift would reverse the last shift
     */
    @Override
    public boolean wouldReverseLastShift(int index, Direction direction) {
        if (lastShift == null) {
            return false;
        }
        return lastShift.isReversedBy(index, direction);
    }
}
