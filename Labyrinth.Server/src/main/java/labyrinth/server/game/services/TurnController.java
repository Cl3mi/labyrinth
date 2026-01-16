package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.abstractions.ITurnController;
import labyrinth.server.game.enums.GameLogType;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.records.GameConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.function.Consumer;

/**
 * Manages turn state and player rotation in the game.
 * Encapsulates the state machine for whose turn it is and what phase of the
 * turn they are in.
 */
public class TurnController implements ITurnController {

    private static final Logger log = LoggerFactory.getLogger(TurnController.class);

    private int currentPlayerIndex = 0;
    private MoveState currentMoveState = MoveState.PLACE_TILE;
    private boolean bonusUsedThisTurn = false;

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
     * Gets the current player index.
     */
    public int getCurrentPlayerIndex() {
        return currentPlayerIndex;
    }

    /**
     * Gets the current move state.
     */
    public MoveState getCurrentMoveState() {
        return currentMoveState;
    }

    /**
     * Sets the move state.
     */
    public void setMoveState(MoveState moveState) {
        this.currentMoveState = moveState;
    }

    /**
     * Checks if a bonus has been used this turn.
     */
    public boolean isBonusUsedThisTurn() {
        return bonusUsedThisTurn;
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
        turnTimer.stop();

        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }

        Player nextPlayer = getCurrentPlayer(players);
        gameLogger.log(GameLogType.NEXT_TURN, "New Player to move: " + nextPlayer.getUsername(), nextPlayer, null);
        currentMoveState = MoveState.PLACE_TILE;
        bonusUsedThisTurn = false;

        onNextPlayerCallback.accept(nextPlayer);

        if (nextPlayer.shouldMoveBePerformedByAi()) {
            aiTurnExecutor.accept(nextPlayer);
        } else {
            turnTimer.start(gameConfig.turnTimeInSeconds(), () -> {
                log.warn("[TurnController] Turn timer exceeded for player: {}. Advancing to next player.", nextPlayer.getUsername());
                gameLogger.log(GameLogType.NEXT_TURN, "Turn timer exceeded - auto-advancing", nextPlayer, null);
                advanceToNextPlayer(players, roomState, gameConfig, aiTurnExecutor);
            });
        }
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
    public java.time.OffsetDateTime getTurnEndTime() {
        return turnTimer.getExpirationTime();
    }
}
