package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.BiMap;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Tests for the TurnController class.
 * Validates turn advancement, state guards, move state transitions, and timer management.
 */
class TurnControllerTest {

    private TurnController turnController;
    private IGameTimer turnTimer;
    private GameLogger gameLogger;
    private List<Player> players;
    private Board board;

    @BeforeEach
    void setUp() {
        turnTimer = mock(IGameTimer.class);
        gameLogger = new GameLogger();
        turnController = new TurnController(turnTimer, gameLogger);
        turnController.setOnNextPlayer(player -> {});

        // Create test players
        players = new ArrayList<>();
        for (int i = 0; i < 4; i++) {
            Player player = new Player(UUID.randomUUID(), "Player" + (i + 1));
            players.add(player);
        }

        // Create a simple board
        BiMap<Position, Tile> tileMap = new BiMap<>();
        for (int row = 0; row < 3; row++) {
            for (int col = 0; col < 3; col++) {
                tileMap.put(new Position(row, col),
                        new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            }
        }
        board = new Board(3, 3, tileMap, new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
    }

    @Nested
    class InitialState {

        @Test
        void shouldStartAtPlayerIndexZero() {
            // Assert
            assertEquals(0, turnController.getCurrentPlayerIndex());
        }

        @Test
        void shouldStartInPlaceTileState() {
            // Assert
            assertEquals(MoveState.PLACE_TILE, turnController.getCurrentMoveState());
        }

        @Test
        void shouldStartWithBonusNotUsed() {
            // Assert
            assertFalse(turnController.isBonusUsedThisTurn());
        }
    }

    @Nested
    class GetCurrentPlayer {

        @Test
        void shouldReturnFirstPlayerInitially() {
            // Act
            Player currentPlayer = turnController.getCurrentPlayer(players);

            // Assert
            assertEquals(players.get(0), currentPlayer);
        }

        @Test
        void shouldReturnCorrectPlayerAfterIndexChange() {
            // Arrange
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Act
            Player currentPlayer = turnController.getCurrentPlayer(players);

            // Assert
            assertEquals(players.get(1), currentPlayer);
        }
    }

    @Nested
    class AdvanceToNextPlayer {

        @Test
        void shouldIncrementPlayerIndex() {
            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert
            assertEquals(1, turnController.getCurrentPlayerIndex());
        }

        @Test
        void shouldWrapAroundToFirstPlayer() {
            // Arrange - advance to last player
            for (int i = 0; i < 3; i++) {
                turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});
            }
            assertEquals(3, turnController.getCurrentPlayerIndex());

            // Act - advance one more
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert
            assertEquals(0, turnController.getCurrentPlayerIndex());
        }

        @Test
        void shouldResetToPlaceTileState() {
            // Arrange
            turnController.setMoveState(MoveState.MOVE);

            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert
            assertEquals(MoveState.PLACE_TILE, turnController.getCurrentMoveState());
        }

        @Test
        void shouldResetBonusUsedFlag() {
            // Arrange
            turnController.markBonusUsed();
            assertTrue(turnController.isBonusUsedThisTurn());

            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert
            assertFalse(turnController.isBonusUsedThisTurn());
        }

        @Test
        void shouldStopTimerBeforeAdvancing() {
            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert
            verify(turnTimer).stop();
        }

        @Test
        void shouldStartTimerForHumanPlayer() {
            // Arrange - make sure first player is not AI
            players.get(1).setAiActive(false);
            players.get(1).setDisconnected(false);

            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert
            verify(turnTimer).start(anyInt(), any());
        }

        @Test
        void shouldExecuteAiTurnForAiPlayer() {
            // Arrange
            players.get(1).setAiActive(true);
            players.get(1).setDisconnected(true);
            Consumer<Player> aiExecutor = mock(Consumer.class);

            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), aiExecutor);

            // Assert
            verify(aiExecutor).accept(players.get(1));
        }

        @Test
        void shouldNotStartTimerForAiPlayer() {
            // Arrange
            players.get(1).setAiActive(true);
            players.get(1).setDisconnected(true);

            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert - timer.start should only be called for human players
            verify(turnTimer, never()).start(anyInt(), any());
        }

        @Test
        void shouldThrowExceptionWhenNotInGame() {
            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    turnController.advanceToNextPlayer(players, RoomState.LOBBY, GameConfig.getDefault(), p -> {})
            );
        }

        @Test
        void shouldCallOnNextPlayerCallback() {
            // Arrange
            AtomicReference<Player> callbackPlayer = new AtomicReference<>();
            turnController.setOnNextPlayer(callbackPlayer::set);

            // Act
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});

            // Assert
            assertEquals(players.get(1), callbackPlayer.get());
        }
    }

    @Nested
    class SetMoveState {

        @Test
        void shouldSetMoveState() {
            // Act
            turnController.setMoveState(MoveState.MOVE);

            // Assert
            assertEquals(MoveState.MOVE, turnController.getCurrentMoveState());
        }
    }

    @Nested
    class MarkBonusUsed {

        @Test
        void shouldMarkBonusAsUsed() {
            // Act
            turnController.markBonusUsed();

            // Assert
            assertTrue(turnController.isBonusUsedThisTurn());
        }
    }

    @Nested
    class GuardForMoveState {

        @Test
        void shouldNotThrowWhenStateMatches() {
            // Arrange
            turnController.setMoveState(MoveState.PLACE_TILE);

            // Act & Assert
            assertDoesNotThrow(() ->
                    turnController.guardForMoveState(board, MoveState.PLACE_TILE)
            );
        }

        @Test
        void shouldThrowWhenStateDoesNotMatch() {
            // Arrange
            turnController.setMoveState(MoveState.PLACE_TILE);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    turnController.guardForMoveState(board, MoveState.MOVE)
            );
        }

        @Test
        void shouldBypassGuardInFreeRoamMode() {
            // Arrange
            board.setFreeRoam(true);
            turnController.setMoveState(MoveState.PLACE_TILE);

            // Act & Assert - should not throw even though states don't match
            assertDoesNotThrow(() ->
                    turnController.guardForMoveState(board, MoveState.MOVE)
            );
        }
    }

    @Nested
    class GuardForRoomState {

        @Test
        void shouldNotThrowWhenRoomStateMatches() {
            // Act & Assert
            assertDoesNotThrow(() ->
                    turnController.guardForRoomState(RoomState.IN_GAME, RoomState.IN_GAME)
            );
        }

        @Test
        void shouldThrowWhenRoomStateDoesNotMatch() {
            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    turnController.guardForRoomState(RoomState.LOBBY, RoomState.IN_GAME)
            );
        }

        @Test
        void shouldThrowForFinishedWhenExpectingInGame() {
            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    turnController.guardForRoomState(RoomState.FINISHED, RoomState.IN_GAME)
            );
        }
    }

    @Nested
    class GuardForPlayer {

        @Test
        void shouldNotThrowWhenCorrectPlayer() {
            // Arrange
            Player currentPlayer = players.get(0);

            // Act & Assert
            assertDoesNotThrow(() ->
                    turnController.guardForPlayer(board, players, currentPlayer)
            );
        }

        @Test
        void shouldThrowWhenWrongPlayer() {
            // Arrange
            Player wrongPlayer = players.get(1);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    turnController.guardForPlayer(board, players, wrongPlayer)
            );
        }

        @Test
        void shouldBypassGuardInFreeRoamMode() {
            // Arrange
            board.setFreeRoam(true);
            Player wrongPlayer = players.get(1);

            // Act & Assert - should not throw even though it's the wrong player
            assertDoesNotThrow(() ->
                    turnController.guardForPlayer(board, players, wrongPlayer)
            );
        }
    }

    @Nested
    class Reset {

        @Test
        void shouldResetPlayerIndexToZero() {
            // Arrange
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});
            assertEquals(1, turnController.getCurrentPlayerIndex());

            // Act
            turnController.reset();

            // Assert
            assertEquals(0, turnController.getCurrentPlayerIndex());
        }

        @Test
        void shouldResetMoveStateToPlaceTile() {
            // Arrange
            turnController.setMoveState(MoveState.MOVE);

            // Act
            turnController.reset();

            // Assert
            assertEquals(MoveState.PLACE_TILE, turnController.getCurrentMoveState());
        }

        @Test
        void shouldResetBonusUsedFlag() {
            // Arrange
            turnController.markBonusUsed();

            // Act
            turnController.reset();

            // Assert
            assertFalse(turnController.isBonusUsedThisTurn());
        }

        @Test
        void shouldStopTimer() {
            // Act
            turnController.reset();

            // Assert
            verify(turnTimer).stop();
        }
    }

    @Nested
    class StopTimer {

        @Test
        void shouldStopTurnTimer() {
            // Act
            turnController.stopTimer();

            // Assert
            verify(turnTimer).stop();
        }
    }

    @Nested
    class TurnTimerCallback {

        @Test
        void shouldAutoAdvanceWhenTimerExpires() {
            // Arrange
            ArgumentCaptor<Runnable> callbackCaptor = ArgumentCaptor.forClass(Runnable.class);

            // Act - trigger the first advance which should start a timer
            turnController.advanceToNextPlayer(players, RoomState.IN_GAME, GameConfig.getDefault(), p -> {});
            verify(turnTimer).start(anyInt(), callbackCaptor.capture());

            // Reset mocks to track the next call
            reset(turnTimer);

            // Simulate timer expiration
            callbackCaptor.getValue().run();

            // Assert - should have advanced to next player
            assertEquals(2, turnController.getCurrentPlayerIndex());
        }
    }
}
