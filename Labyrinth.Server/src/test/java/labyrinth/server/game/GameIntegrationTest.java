package labyrinth.server.game;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.AiStrategy;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.factories.BoardFactory;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.results.MovePlayerToTileResult;
import labyrinth.server.game.results.ShiftResult;
import labyrinth.server.game.services.GameLogger;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Integration tests for the Game class.
 * Tests full game flow, state transitions, and complex interactions.
 */
class GameIntegrationTest {

    private Game game;
    private IGameTimer gameTimer;
    private AiStrategy aiStrategy;
    private GameLogger gameLogger;
    private GameConfig gameConfig;
    private Board board;
    private List<TreasureCard> treasureCards;

    @BeforeEach
    void setUp() {
        gameTimer = mock(IGameTimer.class);
        aiStrategy = mock(AiStrategy.class);
        gameLogger = new GameLogger();

        game = GameTestHelper.createGame(gameTimer, aiStrategy, gameLogger);
        gameConfig = GameConfig.getDefault();
        board = new BoardFactory().createBoard(7, 7);
        treasureCards = new ArrayList<>();
        IntStream.range(0, 24).forEach(i ->
                treasureCards.add(new TreasureCard(i, "Treasure" + i, "img" + i + ".png")));
    }

    @Nested
    class GameLifecycle {

        @Test
        void shouldStartInLobbyState() {
            // Assert
            assertEquals(RoomState.LOBBY, game.getRoomState());
        }

        @Test
        void shouldTransitionToInGameOnStart() {
            // Arrange
            game.join("Player1");

            // Act
            game.startGame(gameConfig, treasureCards, board);

            // Assert
            assertEquals(RoomState.IN_GAME, game.getRoomState());
        }

        @Test
        void shouldThrowExceptionWhenStartingGameTwice() {
            // Arrange
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    game.startGame(gameConfig, treasureCards, board)
            );
        }

        @Test
        void shouldThrowExceptionWhenJoiningDuringGame() {
            // Arrange
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    game.join("Player2")
            );
        }

        @Test
        void shouldThrowExceptionWhenStartingWithNoPlayers() {
            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    game.startGame(gameConfig, treasureCards, board)
            );
        }

        @Test
        void shouldFillWithAiPlayersOnStart() {
            // Arrange
            game.join("HumanPlayer");

            // Act
            game.startGame(gameConfig, treasureCards, board);

            // Assert
            assertEquals(4, game.getPlayers().size());
        }
    }

    @Nested
    class PlayerJoinAndLeave {

        @Test
        void shouldAddPlayerSuccessfully() {
            // Act
            Player player = game.join("TestPlayer");

            // Assert
            assertNotNull(player);
            assertEquals("TestPlayer", player.getUsername());
            assertEquals(1, game.getPlayers().size());
        }

        @Test
        void shouldGetPlayerById() {
            // Arrange
            Player player = game.join("TestPlayer");

            // Act
            Player found = game.getPlayer(player.getId());

            // Assert
            assertEquals(player, found);
        }

        @Test
        void shouldRemovePlayerFromLobby() {
            // Arrange
            Player player = game.join("TestPlayer");

            // Act
            game.leave(player);

            // Assert
            assertEquals(0, game.getPlayers().size());
        }
    }

    @Nested
    class TurnMechanics {

        @BeforeEach
        void startGame() {
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);
        }

        @Test
        void shouldStartInPlaceTileState() {
            // Assert
            assertEquals(MoveState.PLACE_TILE, game.getCurrentMoveState());
        }

        @Test
        void shouldHaveFirstPlayerAsCurrentPlayer() {
            // Assert
            Player currentPlayer = game.getCurrentPlayer();
            assertNotNull(currentPlayer);
            assertEquals(0, game.getCurrentPlayerIndex());
        }

        @Test
        void shouldRotateExtraTileClockwise() {
            // Arrange
            Player player = game.getCurrentPlayer();
            Tile extraTile = board.getExtraTile();
            var entrancesBefore = extraTile.getEntrances();

            // Act
            game.rotateExtraTileClockwise(player);

            // Assert - entrances should have changed
            assertNotEquals(entrancesBefore, extraTile.getEntrances());
        }

        @Test
        void shouldThrowExceptionWhenWrongPlayerRotates() {
            // Arrange - get a player who is not current
            Player notCurrentPlayer = game.getPlayers().stream()
                    .filter(p -> p != game.getCurrentPlayer())
                    .findFirst()
                    .orElseThrow();

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    game.rotateExtraTileClockwise(notCurrentPlayer)
            );
        }
    }

    @Nested
    class BoardShifting {

        @BeforeEach
        void startGame() {
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);
        }

        @Test
        void shouldShiftBoardSuccessfully() {
            // Arrange
            Player player = game.getCurrentPlayer();

            // Act
            ShiftResult result = game.shift(1, Direction.DOWN, player);

            // Assert
            assertTrue(result.shiftSuccess());
        }

        @Test
        void shouldTransitionToMoveStateAfterShift() {
            // Arrange
            Player player = game.getCurrentPlayer();

            // Act
            game.shift(1, Direction.DOWN, player);

            // Assert
            assertEquals(MoveState.MOVE, game.getCurrentMoveState());
        }

        @Test
        void shouldNotShiftFixedColumn() {
            // Arrange
            Player player = game.getCurrentPlayer();

            // Act - column 2 has fixed tiles
            ShiftResult result = game.shift(2, Direction.DOWN, player);

            // Assert
            assertFalse(result.shiftSuccess());
        }

        @Test
        void shouldIncreasePlayerScoreAfterShift() {
            // Arrange
            Player player = game.getCurrentPlayer();
            int initialScore = player.getStatistics().getScore();

            // Act
            game.shift(1, Direction.DOWN, player);

            // Assert
            assertTrue(player.getStatistics().getScore() > initialScore);
        }

        @Test
        void shouldIncrementTilesPushedStat() {
            // Arrange
            Player player = game.getCurrentPlayer();

            // Act
            game.shift(1, Direction.DOWN, player);

            // Assert
            assertEquals(1, player.getStatistics().getTilesPushed());
        }

        @Test
        void shouldThrowExceptionWhenShiftingInMoveState() {
            // Arrange
            Player player = game.getCurrentPlayer();
            game.shift(1, Direction.DOWN, player); // Now in MOVE state

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    game.shift(3, Direction.UP, player)
            );
        }
    }

    @Nested
    class PlayerMovement {

        @BeforeEach
        void startGameAndShift() {
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);
            // Shift to get into MOVE state
            Player player = game.getCurrentPlayer();
            game.shift(1, Direction.DOWN, player);
        }

        @Test
        void shouldMovePlayerToReachableTile() {
            // Arrange
            Player player = game.getCurrentPlayer();
            var currentPos = game.getCurrentPositionOfPlayer(player);
            var reachableTiles = board.getReachableTiles(player);
            var targetTile = reachableTiles.stream()
                    .filter(t -> t != player.getCurrentTile())
                    .findFirst()
                    .orElse(null);

            if (targetTile != null) {
                var targetPos = board.getPositionOfTile(targetTile);

                // Act
                MovePlayerToTileResult result = game.movePlayerToTile(
                        targetPos.row(),
                        targetPos.column(),
                        player
                );

                // Assert
                assertTrue(result.moveSuccess());
            }
        }

        @Test
        void shouldAdvanceToNextPlayerAfterMove() {
            // Arrange
            Player firstPlayer = game.getCurrentPlayer();
            var currentPos = game.getCurrentPositionOfPlayer(firstPlayer);

            // Act - move to same position (always reachable)
            game.movePlayerToTile(currentPos.row(), currentPos.column(), firstPlayer);

            // Assert - should have advanced to next player
            assertEquals(1, game.getCurrentPlayerIndex());
        }
    }

    @Nested
    class BonusIntegration {

        @BeforeEach
        void startGame() {
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);
        }

        @Test
        void shouldActivatePushTwiceBonus() {
            // Arrange
            Player player = game.getCurrentPlayer();
            player.getBonuses().add(BonusTypes.PUSH_TWICE);

            // Act
            boolean result = game.useBonus(BonusTypes.PUSH_TWICE);

            // Assert
            assertTrue(result);
            assertEquals(BonusTypes.PUSH_TWICE, game.getActiveBonus());
        }

        @Test
        void shouldActivatePushFixedBonus() {
            // Arrange
            Player player = game.getCurrentPlayer();
            player.getBonuses().add(BonusTypes.PUSH_FIXED);

            // Act
            boolean result = game.useBonus(BonusTypes.PUSH_FIXED);

            // Assert
            assertTrue(result);
            assertEquals(BonusTypes.PUSH_FIXED, game.getActiveBonus());
        }

        @Test
        void shouldAllowTwoPushesWithPushTwiceBonus() {
            // Arrange
            Player player = game.getCurrentPlayer();
            player.getBonuses().add(BonusTypes.PUSH_TWICE);
            game.useBonus(BonusTypes.PUSH_TWICE);

            // Act - first push
            ShiftResult result1 = game.shift(1, Direction.DOWN, player);

            // Assert - should still be in PLACE_TILE state for second push
            assertTrue(result1.shiftSuccess());
            assertEquals(MoveState.PLACE_TILE, game.getCurrentMoveState());

            // Act - second push
            ShiftResult result2 = game.shift(3, Direction.UP, player);

            // Assert - should now be in MOVE state
            assertTrue(result2.shiftSuccess());
            assertEquals(MoveState.MOVE, game.getCurrentMoveState());
        }

        @Test
        void shouldAllowShiftingFixedColumnWithPushFixedBonus() {
            // Arrange
            Player player = game.getCurrentPlayer();
            player.getBonuses().add(BonusTypes.PUSH_FIXED);
            game.useBonus(BonusTypes.PUSH_FIXED);

            // Act - column 2 has fixed tiles but should work with bonus
            ShiftResult result = game.shift(2, Direction.DOWN, player);

            // Assert
            assertTrue(result.shiftSuccess());
        }
    }

    @Nested
    class ResetAndReturnToLobby {

        @BeforeEach
        void startAndFinishGame() {
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);
        }

        @Test
        void shouldResetToLobbyState() {
            // Act
            game.resetAndReturnToLobby();

            // Assert
            assertEquals(RoomState.LOBBY, game.getRoomState());
        }

        @Test
        void shouldClearBoard() {
            // Act
            game.resetAndReturnToLobby();

            // Assert
            assertNull(game.getBoard());
        }

        @Test
        void shouldRemoveAiPlayers() {
            // Arrange
            int aiCount = (int) game.getPlayers().stream()
                    .filter(Player::isAiActive)
                    .count();
            assertTrue(aiCount > 0, "Should have AI players");

            // Act
            game.resetAndReturnToLobby();

            // Assert
            long remainingAi = game.getPlayers().stream()
                    .filter(Player::isAiActive)
                    .count();
            assertEquals(0, remainingAi);
        }

        @Test
        void shouldResetHumanPlayerState() {
            // Arrange
            Player humanPlayer = game.getPlayers().stream()
                    .filter(p -> !p.isAiActive())
                    .findFirst()
                    .orElseThrow();

            // Act
            game.resetAndReturnToLobby();

            // Assert
            assertTrue(humanPlayer.getAssignedTreasureCards().isEmpty());
            assertTrue(humanPlayer.getBonuses().isEmpty());
            assertNull(humanPlayer.getCurrentTile());
        }
    }

    @Nested
    class AiToggle {

        @BeforeEach
        void setupPlayer() {
            game.join("Player1");
        }

        @Test
        void shouldToggleAiStatus() {
            // Arrange
            Player player = game.getPlayers().get(0);
            boolean initialAiStatus = player.isAiActive();

            // Act
            game.toggleAiForPlayer(player);

            // Assert
            assertEquals(!initialAiStatus, player.isAiActive());
        }

        @Test
        void shouldToggleBackToOriginal() {
            // Arrange
            Player player = game.getPlayers().get(0);
            boolean initialAiStatus = player.isAiActive();

            // Act
            game.toggleAiForPlayer(player);
            game.toggleAiForPlayer(player);

            // Assert
            assertEquals(initialAiStatus, player.isAiActive());
        }
    }

    @Nested
    class GameLogging {

        @Test
        void shouldLogGameEvents() {
            // Arrange
            game.join("Player1");

            // Act
            game.startGame(gameConfig, treasureCards, board);
            Player player = game.getCurrentPlayer();
            game.shift(1, Direction.DOWN, player);

            // Assert
            assertFalse(game.getExecutionLogs().isEmpty());
        }
    }

    @Nested
    class FreeRoamMode {

        @BeforeEach
        void startGame() {
            game.join("Player1");
            game.startGame(gameConfig, treasureCards, board);
        }

        @Test
        void shouldAllowAnyPlayerToActInFreeRoam() {
            // Arrange
            board.setFreeRoam(true);
            Player notCurrentPlayer = game.getPlayers().stream()
                    .filter(p -> p != game.getCurrentPlayer())
                    .findFirst()
                    .orElseThrow();

            // Act & Assert - should not throw
            assertDoesNotThrow(() ->
                    game.shift(1, Direction.DOWN, notCurrentPlayer)
            );
        }

        @Test
        void shouldBypassMoveStateCheckInFreeRoam() {
            // Arrange
            board.setFreeRoam(true);
            Player player = game.getCurrentPlayer();
            game.shift(1, Direction.DOWN, player); // Now in MOVE state

            // Act & Assert - should not throw even though we're in MOVE state
            assertDoesNotThrow(() ->
                    game.shift(3, Direction.UP, player)
            );
        }
    }

    @Nested
    class MaxPlayers {

        @Test
        void shouldReturnCorrectMaxPlayers() {
            // Assert
            assertEquals(4, game.getMaxPlayers());
        }

        @Test
        void shouldNotAllowMoreThanMaxPlayers() {
            // Arrange
            game.join("Player1");
            game.join("Player2");
            game.join("Player3");
            game.join("Player4");

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    game.join("Player5")
            );
        }
    }
}
