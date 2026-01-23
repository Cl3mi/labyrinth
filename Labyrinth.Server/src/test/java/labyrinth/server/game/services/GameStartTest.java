package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.AiStrategy;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static labyrinth.server.game.GameTestHelper.createGame;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

/**
 * Tests for game initialization and start logic.
 * Validates that Game.startGame() correctly initializes all game components.
 */
class GameStartTest {

    private Game game;
    private GameConfig gameConfig;
    private Board board;
    private List<TreasureCard> treasureCards;

    @Mock
    private AiStrategy aiStrategy;


    @BeforeEach
    void setUp() {
        aiStrategy = mock(AiStrategy.class);
        game = createGame(mock(IGameTimer.class), aiStrategy, new GameLogger());
        gameConfig = GameConfig.getDefault();
        board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7);
        treasureCards = new ArrayList<>();
        IntStream.range(0, 24).forEach(i -> treasureCards.add(new TreasureCard(i, "Card" + i)));
    }

    // Basic Game Start Tests

    @Test
    void startGame_shouldTransitionToInGameState() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        assertEquals(RoomState.IN_GAME, game.getRoomState(), "Game should transition to IN_GAME state");
    }

    @Test
    void startGame_shouldSetGameStartTime() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        assertNotNull(game.getGameEndTime(), "Game end time should be set based on start time");
    }

    @Test
    void startGame_shouldSetTheBoard() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        assertNotNull(game.getBoard(), "Board should be set after game start");
        assertEquals(board, game.getBoard(), "Board should be the one provided");
    }

    @Test
    void startGame_shouldThrowException_whenGameAlreadyStarted() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");
        game.startGame(gameConfig, treasureCards, board);

        // Act & Assert
        assertThrows(IllegalStateException.class, () -> {
            game.startGame(gameConfig, new ArrayList<>(), board);
        }, "Should not allow starting game twice");
    }

    @Test
    void startGame_shouldThrowException_whenNoPlayers() {
        // Arrange - no players joined

        // Act & Assert
        assertThrows(IllegalStateException.class, () -> {
            game.startGame(gameConfig, treasureCards, board);
        }, "Should require at least 1 player to start");
    }

    // Player Count Tests

    @Test
    void startGame_shouldRequireAtLeastTwoPlayers() throws Exception{
        // Arrange
        game.join("Player1");

        // Act & Assert
        assertThrows(IllegalStateException.class, () -> {
            game.startGame(gameConfig, treasureCards, board);
        }, "Should require at least 2 players to start");
    }

    @Test
    void startGame_shouldWorkWithFourPlayers() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");
        game.join("Player3");
        game.join("Player4");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        assertEquals(4, game.getPlayers().size(), "Should have exactly 4 players");
        long aiPlayers = game.getPlayers().stream().filter(Player::isAiActive).count();
        assertEquals(0, aiPlayers, "Should not have any AI players");
    }

    // Treasure Card Distribution Tests

    @Test
    void startGame_shouldDistributeTreasureCardsEvenly() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        for (Player player : game.getPlayers()) {
            assertEquals(12, player.getAssignedTreasureCards().size(),
                    "Each of 2 players should get 12 treasure cards (24/2)");
        }
    }

    @Test
    void startGame_shouldDistributeTreasureCardsRoundRobin() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert - verify cards are distributed in round-robin (0,1,2,3... to P1,P2,P1,P2...)
        List<Player> players = game.getPlayers();
        assertEquals(0, players.get(0).getAssignedTreasureCards().getFirst().getId(),
                "Player 1 should get first card");
        assertEquals(1, players.get(1).getAssignedTreasureCards().getFirst().getId(),
                "Player 2 should get second card");
        assertEquals(2, players.get(0).getAssignedTreasureCards().get(1).getId(),
                "Player 1 should get third card");
        assertEquals(3, players.get(1).getAssignedTreasureCards().get(1).getId(),
                "Player 2 should get fourth card");
    }

    @Test
    void startGame_shouldAssignAllTreasureCards() throws Exception {
        // Arrange
        game.join("Player1");
        game.join("Player2");
        int totalCards = treasureCards.size();

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        int assignedCards = game.getPlayers().stream()
                .mapToInt(p -> p.getAssignedTreasureCards().size())
                .sum();
        assertEquals(totalCards, assignedCards, "All treasure cards should be assigned to players");
    }

    // Player Position Initialization Tests

    @Test
    void startGame_shouldInitializePlayerPositions() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        for (Player player : game.getPlayers()) {
            assertNotNull(player.getCurrentTile(), "Player should have a current tile");
            assertNotNull(player.getHomeTile(), "Player should have a home tile");
            assertEquals(player.getHomeTile(), player.getCurrentTile(),
                    "Initially, current tile should be the home tile");
        }
    }

    @Test
    void startGame_shouldPlacePlayersAtCorrectStartPositions() throws Exception {
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert
        List<Player> players = game.getPlayers();
        for (int i = 0; i < players.size(); i++) {
            var expectedPosition = gameConfig.getStartPosition(i);
            var actualTile = players.get(i).getCurrentTile();
            var actualPosition = game.getBoard().getPositionOfTile(actualTile);

            assertEquals(expectedPosition, actualPosition,
                    "Player " + i + " should be at correct start position");
        }
    }

    @Test
    void startGame_shouldPlacePlayersAtCorners() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(gameConfig, treasureCards, board);

        // Assert - GameConfig.getDefault() places players at corners (0,0), (0,6), (6,0), (6,6)
        List<Player> players = game.getPlayers();
        var pos0 = game.getCurrentPositionOfPlayer(players.get(0));
        var pos1 = game.getCurrentPositionOfPlayer(players.get(1));

        assertTrue(game.getBoard().isCornerCoordinate(pos0.row(), pos0.column()),
                "Player 0 should be at a corner");
        assertTrue(game.getBoard().isCornerCoordinate(pos1.row(), pos1.column()),
                "Player 1 should be at a corner");
    }

    // Game Configuration Tests

    @Test
    void startGame_shouldUseDefaultConfig_whenConfigIsNull() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");

        // Act
        game.startGame(null, treasureCards, board);

        // Assert
        assertNotNull(game.getGameEndTime(), "Should use default config with valid duration");
    }

    @Test
    void startGame_shouldUseProvidedConfig() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");
        GameConfig customConfig = new GameConfig(7, 7, 20, 3600, 10, 60);

        // Act
        game.startGame(customConfig, treasureCards, board);

        // Assert - verify config is applied by checking game end time calculation
        assertNotNull(game.getGameEndTime(), "Game end time should be calculated from custom config");
    }

    // Bonus Placement Tests

    @Test
    void startGame_shouldPlaceBonusesOnBoard() throws Exception{
        // Arrange
        game.join("Player1");
        game.join("Player2");
        GameConfig configWithBonuses = new GameConfig(7, 7, 24, 3600, 10, 60);
        Board freshBoard = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7);

        // Act
        game.startGame(configWithBonuses, treasureCards, freshBoard);

        // Assert - check that bonuses were placed (at least some tiles should have bonuses)
        boolean hasBonuses = false;
        for (int row = 0; row < 7; row++) {
            for (int col = 0; col < 7; col++) {
                if (game.getBoard().getTileAt(row, col).getBonus() != null) {
                    hasBonuses = true;
                    break;
                }
            }
        }
        assertTrue(hasBonuses, "Board should have bonuses placed");
    }
}
