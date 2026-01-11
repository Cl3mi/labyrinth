package labyrinth.server.game;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class GameServiceTest {

    @Mock
    private Game mockLobby; // Mock the Game dependency

    @Mock
    private IGameTimer mockGameTimer; // Mock game timer for Game constructor

    @InjectMocks
    private GameService gameService; // Inject mocks into GameService

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this); // Initialize mocks
    }

    // ===== Admin Reassignment Tests (using real Game object) =====

    @Test
    void testAdminReassignmentOnLeave() {
        // Create real Game object for testing admin reassignment logic
        IGameTimer mockTimer = mock(IGameTimer.class);
        labyrinth.server.game.ai.AiStrategy mockAiStrategy = mock(labyrinth.server.game.ai.AiStrategy.class);
        labyrinth.server.game.services.GameLogger mockGameLogger = mock(labyrinth.server.game.services.GameLogger.class);
        labyrinth.server.game.services.GameInitializer mockGameInitializer = mock(labyrinth.server.game.services.GameInitializer.class);
        Game game = new Game(mockTimer, mockAiStrategy, mockGameLogger, mockGameInitializer);

        // Add two players
        Player player1 = game.join("Player1");
        Player player2 = game.join("Player2");

        // player1 should be admin (first player)
        assertTrue(player1.isAdmin());
        assertFalse(player2.isAdmin());

        // Player 1 leaves
        game.leave(player1);

        // Player 2 should now be admin
        assertTrue(player2.isAdmin());
        assertEquals(1, game.getPlayers().size());
    }

    @Test
    void testAdminReassignmentPrefersHuman() {
        IGameTimer mockTimer = mock(IGameTimer.class);
        labyrinth.server.game.ai.AiStrategy mockAiStrategy = mock(labyrinth.server.game.ai.AiStrategy.class);
        labyrinth.server.game.services.GameLogger mockGameLogger = mock(labyrinth.server.game.services.GameLogger.class);
        labyrinth.server.game.services.GameInitializer mockGameInitializer = mock(labyrinth.server.game.services.GameInitializer.class);
        Game game = new Game(mockTimer, mockAiStrategy, mockGameLogger, mockGameInitializer);

        // Add human player
        Player human = game.join("HumanPlayer");

        // Add AI player
        Player ai = game.join("AIPlayer");
        ai.setAiActive(true);

        // Human should be admin (first player)
        assertTrue(human.isAdmin());
        assertFalse(ai.isAdmin());

        // Human leaves
        game.leave(human);

        // AI should become admin (only remaining player)
        assertTrue(ai.isAdmin());

        // Add another human
        Player human2 = game.join("HumanPlayer2");

        // AI still admin
        assertTrue(ai.isAdmin());
        assertFalse(human2.isAdmin());

        // AI leaves
        game.leave(ai);

        // Human2 should become admin (prefers human over AI)
        assertTrue(human2.isAdmin());
    }

    @Test
    void testLeaveEmptyLobby() {
        IGameTimer mockTimer = mock(IGameTimer.class);
        labyrinth.server.game.ai.AiStrategy mockAiStrategy = mock(labyrinth.server.game.ai.AiStrategy.class);
        labyrinth.server.game.services.GameLogger mockGameLogger = mock(labyrinth.server.game.services.GameLogger.class);
        labyrinth.server.game.services.GameInitializer mockGameInitializer = mock(labyrinth.server.game.services.GameInitializer.class);
        Game game = new Game(mockTimer, mockAiStrategy, mockGameLogger, mockGameInitializer);

        Player player1 = game.join("Player1");
        assertTrue(player1.isAdmin());

        // Last player leaves
        game.leave(player1);

        // Lobby should be empty
        assertTrue(game.getPlayers().isEmpty());
    }

    @Test
    void testNonAdminLeave_NoReassignment() {
        IGameTimer mockTimer = mock(IGameTimer.class);
        labyrinth.server.game.ai.AiStrategy mockAiStrategy = mock(labyrinth.server.game.ai.AiStrategy.class);
        labyrinth.server.game.services.GameLogger mockGameLogger = mock(labyrinth.server.game.services.GameLogger.class);
        labyrinth.server.game.services.GameInitializer mockGameInitializer = mock(labyrinth.server.game.services.GameInitializer.class);
        Game game = new Game(mockTimer, mockAiStrategy, mockGameLogger, mockGameInitializer);

        Player player1 = game.join("Player1"); // Admin
        Player player2 = game.join("Player2"); // Not admin

        assertTrue(player1.isAdmin());
        assertFalse(player2.isAdmin());

        // Non-admin player leaves
        game.leave(player2);

        // Admin should remain the same
        assertTrue(player1.isAdmin());
        assertEquals(1, game.getPlayers().size());
    }

    @Test
    void testAdminReassignmentWithMultiplePlayers() {
        IGameTimer mockTimer = mock(IGameTimer.class);
        labyrinth.server.game.ai.AiStrategy mockAiStrategy = mock(labyrinth.server.game.ai.AiStrategy.class);
        labyrinth.server.game.services.GameLogger mockGameLogger = mock(labyrinth.server.game.services.GameLogger.class);
        labyrinth.server.game.services.GameInitializer mockGameInitializer = mock(labyrinth.server.game.services.GameInitializer.class);
        Game game = new Game(mockTimer, mockAiStrategy, mockGameLogger, mockGameInitializer);

        Player player1 = game.join("Player1"); // Admin
        Player player2 = game.join("Player2");
        Player player3 = game.join("Player3");

        assertTrue(player1.isAdmin());

        // Admin leaves
        game.leave(player1);

        // Player2 should become admin (first remaining player)
        assertTrue(player2.isAdmin());
        assertFalse(player3.isAdmin());

        assertEquals(2, game.getPlayers().size());
    }
}
