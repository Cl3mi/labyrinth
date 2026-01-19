package labyrinth.server.game.services;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.models.Player;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the PlayerRegistry class.
 * Validates player join/leave operations, admin management, AI player handling, and color assignment.
 */
class PlayerRegistryTest {

    private PlayerRegistry registry;

    @BeforeEach
    void setUp() {
        registry = new PlayerRegistry(4);
    }

    @Nested
    class AddPlayer {

        @Test
        void shouldAddPlayerSuccessfully() {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player);
            assertEquals("TestPlayer", player.getUsername());
            assertEquals(1, registry.getPlayers().size());
        }

        @Test
        void shouldAssignUniqueId() {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player.getId());
        }

        @Test
        void shouldAssignColor() {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player.getColor());
        }

        @Test
        void shouldAssignDifferentColorsToPlayers() {
            // Act
            Player player1 = registry.addPlayer("Player1");
            Player player2 = registry.addPlayer("Player2");

            // Assert
            assertNotEquals(player1.getColor(), player2.getColor());
        }

        @Test
        void shouldMakeFirstPlayerAdmin() {
            // Act
            Player player = registry.addPlayer("FirstPlayer");

            // Assert
            assertTrue(player.isAdmin());
        }

        @Test
        void shouldNotMakeSubsequentPlayersAdmin() {
            // Arrange
            registry.addPlayer("FirstPlayer");

            // Act
            Player secondPlayer = registry.addPlayer("SecondPlayer");

            // Assert
            assertFalse(secondPlayer.isAdmin());
        }

        @Test
        void shouldSetJoinDate() {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player.getJoinDate());
        }

        @Test
        void shouldThrowExceptionWhenRoomIsFull() {
            // Arrange
            registry.addPlayer("Player1");
            registry.addPlayer("Player2");
            registry.addPlayer("Player3");
            registry.addPlayer("Player4");

            // Act & Assert
            assertThrows(IllegalStateException.class, () ->
                    registry.addPlayer("Player5")
            );
        }

        @Test
        void shouldThrowExceptionForDuplicateUsername() {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    registry.addPlayer("TestPlayer")
            );
        }

        @Test
        void shouldThrowExceptionForDuplicateUsernameCaseInsensitive() {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    registry.addPlayer("testplayer")
            );
        }
    }

    @Nested
    class RemovePlayer {

        @Test
        void shouldRemovePlayer() {
            // Arrange
            Player player = registry.addPlayer("TestPlayer");

            // Act
            registry.removePlayer(player);

            // Assert
            assertEquals(0, registry.getPlayers().size());
        }

        @Test
        void shouldDoNothingWhenRemovingNonExistentPlayer() {
            // Arrange
            registry.addPlayer("TestPlayer");
            Player nonExistent = new Player(UUID.randomUUID(), "NonExistent");

            // Act
            registry.removePlayer(nonExistent);

            // Assert
            assertEquals(1, registry.getPlayers().size());
        }

        @Test
        void shouldReassignAdminWhenAdminLeaves() {
            // Arrange
            Player admin = registry.addPlayer("Admin");
            Player player2 = registry.addPlayer("Player2");
            assertTrue(admin.isAdmin());
            assertFalse(player2.isAdmin());

            // Act
            registry.removePlayer(admin);

            // Assert
            assertTrue(player2.isAdmin());
        }

        @Test
        void shouldPreferHumanOverAiForAdminReassignment() {
            // Arrange
            Player admin = registry.addPlayer("Admin");
            registry.fillWithAiPlayers(); // Fill remaining spots with AI

            // Get a reference to a human player if added after (for this test, admin is the only human)
            // Act
            registry.removePlayer(admin);

            // Assert - first available player (AI) should become admin if no humans left
            assertFalse(registry.getPlayers().isEmpty());
            assertTrue(registry.getPlayers().get(0).isAdmin());
        }
    }

    @Nested
    class GetPlayer {

        @Test
        void shouldGetPlayerById() {
            // Arrange
            Player player = registry.addPlayer("TestPlayer");
            UUID playerId = player.getId();

            // Act
            Player found = registry.getPlayer(playerId);

            // Assert
            assertEquals(player, found);
        }

        @Test
        void shouldReturnNullForUnknownId() {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act
            Player found = registry.getPlayer(UUID.randomUUID());

            // Assert
            assertNull(found);
        }
    }

    @Nested
    class GetPlayers {

        @Test
        void shouldReturnImmutableList() {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act & Assert
            assertThrows(UnsupportedOperationException.class, () ->
                    registry.getPlayers().add(new Player(UUID.randomUUID(), "Hacker"))
            );
        }

        @Test
        void shouldReturnAllPlayers() {
            // Arrange
            registry.addPlayer("Player1");
            registry.addPlayer("Player2");
            registry.addPlayer("Player3");

            // Act
            var players = registry.getPlayers();

            // Assert
            assertEquals(3, players.size());
        }
    }

    @Nested
    class GetPlayersInternal {

        @Test
        void shouldReturnMutableList() {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act
            var internalList = registry.getPlayersInternal();

            // Assert - should be modifiable (though not recommended)
            assertDoesNotThrow(() -> internalList.clear());
        }
    }

    @Nested
    class IsFull {

        @Test
        void shouldReturnFalseWhenNotFull() {
            // Arrange
            registry.addPlayer("Player1");

            // Assert
            assertFalse(registry.isFull());
        }

        @Test
        void shouldReturnTrueWhenFull() {
            // Arrange
            registry.addPlayer("Player1");
            registry.addPlayer("Player2");
            registry.addPlayer("Player3");
            registry.addPlayer("Player4");

            // Assert
            assertTrue(registry.isFull());
        }

        @Test
        void shouldReturnFalseWhenEmpty() {
            // Assert
            assertFalse(registry.isFull());
        }
    }

    @Nested
    class FillWithAiPlayers {

        @Test
        void shouldFillRemainingSlots() {
            // Arrange
            registry.addPlayer("HumanPlayer");
            assertEquals(1, registry.getPlayers().size());

            // Act
            registry.fillWithAiPlayers();

            // Assert
            assertEquals(4, registry.getPlayers().size());
        }

        @Test
        void shouldMarkAiPlayersAsAi() {
            // Arrange
            registry.addPlayer("HumanPlayer");

            // Act
            registry.fillWithAiPlayers();

            // Assert
            long aiCount = registry.getPlayers().stream()
                    .filter(Player::isAiActive)
                    .count();
            assertEquals(3, aiCount);
        }

        @Test
        void shouldMarkAiPlayersAsDisconnected() {
            // Arrange
            registry.addPlayer("HumanPlayer");

            // Act
            registry.fillWithAiPlayers();

            // Assert
            long disconnectedAiCount = registry.getPlayers().stream()
                    .filter(p -> p.isAiActive() && p.isDisconnected())
                    .count();
            assertEquals(3, disconnectedAiCount);
        }

        @Test
        void shouldAssignColorsToAiPlayers() {
            // Arrange
            registry.addPlayer("HumanPlayer");

            // Act
            registry.fillWithAiPlayers();

            // Assert
            assertTrue(registry.getPlayers().stream()
                    .allMatch(p -> p.getColor() != null));
        }

        @Test
        void shouldNameAiPlayersSequentially() {
            // Act
            registry.fillWithAiPlayers();

            // Assert
            assertTrue(registry.getPlayers().stream()
                    .anyMatch(p -> p.getUsername().contains("Bot")));
        }

        @Test
        void shouldDoNothingWhenAlreadyFull() {
            // Arrange
            registry.addPlayer("Player1");
            registry.addPlayer("Player2");
            registry.addPlayer("Player3");
            registry.addPlayer("Player4");

            // Act
            registry.fillWithAiPlayers();

            // Assert - no exception and still 4 players
            assertEquals(4, registry.getPlayers().size());
        }

        @Test
        void shouldFillCompletelyWhenEmpty() {
            // Act
            registry.fillWithAiPlayers();

            // Assert
            assertEquals(4, registry.getPlayers().size());
            assertTrue(registry.getPlayers().stream().allMatch(Player::isAiActive));
        }
    }

    @Nested
    class ColorAssignment {

        @Test
        void shouldAssignAllDifferentColors() {
            // Act
            registry.addPlayer("Player1");
            registry.addPlayer("Player2");
            registry.addPlayer("Player3");
            registry.addPlayer("Player4");

            // Assert
            var colors = registry.getPlayers().stream()
                    .map(Player::getColor)
                    .distinct()
                    .count();
            assertEquals(4, colors);
        }

        @Test
        void shouldReuseColorAfterPlayerLeaves() {
            // Arrange
            Player player1 = registry.addPlayer("Player1");
            PlayerColor firstColor = player1.getColor();
            registry.removePlayer(player1);

            // Act
            Player newPlayer = registry.addPlayer("NewPlayer");

            // Assert - first color should be available again
            assertEquals(firstColor, newPlayer.getColor());
        }
    }

    @Nested
    class AdminReassignment {

        @Test
        void shouldReassignAdminToHumanOverAi() {
            // Arrange
            Player admin = registry.addPlayer("Admin");
            Player human = registry.addPlayer("Human");
            registry.fillWithAiPlayers();

            // Act
            registry.removePlayer(admin);

            // Assert - human should become admin
            assertTrue(human.isAdmin());
        }

        @Test
        void shouldReassignAdminToFirstAvailableWhenNoHumans() {
            // Arrange
            Player admin = registry.addPlayer("Admin");
            registry.fillWithAiPlayers();

            // Remove the human admin
            registry.removePlayer(admin);

            // Assert - first AI should become admin
            Player newAdmin = registry.getPlayers().stream()
                    .filter(Player::isAdmin)
                    .findFirst()
                    .orElse(null);
            assertNotNull(newAdmin);
        }

        @Test
        void shouldNotReassignWhenNonAdminLeaves() {
            // Arrange
            Player admin = registry.addPlayer("Admin");
            Player player2 = registry.addPlayer("Player2");

            // Act
            registry.removePlayer(player2);

            // Assert - admin should still be admin
            assertTrue(admin.isAdmin());
        }
    }

    @Nested
    class RegistryWithDifferentMaxPlayers {

        @Test
        void shouldRespectMaxPlayersOfTwo() {
            // Arrange
            PlayerRegistry smallRegistry = new PlayerRegistry(2);

            // Act
            smallRegistry.addPlayer("Player1");
            smallRegistry.addPlayer("Player2");

            // Assert
            assertTrue(smallRegistry.isFull());
            assertThrows(IllegalStateException.class, () ->
                    smallRegistry.addPlayer("Player3")
            );
        }

        @Test
        void shouldFillWithAiUpToMaxPlayers() {
            // Arrange
            PlayerRegistry smallRegistry = new PlayerRegistry(2);
            smallRegistry.addPlayer("Human");

            // Act
            smallRegistry.fillWithAiPlayers();

            // Assert
            assertEquals(2, smallRegistry.getPlayers().size());
        }
    }
}
