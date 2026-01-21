package labyrinth.server.game.services;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.exceptions.UsernameTakenException;
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
        void shouldAddPlayerSuccessfully() throws UsernameTakenException {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player);
            assertEquals("TestPlayer", player.getUsername());
            assertEquals(1, registry.getPlayers().size());
        }

        @Test
        void shouldAssignUniqueId() throws UsernameTakenException {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player.getId());
        }

        @Test
        void shouldAssignColor()  throws UsernameTakenException {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player.getColor());
        }

        @Test
        void shouldAssignDifferentColorsToPlayers()  throws UsernameTakenException {
            // Act
            Player player1 = registry.addPlayer("Player1");
            Player player2 = registry.addPlayer("Player2");

            // Assert
            assertNotEquals(player1.getColor(), player2.getColor());
        }

        @Test
        void shouldMakeFirstPlayerAdmin()  throws UsernameTakenException {
            // Act
            Player player = registry.addPlayer("FirstPlayer");

            // Assert
            assertTrue(player.isAdmin());
        }

        @Test
        void shouldNotMakeSubsequentPlayersAdmin() throws UsernameTakenException  {
            // Arrange
            registry.addPlayer("FirstPlayer");

            // Act
            Player secondPlayer = registry.addPlayer("SecondPlayer");

            // Assert
            assertFalse(secondPlayer.isAdmin());
        }

        @Test
        void shouldSetJoinDate()  throws UsernameTakenException {
            // Act
            Player player = registry.addPlayer("TestPlayer");

            // Assert
            assertNotNull(player.getJoinDate());
        }

        @Test
        void shouldThrowExceptionWhenRoomIsFull()  throws UsernameTakenException {
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
        void shouldThrowExceptionForDuplicateUsername() throws UsernameTakenException  {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act & Assert
            assertThrows(UsernameTakenException.class, () ->
                    registry.addPlayer("TestPlayer")
            );
        }

        @Test
        void shouldThrowExceptionForDuplicateUsernameCaseInsensitive() throws UsernameTakenException  {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act & Assert
            assertThrows(UsernameTakenException.class, () ->
                    registry.addPlayer("testplayer")
            );
        }
    }

    @Nested
    class RemovePlayer {

        @Test
        void shouldRemovePlayer() throws UsernameTakenException  {
            // Arrange
            Player player = registry.addPlayer("TestPlayer");

            // Act
            registry.removePlayer(player);

            // Assert
            assertEquals(0, registry.getPlayers().size());
        }

        @Test
        void shouldDoNothingWhenRemovingNonExistentPlayer()  throws UsernameTakenException {
            // Arrange
            registry.addPlayer("TestPlayer");
            Player nonExistent = new Player(UUID.randomUUID(), "NonExistent");

            // Act
            registry.removePlayer(nonExistent);

            // Assert
            assertEquals(1, registry.getPlayers().size());
        }

        @Test
        void shouldReassignAdminWhenAdminLeaves() throws UsernameTakenException  {
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

    }

    @Nested
    class GetPlayer {

        @Test
        void shouldGetPlayerById() throws UsernameTakenException  {
            // Arrange
            Player player = registry.addPlayer("TestPlayer");
            UUID playerId = player.getId();

            // Act
            Player found = registry.getPlayer(playerId);

            // Assert
            assertEquals(player, found);
        }

        @Test
        void shouldReturnNullForUnknownId() throws UsernameTakenException  {
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
        void shouldReturnImmutableList() throws UsernameTakenException  {
            // Arrange
            registry.addPlayer("TestPlayer");

            // Act & Assert
            assertThrows(UnsupportedOperationException.class, () ->
                    registry.getPlayers().add(new Player(UUID.randomUUID(), "Hacker"))
            );
        }

        @Test
        void shouldReturnAllPlayers() throws UsernameTakenException  {
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
        void shouldReturnMutableList() throws UsernameTakenException  {
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
        void shouldReturnFalseWhenNotFull() throws UsernameTakenException  {
            // Arrange
            registry.addPlayer("Player1");

            // Assert
            assertFalse(registry.isFull());
        }

        @Test
        void shouldReturnTrueWhenFull()  throws UsernameTakenException {
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
    class ColorAssignment {

        @Test
        void shouldAssignAllDifferentColors() throws UsernameTakenException  {
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
        void shouldReuseColorAfterPlayerLeaves() throws UsernameTakenException  {
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
        void shouldReassignAdminToNextPlayer() throws UsernameTakenException  {
            // Arrange
            Player admin = registry.addPlayer("Admin");
            Player human = registry.addPlayer("Human");

            // Act
            registry.removePlayer(admin);

            // Assert - human should become admin
            assertTrue(human.isAdmin());
        }

        @Test
        void shouldNotReassignWhenNonAdminLeaves() throws UsernameTakenException  {
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
        void shouldRespectMaxPlayersOfTwo() throws UsernameTakenException  {
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

    }
}
