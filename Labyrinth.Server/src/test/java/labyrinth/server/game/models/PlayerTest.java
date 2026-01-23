package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.time.OffsetDateTime;
import java.util.EnumSet;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the Player class.
 * Validates player state management, treasure tracking, bonus usage, and game reset.
 */
class PlayerTest {

    private Player player;
    private UUID playerId;

    @BeforeEach
    void setUp() {
        playerId = UUID.randomUUID();
        player = new Player(playerId, "TestPlayer");
    }

    @Nested
    class PlayerCreation {

        @Test
        void shouldCreatePlayerWithIdAndUsername() {
            // Assert
            assertEquals(playerId, player.getId());
            assertEquals("TestPlayer", player.getUsername());
        }

        @Test
        void shouldThrowExceptionForNullId() {
            // Act & Assert
            assertThrows(NullPointerException.class, () ->
                    new Player(null, "TestPlayer")
            );
        }

        @Test
        void shouldThrowExceptionForNullUsername() {
            // Act & Assert
            assertThrows(NullPointerException.class, () ->
                    new Player(UUID.randomUUID(), null)
            );
        }

        @Test
        void shouldHaveEmptyTreasureCardsOnCreation() {
            // Assert
            assertTrue(player.getAssignedTreasureCards().isEmpty());
        }

        @Test
        void shouldHaveEmptyBonusesOnCreation() {
            // Assert
            assertTrue(player.getBonuses().isEmpty());
        }

        @Test
        void shouldHaveNullCurrentTileOnCreation() {
            // Assert
            assertNull(player.getCurrentTile());
        }

        @Test
        void shouldHaveNullHomeTileOnCreation() {
            // Assert
            assertNull(player.getHomeTile());
        }

        @Test
        void shouldHaveZeroStatisticsOnCreation() {
            // Assert
            assertEquals(0, player.getStatistics().getScore());
            assertEquals(0, player.getStatistics().getStepsTaken());
            assertEquals(0, player.getStatistics().getTilesPushed());
            assertEquals(0, player.getStatistics().getTreasuresCollected());
        }

        @Test
        void shouldNotBeAiByDefault() {
            // Assert
            assertFalse(player.isAiActive());
        }

        @Test
        void shouldNotBeDisconnectedByDefault() {
            // Assert
            assertFalse(player.isDisconnected());
        }

        @Test
        void shouldNotBeAdminByDefault() {
            // Assert
            assertFalse(player.isAdmin());
        }
    }

    @Nested
    class TreasureCardManagement {

        @Test
        void shouldReturnNullWhenNoTreasureCardsAssigned() {
            // Act
            TreasureCard current = player.getCurrentTreasureCard();

            // Assert
            assertNull(current);
        }

        @Test
        void shouldReturnFirstUncollectedTreasureCard() {
            // Arrange
            TreasureCard card1 = new TreasureCard(1, "Gold");
            TreasureCard card2 = new TreasureCard(2, "Silver");
            player.getAssignedTreasureCards().add(card1);
            player.getAssignedTreasureCards().add(card2);

            // Act
            TreasureCard current = player.getCurrentTreasureCard();

            // Assert
            assertEquals(card1, current);
        }

        @Test
        void shouldReturnSecondCardAfterFirstIsCollected() {
            // Arrange
            TreasureCard card1 = new TreasureCard(1, "Gold");
            TreasureCard card2 = new TreasureCard(2, "Silver");
            player.getAssignedTreasureCards().add(card1);
            player.getAssignedTreasureCards().add(card2);

            // Act
            card1.collect();
            TreasureCard current = player.getCurrentTreasureCard();

            // Assert
            assertEquals(card2, current);
        }

        @Test
        void shouldReturnNullWhenAllTreasuresCollected() {
            // Arrange
            TreasureCard card1 = new TreasureCard(1, "Gold");
            TreasureCard card2 = new TreasureCard(2, "Silver");
            player.getAssignedTreasureCards().add(card1);
            player.getAssignedTreasureCards().add(card2);

            // Act
            card1.collect();
            card2.collect();
            TreasureCard current = player.getCurrentTreasureCard();

            // Assert
            assertNull(current);
        }

        @Test
        void shouldSkipCollectedCardsInTheMiddle() {
            // Arrange
            TreasureCard card1 = new TreasureCard(1, "Gold");
            TreasureCard card2 = new TreasureCard(2, "Silver");
            TreasureCard card3 = new TreasureCard(3, "Bronze");
            player.getAssignedTreasureCards().add(card1);
            player.getAssignedTreasureCards().add(card2);
            player.getAssignedTreasureCards().add(card3);

            // Act - collect first two
            card1.collect();
            card2.collect();
            TreasureCard current = player.getCurrentTreasureCard();

            // Assert
            assertEquals(card3, current);
        }
    }

    @Nested
    class BonusManagement {

        @Test
        void shouldReturnTrueWhenBonusIsUsedSuccessfully() {
            // Arrange
            player.getBonuses().add(BonusTypes.BEAM);

            // Act
            boolean result = player.useBonus(BonusTypes.BEAM);

            // Assert
            assertTrue(result);
            assertFalse(player.getBonuses().contains(BonusTypes.BEAM));
        }

        @Test
        void shouldReturnFalseWhenBonusNotAvailable() {
            // Act
            boolean result = player.useBonus(BonusTypes.BEAM);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldRemoveOnlyOneBonusOfType() {
            // Arrange - two BEAM bonuses
            player.getBonuses().add(BonusTypes.BEAM);
            player.getBonuses().add(BonusTypes.BEAM);

            // Act
            boolean result = player.useBonus(BonusTypes.BEAM);

            // Assert
            assertTrue(result);
            assertEquals(1, player.getBonuses().stream()
                    .filter(b -> b == BonusTypes.BEAM)
                    .count());
        }

        @ParameterizedTest
        @EnumSource(BonusTypes.class)
        void shouldUseDifferentBonusTypes(BonusTypes bonusType) {
            // Arrange
            player.getBonuses().add(bonusType);

            // Act
            boolean result = player.useBonus(bonusType);

            // Assert
            assertTrue(result);
            assertFalse(player.getBonuses().contains(bonusType));
        }

        @Test
        void shouldNotRemoveOtherBonusTypes() {
            // Arrange
            player.getBonuses().add(BonusTypes.BEAM);
            player.getBonuses().add(BonusTypes.SWAP);

            // Act
            player.useBonus(BonusTypes.BEAM);

            // Assert
            assertTrue(player.getBonuses().contains(BonusTypes.SWAP));
        }
    }

    @Nested
    class AiMoveBehavior {

        @Test
        void shouldNotPerformAiMoveWhenNotAiAndNotDisconnected() {
            // Arrange
            player.setAiActive(false);
            player.setDisconnected(false);

            // Act & Assert
            assertFalse(player.shouldMoveBePerformedByAi());
        }

        @Test
        void shouldNotPerformAiMoveWhenAiButNotDisconnected() {
            // Arrange
            player.setAiActive(true);
            player.setDisconnected(false);

            // Act & Assert
            assertFalse(player.shouldMoveBePerformedByAi());
        }

        @Test
        void shouldNotPerformAiMoveWhenDisconnectedButNotAi() {
            // Arrange
            player.setAiActive(false);
            player.setDisconnected(true);

            // Act & Assert
            assertFalse(player.shouldMoveBePerformedByAi());
        }

        @Test
        void shouldPerformAiMoveWhenAiAndDisconnected() {
            // Arrange
            player.setAiActive(true);
            player.setDisconnected(true);

            // Act & Assert
            assertTrue(player.shouldMoveBePerformedByAi());
        }
    }

    @Nested
    class PlayerReset {

        @BeforeEach
        void setUpPlayerState() {
            // Set up player with state
            player.getAssignedTreasureCards().add(new TreasureCard(1, "Gold"));
            player.getBonuses().add(BonusTypes.BEAM);
            player.setCurrentTile(new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            player.setHomeTile(new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            player.getStatistics().increaseScore(100);
            player.getStatistics().increaseStepsTaken(10);
            player.setAdmin(true);
            player.setColor(PlayerColor.RED);
        }

        @Test
        void shouldClearTreasureCardsOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertTrue(player.getAssignedTreasureCards().isEmpty());
        }

        @Test
        void shouldClearBonusesOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertTrue(player.getBonuses().isEmpty());
        }

        @Test
        void shouldClearCurrentTileOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertNull(player.getCurrentTile());
        }

        @Test
        void shouldClearHomeTileOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertNull(player.getHomeTile());
        }

        @Test
        void shouldResetStatisticsOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertEquals(0, player.getStatistics().getScore());
            assertEquals(0, player.getStatistics().getStepsTaken());
        }

        @Test
        void shouldPreserveIdOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertEquals(playerId, player.getId());
        }

        @Test
        void shouldPreserveUsernameOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertEquals("TestPlayer", player.getUsername());
        }

        @Test
        void shouldPreserveAdminStatusOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertTrue(player.isAdmin());
        }

        @Test
        void shouldPreserveColorOnReset() {
            // Act
            player.resetForNewGame();

            // Assert
            assertEquals(PlayerColor.RED, player.getColor());
        }
    }

    @Nested
    class PlayerCopy {

        @BeforeEach
        void setUpPlayerState() {
            player.setJoinDate(OffsetDateTime.now());
            player.setAiActive(true);
            player.setAdmin(true);
            player.setColor(PlayerColor.BLUE);
            player.getAssignedTreasureCards().add(new TreasureCard(1, "Gold"));
            player.getStatistics().increaseScore(50);
        }

        @Test
        void shouldCreateCopyWithSameId() {
            // Act
            Player copy = player.copy();

            // Assert
            assertEquals(player.getId(), copy.getId());
        }

        @Test
        void shouldCreateCopyWithSameUsername() {
            // Act
            Player copy = player.copy();

            // Assert
            assertEquals(player.getUsername(), copy.getUsername());
        }

        @Test
        void shouldCopyAiStatus() {
            // Act
            Player copy = player.copy();

            // Assert
            assertEquals(player.isAiActive(), copy.isAiActive());
        }

        @Test
        void shouldCopyAdminStatus() {
            // Act
            Player copy = player.copy();

            // Assert
            assertEquals(player.isAdmin(), copy.isAdmin());
        }

        @Test
        void shouldCopyColor() {
            // Act
            Player copy = player.copy();

            // Assert
            assertEquals(player.getColor(), copy.getColor());
        }

        @Test
        void shouldCopyTreasureCards() {
            // Act
            Player copy = player.copy();

            // Assert
            assertEquals(player.getAssignedTreasureCards().size(), copy.getAssignedTreasureCards().size());
        }

        @Test
        void shouldCreateDifferentInstance() {
            // Act
            Player copy = player.copy();

            // Assert
            assertNotSame(player, copy);
        }
    }

    @Nested
    class PlayerSetters {

        @Test
        void shouldSetCurrentTile() {
            // Arrange
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Act
            player.setCurrentTile(tile);

            // Assert
            assertEquals(tile, player.getCurrentTile());
        }

        @Test
        void shouldSetHomeTile() {
            // Arrange
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Act
            player.setHomeTile(tile);

            // Assert
            assertEquals(tile, player.getHomeTile());
        }

        @Test
        void shouldSetColor() {
            // Act
            player.setColor(PlayerColor.GREEN);

            // Assert
            assertEquals(PlayerColor.GREEN, player.getColor());
        }

        @Test
        void shouldSetJoinDate() {
            // Arrange
            OffsetDateTime now = OffsetDateTime.now();

            // Act
            player.setJoinDate(now);

            // Assert
            assertEquals(now, player.getJoinDate());
        }
    }

    @Nested
    class PlayerToString {

        @Test
        void shouldIncludeIdInToString() {
            // Act
            String str = player.toString();

            // Assert
            assertTrue(str.contains(playerId.toString()));
        }

        @Test
        void shouldIncludeUsernameInToString() {
            // Act
            String str = player.toString();

            // Assert
            assertTrue(str.contains("TestPlayer"));
        }
    }
}
