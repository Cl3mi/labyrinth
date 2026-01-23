package labyrinth.server.game.services;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.results.TileInteractionResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the MovementManager class.
 * Validates tile interactions, treasure collection, bonus pickup, and player blocking.
 */
class MovementManagerTest {

    private MovementManager movementManager;
    private Player player;
    private Tile tile;

    @BeforeEach
    void setUp() {
        movementManager = new MovementManager();
        player = new Player(UUID.randomUUID(), "TestPlayer");
        tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
    }

    @Nested
    class CheckTileForCollectibles {

        @Test
        void shouldReturnEmptyResultForEmptyTile() {
            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert
            assertNull(result.collectedTreasure());
            assertNull(result.collectedBonus());
            assertFalse(result.wasCurrentTarget());
        }

        @Test
        void shouldReturnTreasureWhenIsCurrentTarget() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);

            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert
            assertEquals(treasure, result.collectedTreasure());
            assertTrue(result.wasCurrentTarget());
        }

        @Test
        void shouldNotReturnTreasureWhenNotCurrentTarget() {
            // Arrange
            TreasureCard playerTreasure = new TreasureCard(1, "Gold");
            TreasureCard tileTreasure = new TreasureCard(2, "Silver");
            player.getAssignedTreasureCards().add(playerTreasure);
            tile.setTreasureCard(tileTreasure);

            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert
            assertNull(result.collectedTreasure());
            assertFalse(result.wasCurrentTarget());
        }

        @Test
        void shouldReturnBonusWhenPresent() {
            // Arrange
            tile.setBonus(BonusTypes.BEAM);

            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert
            assertEquals(BonusTypes.BEAM, result.collectedBonus());
        }

        @ParameterizedTest
        @EnumSource(BonusTypes.class)
        void shouldReturnAnyBonusType(BonusTypes bonusType) {
            // Arrange
            tile.setBonus(bonusType);

            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert
            assertEquals(bonusType, result.collectedBonus());
        }

        @Test
        void shouldReturnBothTreasureAndBonus() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);
            tile.setBonus(BonusTypes.SWAP);

            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert
            assertEquals(treasure, result.collectedTreasure());
            assertEquals(BonusTypes.SWAP, result.collectedBonus());
            assertTrue(result.wasCurrentTarget());
        }

        @Test
        void shouldNotMutateTileOrPlayer() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);
            tile.setBonus(BonusTypes.BEAM);

            // Act
            movementManager.checkTileForCollectibles(player, tile);

            // Assert - tile and player should be unchanged
            assertNotNull(tile.getTreasureCard());
            assertNotNull(tile.getBonus());
            assertFalse(treasure.isCollected());
            assertTrue(player.getBonuses().isEmpty());
        }
    }

    @Nested
    class ProcessPlayerStepOnTile {

        @Test
        void shouldMovePlayerToTile() {
            // Arrange
            Tile startTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            player.setCurrentTile(startTile);

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertEquals(tile, player.getCurrentTile());
        }

        @Test
        void shouldCollectTreasureWhenIsCurrentTarget() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertTrue(treasure.isCollected());
            assertNull(tile.getTreasureCard());
        }

        @Test
        void shouldIncreaseScoreWhenCollectingTreasure() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);
            int initialScore = player.getStatistics().getScore();

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertTrue(player.getStatistics().getScore() > initialScore);
        }

        @Test
        void shouldIncreaseTreasuresCollectedStat() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertEquals(1, player.getStatistics().getTreasuresCollected());
        }

        @Test
        void shouldNotCollectTreasureWhenNotCurrentTarget() {
            // Arrange
            TreasureCard playerTreasure = new TreasureCard(1, "Gold");
            TreasureCard tileTreasure = new TreasureCard(2, "Silver");
            player.getAssignedTreasureCards().add(playerTreasure);
            tile.setTreasureCard(tileTreasure);

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertFalse(tileTreasure.isCollected());
            assertEquals(tileTreasure, tile.getTreasureCard()); // Still on tile
        }

        @Test
        void shouldCollectBonus() {
            // Arrange
            tile.setBonus(BonusTypes.BEAM);
            assertTrue(player.getBonuses().isEmpty());

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertTrue(player.getBonuses().contains(BonusTypes.BEAM));
            assertNull(tile.getBonus());
        }

        @ParameterizedTest
        @EnumSource(BonusTypes.class)
        void shouldCollectAnyBonusType(BonusTypes bonusType) {
            // Arrange
            tile.setBonus(bonusType);

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertTrue(player.getBonuses().contains(bonusType));
            assertNull(tile.getBonus());
        }

        @Test
        void shouldCollectBothTreasureAndBonus() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);
            tile.setBonus(BonusTypes.SWAP);

            // Act
            movementManager.processPlayerStepOnTile(player, tile);

            // Assert
            assertTrue(treasure.isCollected());
            assertTrue(player.getBonuses().contains(BonusTypes.SWAP));
            assertNull(tile.getTreasureCard());
            assertNull(tile.getBonus());
        }

        @Test
        void shouldHandleEmptyTile() {
            // Arrange - no treasure or bonus

            // Act & Assert - should not throw
            assertDoesNotThrow(() ->
                    movementManager.processPlayerStepOnTile(player, tile)
            );
            assertEquals(tile, player.getCurrentTile());
        }
    }

    @Nested
    class CanPlayerReachTile {

        @Test
        void shouldReturnTrueWhenTileIsReachable() {
            // Arrange
            Set<Tile> reachableTiles = new HashSet<>();
            reachableTiles.add(tile);

            // Act
            boolean canReach = movementManager.canPlayerReachTile(player, tile, reachableTiles);

            // Assert
            assertTrue(canReach);
        }

        @Test
        void shouldReturnFalseWhenTileIsNotReachable() {
            // Arrange
            Set<Tile> reachableTiles = new HashSet<>();
            // tile is not in the set

            // Act
            boolean canReach = movementManager.canPlayerReachTile(player, tile, reachableTiles);

            // Assert
            assertFalse(canReach);
        }

        @Test
        void shouldReturnFalseForEmptyReachableSet() {
            // Arrange
            Set<Tile> reachableTiles = new HashSet<>();

            // Act
            boolean canReach = movementManager.canPlayerReachTile(player, tile, reachableTiles);

            // Assert
            assertFalse(canReach);
        }
    }

    @Nested
    class IsTileBlockedByPlayer {

        @Test
        void shouldReturnTrueWhenOtherPlayerOnTile() {
            // Arrange
            Player otherPlayer = new Player(UUID.randomUUID(), "OtherPlayer");
            otherPlayer.setCurrentTile(tile);
            List<Player> players = List.of(player, otherPlayer);

            // Act
            boolean isBlocked = movementManager.isTileBlockedByPlayer(tile, players, player);

            // Assert
            assertTrue(isBlocked);
        }

        @Test
        void shouldReturnFalseWhenTileIsEmpty() {
            // Arrange
            Player otherPlayer = new Player(UUID.randomUUID(), "OtherPlayer");
            Tile otherTile = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));
            otherPlayer.setCurrentTile(otherTile);
            List<Player> players = List.of(player, otherPlayer);

            // Act
            boolean isBlocked = movementManager.isTileBlockedByPlayer(tile, players, player);

            // Assert
            assertFalse(isBlocked);
        }

        @Test
        void shouldExcludeMovingPlayerFromCheck() {
            // Arrange
            player.setCurrentTile(tile); // The moving player is already on the tile
            List<Player> players = List.of(player);

            // Act
            boolean isBlocked = movementManager.isTileBlockedByPlayer(tile, players, player);

            // Assert - should not be blocked by the moving player themselves
            assertFalse(isBlocked);
        }

        @Test
        void shouldReturnTrueWithMultiplePlayersWhenOneBlocks() {
            // Arrange
            Player player2 = new Player(UUID.randomUUID(), "Player2");
            Player player3 = new Player(UUID.randomUUID(), "Player3");
            player2.setCurrentTile(new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            player3.setCurrentTile(tile); // This player blocks the tile
            List<Player> players = List.of(player, player2, player3);

            // Act
            boolean isBlocked = movementManager.isTileBlockedByPlayer(tile, players, player);

            // Assert
            assertTrue(isBlocked);
        }

        @Test
        void shouldReturnFalseWhenNoOtherPlayers() {
            // Arrange
            List<Player> players = List.of(player);

            // Act
            boolean isBlocked = movementManager.isTileBlockedByPlayer(tile, players, player);

            // Assert
            assertFalse(isBlocked);
        }
    }

    @Nested
    class TreasureCollectionEdgeCases {

        @Test
        void shouldNotCollectAlreadyCollectedTreasure() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold");
            treasure.collect(); // Already collected
            player.getAssignedTreasureCards().add(treasure);
            tile.setTreasureCard(treasure);
            int initialTreasuresCollected = player.getStatistics().getTreasuresCollected();

            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert - should not identify as current target since it's already collected
            assertNull(result.collectedTreasure());
        }

        @Test
        void shouldCollectSecondTreasureAfterFirstIsCollected() {
            // Arrange
            TreasureCard treasure1 = new TreasureCard(1, "Gold");
            TreasureCard treasure2 = new TreasureCard(2, "Silver");
            treasure1.collect(); // First one already collected
            player.getAssignedTreasureCards().add(treasure1);
            player.getAssignedTreasureCards().add(treasure2);
            tile.setTreasureCard(treasure2);

            // Act
            TileInteractionResult result = movementManager.checkTileForCollectibles(player, tile);

            // Assert - should identify second treasure as current target
            assertEquals(treasure2, result.collectedTreasure());
        }
    }

    @Nested
    class MultipleBonusCollection {

        @Test
        void shouldCollectMultipleBonusesOverTime() {
            // Arrange
            Tile tile1 = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            Tile tile2 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));
            tile1.setBonus(BonusTypes.BEAM);
            tile2.setBonus(BonusTypes.SWAP);

            // Act
            movementManager.processPlayerStepOnTile(player, tile1);
            movementManager.processPlayerStepOnTile(player, tile2);

            // Assert
            assertEquals(2, player.getBonuses().size());
            assertTrue(player.getBonuses().contains(BonusTypes.BEAM));
            assertTrue(player.getBonuses().contains(BonusTypes.SWAP));
        }

        @Test
        void shouldCollectDuplicateBonusTypes() {
            // Arrange
            Tile tile1 = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            Tile tile2 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));
            tile1.setBonus(BonusTypes.BEAM);
            tile2.setBonus(BonusTypes.BEAM);

            // Act
            movementManager.processPlayerStepOnTile(player, tile1);
            movementManager.processPlayerStepOnTile(player, tile2);

            // Assert
            assertEquals(2, player.getBonuses().stream()
                    .filter(b -> b == BonusTypes.BEAM)
                    .count());
        }
    }
}
