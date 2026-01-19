package labyrinth.server.game.models;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.EnumSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the Tile class.
 * Validates tile creation, rotation, connectivity, and treasure/bonus management.
 */
class TileTest {

    private Tile tile;

    @Nested
    class TileCreation {

        @Test
        void shouldCreateTileWithTwoEntrances() {
            // Arrange & Act
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Assert
            assertEquals(2, tile.getEntrances().size());
            assertTrue(tile.getEntrances().contains(Direction.UP));
            assertTrue(tile.getEntrances().contains(Direction.DOWN));
        }

        @Test
        void shouldCreateTileWithThreeEntrances() {
            // Arrange & Act
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN, Direction.LEFT));

            // Assert
            assertEquals(3, tile.getEntrances().size());
            assertTrue(tile.getEntrances().containsAll(Set.of(Direction.UP, Direction.DOWN, Direction.LEFT)));
        }

        @Test
        void shouldCreateTileWithFourEntrances() {
            // Arrange & Act
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT));

            // Assert
            assertEquals(4, tile.getEntrances().size());
        }

        @Test
        void shouldThrowExceptionForLessThanTwoEntrances() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Tile(EnumSet.of(Direction.UP))
            );
        }

        @Test
        void shouldThrowExceptionForEmptyEntrances() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Tile(EnumSet.noneOf(Direction.class))
            );
        }

        @Test
        void shouldNotBeFixedByDefault() {
            // Arrange & Act
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Assert
            assertFalse(tile.isFixed());
        }

        @Test
        void shouldNotHaveTreasureByDefault() {
            // Arrange & Act
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Assert
            assertNull(tile.getTreasureCard());
        }

        @Test
        void shouldNotHaveBonusByDefault() {
            // Arrange & Act
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Assert
            assertNull(tile.getBonus());
        }
    }

    @Nested
    class TileRotation {

        @BeforeEach
        void setUp() {
            // Create a simple corner tile (UP, RIGHT)
            tile = new Tile(EnumSet.of(Direction.UP, Direction.RIGHT));
        }

        @Test
        void shouldRotateClockwiseOnce() {
            // Act
            tile.rotate();

            // Assert - UP -> RIGHT, RIGHT -> DOWN
            assertTrue(tile.getEntrances().contains(Direction.RIGHT));
            assertTrue(tile.getEntrances().contains(Direction.DOWN));
            assertFalse(tile.getEntrances().contains(Direction.UP));
            assertFalse(tile.getEntrances().contains(Direction.LEFT));
        }

        @Test
        void shouldRotateClockwiseTwice() {
            // Act
            tile.rotate();
            tile.rotate();

            // Assert - UP -> DOWN, RIGHT -> LEFT
            assertTrue(tile.getEntrances().contains(Direction.DOWN));
            assertTrue(tile.getEntrances().contains(Direction.LEFT));
        }

        @Test
        void shouldRotateClockwiseThreeTimes() {
            // Act
            tile.rotate();
            tile.rotate();
            tile.rotate();

            // Assert - UP -> LEFT, RIGHT -> UP
            assertTrue(tile.getEntrances().contains(Direction.LEFT));
            assertTrue(tile.getEntrances().contains(Direction.UP));
        }

        @Test
        void shouldReturnToOriginalAfterFourRotations() {
            // Arrange
            Set<Direction> originalEntrances = EnumSet.copyOf(tile.getEntrances());

            // Act
            tile.rotate();
            tile.rotate();
            tile.rotate();
            tile.rotate();

            // Assert
            assertEquals(originalEntrances, tile.getEntrances());
        }

        @Test
        void shouldRotateStraightTileCorrectly() {
            // Arrange - vertical straight tile
            Tile straightTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Act
            straightTile.rotate();

            // Assert - becomes horizontal
            assertTrue(straightTile.getEntrances().contains(Direction.LEFT));
            assertTrue(straightTile.getEntrances().contains(Direction.RIGHT));
            assertFalse(straightTile.getEntrances().contains(Direction.UP));
            assertFalse(straightTile.getEntrances().contains(Direction.DOWN));
        }

        @Test
        void shouldRotateTJunctionCorrectly() {
            // Arrange - T with opening facing down (UP, LEFT, RIGHT)
            Tile tTile = new Tile(EnumSet.of(Direction.UP, Direction.LEFT, Direction.RIGHT));

            // Act
            tTile.rotate();

            // Assert - (RIGHT, UP, DOWN)
            assertTrue(tTile.getEntrances().contains(Direction.RIGHT));
            assertTrue(tTile.getEntrances().contains(Direction.UP));
            assertTrue(tTile.getEntrances().contains(Direction.DOWN));
            assertFalse(tTile.getEntrances().contains(Direction.LEFT));
        }
    }

    @Nested
    class TileConnectivity {

        @Test
        void shouldBeConnectedWhenEntrancesMatch() {
            // Arrange
            Tile tile1 = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            Tile tile2 = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Act & Assert - tile1 is above tile2 (tile1 has DOWN, tile2 has UP = connected)
            assertTrue(tile1.isConnectedTo(tile2, Direction.DOWN));
        }

        @Test
        void shouldNotBeConnectedWhenEntrancesDontMatch() {
            // Arrange
            Tile tile1 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)); // horizontal
            Tile tile2 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)); // horizontal

            // Act & Assert - tile1 above tile2, but neither has UP/DOWN
            assertFalse(tile1.isConnectedTo(tile2, Direction.DOWN));
        }

        @Test
        void shouldNotBeConnectedWhenOnlyOneHasEntrance() {
            // Arrange
            Tile tile1 = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));  // has DOWN
            Tile tile2 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)); // no UP

            // Act & Assert
            assertFalse(tile1.isConnectedTo(tile2, Direction.DOWN));
        }

        @Test
        void shouldBeConnectedHorizontally() {
            // Arrange
            Tile tile1 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));
            Tile tile2 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT));

            // Act & Assert - tile1 to the left of tile2
            assertTrue(tile1.isConnectedTo(tile2, Direction.RIGHT));
        }

        @ParameterizedTest
        @EnumSource(Direction.class)
        void shouldCheckConnectivityInAllDirections(Direction direction) {
            // Arrange - create tiles with matching entrances
            Tile tile1 = new Tile(EnumSet.of(direction, direction.opposite()));
            Tile tile2 = new Tile(EnumSet.of(direction, direction.opposite()));

            // Act & Assert
            assertTrue(tile1.isConnectedTo(tile2, direction),
                    "Should be connected in direction " + direction);
        }
    }

    @Nested
    class TileTreasureAndBonus {

        @BeforeEach
        void setUp() {
            tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
        }

        @Test
        void shouldSetAndGetTreasureCard() {
            // Arrange
            TreasureCard treasure = new TreasureCard(1, "Gold", "gold.png");

            // Act
            tile.setTreasureCard(treasure);

            // Assert
            assertEquals(treasure, tile.getTreasureCard());
        }

        @Test
        void shouldSetAndGetBonus() {
            // Act
            tile.setBonus(BonusTypes.BEAM);

            // Assert
            assertEquals(BonusTypes.BEAM, tile.getBonus());
        }

        @Test
        void shouldAllowClearingTreasureCard() {
            // Arrange
            tile.setTreasureCard(new TreasureCard(1, "Gold", "gold.png"));

            // Act
            tile.setTreasureCard(null);

            // Assert
            assertNull(tile.getTreasureCard());
        }

        @Test
        void shouldAllowClearingBonus() {
            // Arrange
            tile.setBonus(BonusTypes.SWAP);

            // Act
            tile.setBonus(null);

            // Assert
            assertNull(tile.getBonus());
        }
    }

    @Nested
    class TileFixedState {

        @BeforeEach
        void setUp() {
            tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
        }

        @Test
        void shouldSetFixedToTrue() {
            // Act
            tile.setIsFixed(true);

            // Assert
            assertTrue(tile.isFixed());
        }

        @Test
        void shouldSetFixedToFalse() {
            // Arrange
            tile.setIsFixed(true);

            // Act
            tile.setIsFixed(false);

            // Assert
            assertFalse(tile.isFixed());
        }
    }

    @Nested
    class TileCopy {

        @Test
        void shouldCreateIndependentCopy() {
            // Arrange
            Tile original = new Tile(EnumSet.of(Direction.UP, Direction.RIGHT));
            original.setIsFixed(true);
            original.setTreasureCard(new TreasureCard(1, "Gold", "gold.png"));

            // Act
            Tile copy = original.copy();

            // Assert
            assertNotSame(original, copy);
            assertEquals(original.getEntrances(), copy.getEntrances());
            assertEquals(original.isFixed(), copy.isFixed());
            // Treasure card is reference copy (same instance)
            assertSame(original.getTreasureCard(), copy.getTreasureCard());
        }

        @Test
        void shouldNotAffectOriginalWhenCopyIsRotated() {
            // Arrange
            Tile original = new Tile(EnumSet.of(Direction.UP, Direction.RIGHT));
            Set<Direction> originalEntrances = EnumSet.copyOf(original.getEntrances());

            // Act
            Tile copy = original.copy();
            copy.rotate();

            // Assert
            assertEquals(originalEntrances, original.getEntrances());
            assertNotEquals(original.getEntrances(), copy.getEntrances());
        }
    }

    @Nested
    class TileToString {

        @Test
        void shouldIncludeEntrancesInToString() {
            // Arrange
            Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Act
            String str = tile.toString();

            // Assert
            assertNotNull(str);
            assertTrue(str.contains("Tile"));
            assertTrue(str.contains("entrances"));
        }
    }
}
