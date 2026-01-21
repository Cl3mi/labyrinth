package labyrinth.server.game.models.records;

import labyrinth.server.game.enums.Direction;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the LastShift record.
 * Validates the reverse shift detection logic.
 */
class LastShiftTest {

    @Nested
    class IsReversedBy {

        @Test
        void shouldDetectUpReversedByDown() {
            // Arrange
            LastShift lastShift = new LastShift(1, Direction.UP);

            // Act & Assert
            assertTrue(lastShift.isReversedBy(1, Direction.DOWN));
        }

        @Test
        void shouldDetectDownReversedByUp() {
            // Arrange
            LastShift lastShift = new LastShift(1, Direction.DOWN);

            // Act & Assert
            assertTrue(lastShift.isReversedBy(1, Direction.UP));
        }

        @Test
        void shouldDetectLeftReversedByRight() {
            // Arrange
            LastShift lastShift = new LastShift(2, Direction.LEFT);

            // Act & Assert
            assertTrue(lastShift.isReversedBy(2, Direction.RIGHT));
        }

        @Test
        void shouldDetectRightReversedByLeft() {
            // Arrange
            LastShift lastShift = new LastShift(2, Direction.RIGHT);

            // Act & Assert
            assertTrue(lastShift.isReversedBy(2, Direction.LEFT));
        }

        @Test
        void shouldNotBeReversedBySameDirection() {
            // Arrange
            LastShift lastShift = new LastShift(1, Direction.UP);

            // Act & Assert
            assertFalse(lastShift.isReversedBy(1, Direction.UP));
        }

        @Test
        void shouldNotBeReversedByDifferentIndex() {
            // Arrange
            LastShift lastShift = new LastShift(1, Direction.UP);

            // Act & Assert - opposite direction but different index
            assertFalse(lastShift.isReversedBy(2, Direction.DOWN));
        }

        @Test
        void shouldNotBeReversedByPerpendicularDirection() {
            // Arrange
            LastShift lastShift = new LastShift(1, Direction.UP);

            // Act & Assert
            assertFalse(lastShift.isReversedBy(1, Direction.LEFT));
            assertFalse(lastShift.isReversedBy(1, Direction.RIGHT));
        }

        @Test
        void shouldWorkWithColumnShifts() {
            // Arrange - column 3 shifted down
            LastShift lastShift = new LastShift(3, Direction.DOWN);

            // Act & Assert - same column shifted up should be a reverse
            assertTrue(lastShift.isReversedBy(3, Direction.UP));
            // Different column should not be a reverse
            assertFalse(lastShift.isReversedBy(5, Direction.UP));
        }

        @Test
        void shouldWorkWithRowShifts() {
            // Arrange - row 2 shifted right
            LastShift lastShift = new LastShift(2, Direction.RIGHT);

            // Act & Assert - same row shifted left should be a reverse
            assertTrue(lastShift.isReversedBy(2, Direction.LEFT));
            // Different row should not be a reverse
            assertFalse(lastShift.isReversedBy(4, Direction.LEFT));
        }
    }

    @Nested
    class RecordProperties {

        @Test
        void shouldStoreIndex() {
            // Arrange & Act
            LastShift lastShift = new LastShift(5, Direction.UP);

            // Assert
            assertEquals(5, lastShift.index());
        }

        @Test
        void shouldStoreDirection() {
            // Arrange & Act
            LastShift lastShift = new LastShift(3, Direction.LEFT);

            // Assert
            assertEquals(Direction.LEFT, lastShift.direction());
        }
    }
}
