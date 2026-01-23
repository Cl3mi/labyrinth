package labyrinth.client.unit.models;

import labyrinth.client.models.Position;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.*;

@DisplayName("Position")
class PositionTest {

    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {

        @Test
        @DisplayName("constructor_validCoordinates_createsPosition")
        void constructor_validCoordinates_createsPosition() {
            // Given
            int row = 3;
            int column = 5;

            // When
            Position position = new Position(row, column);

            // Then
            assertThat(position.getRow()).isEqualTo(row);
            assertThat(position.getColumn()).isEqualTo(column);
        }

        @Test
        @DisplayName("constructor_zeroCoordinates_createsPosition")
        void constructor_zeroCoordinates_createsPosition() {
            // Given
            int row = 0;
            int column = 0;

            // When
            Position position = new Position(row, column);

            // Then
            assertThat(position.getRow()).isZero();
            assertThat(position.getColumn()).isZero();
        }

        @Test
        @DisplayName("constructor_largeCoordinates_createsPosition")
        void constructor_largeCoordinates_createsPosition() {
            // Given
            int row = Integer.MAX_VALUE;
            int column = Integer.MAX_VALUE;

            // When
            Position position = new Position(row, column);

            // Then
            assertThat(position.getRow()).isEqualTo(Integer.MAX_VALUE);
            assertThat(position.getColumn()).isEqualTo(Integer.MAX_VALUE);
        }

        @ParameterizedTest
        @ValueSource(ints = {-1, -100, Integer.MIN_VALUE})
        @DisplayName("constructor_negativeRow_throwsException")
        void constructor_negativeRow_throwsException(int negativeRow) {
            // Given
            int column = 0;

            // When/Then
            assertThatThrownBy(() -> new Position(negativeRow, column))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("non-negative");
        }

        @ParameterizedTest
        @ValueSource(ints = {-1, -100, Integer.MIN_VALUE})
        @DisplayName("constructor_negativeColumn_throwsException")
        void constructor_negativeColumn_throwsException(int negativeColumn) {
            // Given
            int row = 0;

            // When/Then
            assertThatThrownBy(() -> new Position(row, negativeColumn))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("non-negative");
        }

        @Test
        @DisplayName("constructor_bothNegative_throwsException")
        void constructor_bothNegative_throwsException() {
            // Given
            int row = -5;
            int column = -10;

            // When/Then
            assertThatThrownBy(() -> new Position(row, column))
                    .isInstanceOf(IllegalArgumentException.class);
        }
    }

    @Nested
    @DisplayName("Equals")
    class EqualsTests {

        @Test
        @DisplayName("equals_sameCoordinates_returnsTrue")
        void equals_sameCoordinates_returnsTrue() {
            // Given
            Position position1 = new Position(2, 3);
            Position position2 = new Position(2, 3);

            // When/Then
            assertThat(position1).isEqualTo(position2);
            assertThat(position2).isEqualTo(position1);
        }

        @Test
        @DisplayName("equals_sameInstance_returnsTrue")
        void equals_sameInstance_returnsTrue() {
            // Given
            Position position = new Position(2, 3);

            // When/Then
            assertThat(position).isEqualTo(position);
        }

        @ParameterizedTest
        @CsvSource({
                "0, 0, 0, 1",
                "0, 0, 1, 0",
                "1, 2, 2, 1",
                "5, 5, 5, 6"
        })
        @DisplayName("equals_differentCoordinates_returnsFalse")
        void equals_differentCoordinates_returnsFalse(int row1, int col1, int row2, int col2) {
            // Given
            Position position1 = new Position(row1, col1);
            Position position2 = new Position(row2, col2);

            // When/Then
            assertThat(position1).isNotEqualTo(position2);
        }

        @Test
        @DisplayName("equals_null_returnsFalse")
        void equals_null_returnsFalse() {
            // Given
            Position position = new Position(2, 3);

            // When/Then
            assertThat(position).isNotEqualTo(null);
        }

        @Test
        @DisplayName("equals_differentType_returnsFalse")
        void equals_differentType_returnsFalse() {
            // Given
            Position position = new Position(2, 3);
            String other = "2,3";

            // When/Then
            assertThat(position).isNotEqualTo(other);
        }
    }

    @Nested
    @DisplayName("HashCode")
    class HashCodeTests {

        @Test
        @DisplayName("hashCode_sameCoordinates_sameHash")
        void hashCode_sameCoordinates_sameHash() {
            // Given
            Position position1 = new Position(2, 3);
            Position position2 = new Position(2, 3);

            // When/Then
            assertThat(position1.hashCode()).isEqualTo(position2.hashCode());
        }

        @Test
        @DisplayName("hashCode_differentCoordinates_differentHash")
        void hashCode_differentCoordinates_differentHash() {
            // Given
            Position position1 = new Position(2, 3);
            Position position2 = new Position(3, 2);

            // When/Then
            // Note: While not strictly required, different positions should ideally have different hash codes
            assertThat(position1.hashCode()).isNotEqualTo(position2.hashCode());
        }

        @Test
        @DisplayName("hashCode_consistent_returnsSameValue")
        void hashCode_consistent_returnsSameValue() {
            // Given
            Position position = new Position(5, 7);

            // When
            int hash1 = position.hashCode();
            int hash2 = position.hashCode();

            // Then
            assertThat(hash1).isEqualTo(hash2);
        }
    }

    @Nested
    @DisplayName("ToString")
    class ToStringTests {

        @Test
        @DisplayName("toString_validPosition_containsRowAndColumn")
        void toString_validPosition_containsRowAndColumn() {
            // Given
            Position position = new Position(4, 6);

            // When
            String result = position.toString();

            // Then
            assertThat(result)
                    .contains("row")
                    .contains("4")
                    .contains("column")
                    .contains("6");
        }

        @Test
        @DisplayName("toString_zeroPosition_containsZeros")
        void toString_zeroPosition_containsZeros() {
            // Given
            Position position = new Position(0, 0);

            // When
            String result = position.toString();

            // Then
            assertThat(result)
                    .contains("0");
        }
    }

    @Nested
    @DisplayName("Getters")
    class GetterTests {

        @Test
        @DisplayName("getRow_afterCreation_returnsCorrectValue")
        void getRow_afterCreation_returnsCorrectValue() {
            // Given
            Position position = new Position(7, 3);

            // When
            int row = position.getRow();

            // Then
            assertThat(row).isEqualTo(7);
        }

        @Test
        @DisplayName("getColumn_afterCreation_returnsCorrectValue")
        void getColumn_afterCreation_returnsCorrectValue() {
            // Given
            Position position = new Position(7, 3);

            // When
            int column = position.getColumn();

            // Then
            assertThat(column).isEqualTo(3);
        }
    }

    @Nested
    @DisplayName("Immutability")
    class ImmutabilityTests {

        @Test
        @DisplayName("position_afterCreation_isImmutable")
        void position_afterCreation_isImmutable() {
            // Given
            Position position = new Position(5, 5);
            int originalRow = position.getRow();
            int originalColumn = position.getColumn();

            // When - try to use the position in various operations
            Position other = new Position(position.getRow() + 1, position.getColumn() + 1);

            // Then - original position should remain unchanged
            assertThat(position.getRow()).isEqualTo(originalRow);
            assertThat(position.getColumn()).isEqualTo(originalColumn);
        }
    }
}
