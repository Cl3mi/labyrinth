package labyrinth.client.unit.ai;

import labyrinth.client.ai.AiDecision;
import labyrinth.client.ai.ShiftOperation;
import labyrinth.client.ai.SimulationResult;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.Direction;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for AiDecision record.
 */
@DisplayName("AiDecision")
class AiDecisionTest {

    @Nested
    @DisplayName("Constructors")
    class ConstructorTests {

        @Test
        @DisplayName("fullConstructor_allParams_createsDecision")
        void fullConstructor_allParams_createsDecision() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.UP);
            Position targetPos = new Position(2, 3);
            SimulationResult.BonusAction bonus = SimulationResult.BonusAction.beam(targetPos);

            // When
            AiDecision decision = new AiDecision(shift, 2, targetPos, bonus);

            // Then
            assertThat(decision.shift()).isEqualTo(shift);
            assertThat(decision.rotations()).isEqualTo(2);
            assertThat(decision.targetPosition()).isEqualTo(targetPos);
            assertThat(decision.bonusAction()).isEqualTo(bonus);
        }

        @Test
        @DisplayName("threeParamConstructor_nullBonus")
        void threeParamConstructor_nullBonus() {
            // Given
            ShiftOperation shift = ShiftOperation.column(1, Direction.LEFT);
            Position targetPos = new Position(4, 5);

            // When
            AiDecision decision = new AiDecision(shift, 1, targetPos);

            // Then
            assertThat(decision.shift()).isEqualTo(shift);
            assertThat(decision.rotations()).isEqualTo(1);
            assertThat(decision.targetPosition()).isEqualTo(targetPos);
            assertThat(decision.bonusAction()).isNull();
        }

        @Test
        @DisplayName("constructor_zeroRotations_valid")
        void constructor_zeroRotations_valid() {
            // Given
            ShiftOperation shift = ShiftOperation.column(5, Direction.DOWN);
            Position targetPos = new Position(0, 0);

            // When
            AiDecision decision = new AiDecision(shift, 0, targetPos);

            // Then
            assertThat(decision.rotations()).isZero();
        }

        @Test
        @DisplayName("constructor_nullTargetPosition_allowed")
        void constructor_nullTargetPosition_allowed() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.RIGHT);

            // When
            AiDecision decision = new AiDecision(shift, 1, null);

            // Then
            assertThat(decision.targetPosition()).isNull();
        }
    }

    @Nested
    @DisplayName("from factory method")
    class FromTests {

        @Test
        @DisplayName("from_validResult_createsDecision")
        void from_validResult_createsDecision() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.UP);
            Position targetPos = new Position(2, 3);
            SimulationResult result = new SimulationResult(shift, 2, 100, 5, targetPos);

            // When
            AiDecision decision = AiDecision.from(result);

            // Then
            assertThat(decision).isNotNull();
            assertThat(decision.shift()).isEqualTo(shift);
            assertThat(decision.rotations()).isEqualTo(2);
            assertThat(decision.targetPosition()).isEqualTo(targetPos);
            assertThat(decision.bonusAction()).isNull();
        }

        @Test
        @DisplayName("from_resultWithBonus_copiesBonus")
        void from_resultWithBonus_copiesBonus() {
            // Given
            ShiftOperation shift = ShiftOperation.column(1, Direction.LEFT);
            Position targetPos = new Position(4, 5);
            SimulationResult.BonusAction bonus = SimulationResult.BonusAction.beam(new Position(6, 6));
            SimulationResult result = new SimulationResult(shift, 3, 90, 2, targetPos, bonus);

            // When
            AiDecision decision = AiDecision.from(result);

            // Then
            assertThat(decision.bonusAction()).isEqualTo(bonus);
        }

        @Test
        @DisplayName("from_nullResult_returnsNull")
        void from_nullResult_returnsNull() {
            // When
            AiDecision decision = AiDecision.from(null);

            // Then
            assertThat(decision).isNull();
        }
    }

    @Nested
    @DisplayName("Record equality")
    class EqualityTests {

        @Test
        @DisplayName("twoDecisionsWithSameValues_areEqual")
        void twoDecisionsWithSameValues_areEqual() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.UP);
            Position pos = new Position(2, 2);

            AiDecision decision1 = new AiDecision(shift, 1, pos);
            AiDecision decision2 = new AiDecision(shift, 1, pos);

            // Then
            assertThat(decision1).isEqualTo(decision2);
            assertThat(decision1.hashCode()).isEqualTo(decision2.hashCode());
        }

        @Test
        @DisplayName("decisionsWithDifferentShifts_areNotEqual")
        void decisionsWithDifferentShifts_areNotEqual() {
            // Given
            ShiftOperation shift1 = ShiftOperation.row(3, Direction.UP);
            ShiftOperation shift2 = ShiftOperation.row(3, Direction.DOWN);
            Position pos = new Position(2, 2);

            AiDecision decision1 = new AiDecision(shift1, 1, pos);
            AiDecision decision2 = new AiDecision(shift2, 1, pos);

            // Then
            assertThat(decision1).isNotEqualTo(decision2);
        }

        @Test
        @DisplayName("decisionsWithDifferentRotations_areNotEqual")
        void decisionsWithDifferentRotations_areNotEqual() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.UP);
            Position pos = new Position(2, 2);

            AiDecision decision1 = new AiDecision(shift, 1, pos);
            AiDecision decision2 = new AiDecision(shift, 2, pos);

            // Then
            assertThat(decision1).isNotEqualTo(decision2);
        }
    }

    @Nested
    @DisplayName("ShiftOperation")
    class ShiftOperationTests {

        @Test
        @DisplayName("shiftOperation_row_createsCorrectly")
        void shiftOperation_row_createsCorrectly() {
            // When
            ShiftOperation shift = ShiftOperation.row(3, Direction.UP);

            // Then
            assertThat(shift.direction()).isEqualTo(Direction.UP);
            assertThat(shift.index()).isEqualTo(3);
            assertThat(shift.isRow()).isTrue();
        }

        @Test
        @DisplayName("shiftOperation_column_createsCorrectly")
        void shiftOperation_column_createsCorrectly() {
            // When
            ShiftOperation shift = ShiftOperation.column(5, Direction.LEFT);

            // Then
            assertThat(shift.direction()).isEqualTo(Direction.LEFT);
            assertThat(shift.index()).isEqualTo(5);
            assertThat(shift.isRow()).isFalse();
        }

        @Test
        @DisplayName("shiftOperation_allDirections_valid")
        void shiftOperation_allDirections_valid() {
            // Test all directions with row
            assertThat(ShiftOperation.row(1, Direction.UP).direction()).isEqualTo(Direction.UP);
            assertThat(ShiftOperation.row(1, Direction.DOWN).direction()).isEqualTo(Direction.DOWN);
            // Test all directions with column
            assertThat(ShiftOperation.column(1, Direction.LEFT).direction()).isEqualTo(Direction.LEFT);
            assertThat(ShiftOperation.column(1, Direction.RIGHT).direction()).isEqualTo(Direction.RIGHT);
        }

        @Test
        @DisplayName("shiftOperation_equalityWorks")
        void shiftOperation_equalityWorks() {
            // Given
            ShiftOperation shift1 = ShiftOperation.row(3, Direction.UP);
            ShiftOperation shift2 = ShiftOperation.row(3, Direction.UP);
            ShiftOperation shift3 = ShiftOperation.row(3, Direction.DOWN);
            ShiftOperation shift4 = ShiftOperation.column(3, Direction.UP);

            // Then
            assertThat(shift1).isEqualTo(shift2);
            assertThat(shift1).isNotEqualTo(shift3); // Different direction
            assertThat(shift1).isNotEqualTo(shift4); // Different isRow
        }
    }
}
