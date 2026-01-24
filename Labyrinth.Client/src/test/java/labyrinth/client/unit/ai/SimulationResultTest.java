package labyrinth.client.unit.ai;

import labyrinth.client.ai.ShiftOperation;
import labyrinth.client.ai.SimulationResult;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for SimulationResult and SimulationResult.BonusAction.
 */
@DisplayName("SimulationResult")
class SimulationResultTest {

    @Nested
    @DisplayName("SimulationResult constructors")
    class ConstructorTests {

        @Test
        @DisplayName("fullConstructor_allParams_createsResult")
        void fullConstructor_allParams_createsResult() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.UP);
            Position targetPos = new Position(2, 3);
            SimulationResult.BonusAction bonus = SimulationResult.BonusAction.beam(targetPos);

            // When
            SimulationResult result = new SimulationResult(shift, 2, 100, 5, targetPos, 3, bonus);

            // Then
            assertThat(result.shift()).isEqualTo(shift);
            assertThat(result.rotations()).isEqualTo(2);
            assertThat(result.score()).isEqualTo(100);
            assertThat(result.distanceToTarget()).isEqualTo(5);
            assertThat(result.targetMovePosition()).isEqualTo(targetPos);
            assertThat(result.stepsToMove()).isEqualTo(3);
            assertThat(result.bonusAction()).isEqualTo(bonus);
        }

        @Test
        @DisplayName("fiveParamConstructor_defaultsStepsAndBonus")
        void fiveParamConstructor_defaultsStepsAndBonus() {
            // Given
            ShiftOperation shift = ShiftOperation.column(1, Direction.LEFT);
            Position targetPos = new Position(4, 5);

            // When
            SimulationResult result = new SimulationResult(shift, 1, 50, 3, targetPos);

            // Then
            assertThat(result.stepsToMove()).isEqualTo(0);
            assertThat(result.bonusAction()).isNull();
        }

        @Test
        @DisplayName("sixParamConstructor_defaultsBonus")
        void sixParamConstructor_defaultsBonus() {
            // Given
            ShiftOperation shift = ShiftOperation.column(5, Direction.RIGHT);
            Position targetPos = new Position(1, 1);

            // When
            SimulationResult result = new SimulationResult(shift, 0, 75, 2, targetPos, 4);

            // Then
            assertThat(result.stepsToMove()).isEqualTo(4);
            assertThat(result.bonusAction()).isNull();
        }

        @Test
        @DisplayName("sixParamWithBonusConstructor_defaultsSteps")
        void sixParamWithBonusConstructor_defaultsSteps() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.DOWN);
            Position targetPos = new Position(6, 6);
            SimulationResult.BonusAction bonus = SimulationResult.BonusAction.beam(targetPos);

            // When
            SimulationResult result = new SimulationResult(shift, 3, 90, 1, targetPos, bonus);

            // Then
            assertThat(result.stepsToMove()).isEqualTo(0);
            assertThat(result.bonusAction()).isEqualTo(bonus);
        }
    }

    @Nested
    @DisplayName("Score constants")
    class ScoreConstantsTests {

        @Test
        @DisplayName("scoreConstants_haveCorrectValues")
        void scoreConstants_haveCorrectValues() {
            assertThat(SimulationResult.SCORE_FINISH_GAME).isEqualTo(1000);
            assertThat(SimulationResult.SCORE_DOUBLE_TREASURE).isEqualTo(150);
            assertThat(SimulationResult.SCORE_TARGET_REACHABLE).isEqualTo(100);
            assertThat(SimulationResult.SCORE_BONUS_REACHABLE).isEqualTo(50);
            assertThat(SimulationResult.SCORE_TARGET_WITH_BONUS).isEqualTo(90);
            assertThat(SimulationResult.SCORE_MOVING_CLOSER).isEqualTo(10);
            assertThat(SimulationResult.SCORE_FALLBACK).isEqualTo(1);
            assertThat(SimulationResult.SCORE_NO_MOVE).isEqualTo(0);
        }

        @Test
        @DisplayName("scoreConstants_inDescendingOrder")
        void scoreConstants_inDescendingOrder() {
            // Verify score hierarchy makes sense
            assertThat(SimulationResult.SCORE_FINISH_GAME)
                    .isGreaterThan(SimulationResult.SCORE_DOUBLE_TREASURE);
            assertThat(SimulationResult.SCORE_DOUBLE_TREASURE)
                    .isGreaterThan(SimulationResult.SCORE_TARGET_REACHABLE);
            assertThat(SimulationResult.SCORE_TARGET_REACHABLE)
                    .isGreaterThan(SimulationResult.SCORE_TARGET_WITH_BONUS);
            assertThat(SimulationResult.SCORE_TARGET_WITH_BONUS)
                    .isGreaterThan(SimulationResult.SCORE_BONUS_REACHABLE);
            assertThat(SimulationResult.SCORE_BONUS_REACHABLE)
                    .isGreaterThan(SimulationResult.SCORE_MOVING_CLOSER);
            assertThat(SimulationResult.SCORE_MOVING_CLOSER)
                    .isGreaterThan(SimulationResult.SCORE_FALLBACK);
            assertThat(SimulationResult.SCORE_FALLBACK)
                    .isGreaterThan(SimulationResult.SCORE_NO_MOVE);
        }
    }

    @Nested
    @DisplayName("BonusAction factory methods")
    class BonusActionTests {

        @Test
        @DisplayName("beam_createsBeamAction")
        void beam_createsBeamAction() {
            // Given
            Position targetPos = new Position(3, 4);

            // When
            SimulationResult.BonusAction action = SimulationResult.BonusAction.beam(targetPos);

            // Then
            assertThat(action.bonusType()).isEqualTo(BonusType.BEAM);
            assertThat(action.targetPosition()).isEqualTo(targetPos);
            assertThat(action.targetPlayerId()).isNull();
            assertThat(action.pushFixedOp()).isNull();
            assertThat(action.pushFixedRotations()).isZero();
            assertThat(action.secondPush()).isNull();
            assertThat(action.secondPushRotations()).isZero();
        }

        @Test
        @DisplayName("swap_createsSwapAction")
        void swap_createsSwapAction() {
            // Given
            String playerId = "player-2";
            Position resultPos = new Position(5, 5);

            // When
            SimulationResult.BonusAction action = SimulationResult.BonusAction.swap(playerId, resultPos);

            // Then
            assertThat(action.bonusType()).isEqualTo(BonusType.SWAP);
            assertThat(action.targetPlayerId()).isEqualTo(playerId);
            assertThat(action.targetPosition()).isEqualTo(resultPos);
            assertThat(action.pushFixedOp()).isNull();
            assertThat(action.secondPush()).isNull();
        }

        @Test
        @DisplayName("pushFixed_createsPushFixedAction")
        void pushFixed_createsPushFixedAction() {
            // Given
            ShiftOperation op = ShiftOperation.row(1, Direction.UP);
            int rotations = 2;

            // When
            SimulationResult.BonusAction action = SimulationResult.BonusAction.pushFixed(op, rotations);

            // Then
            assertThat(action.bonusType()).isEqualTo(BonusType.PUSH_FIXED);
            assertThat(action.pushFixedOp()).isEqualTo(op);
            assertThat(action.pushFixedRotations()).isEqualTo(rotations);
            assertThat(action.targetPosition()).isNull();
            assertThat(action.targetPlayerId()).isNull();
            assertThat(action.secondPush()).isNull();
        }

        @Test
        @DisplayName("pushTwice_createsPushTwiceAction")
        void pushTwice_createsPushTwiceAction() {
            // Given
            ShiftOperation secondOp = ShiftOperation.column(5, Direction.DOWN);
            int secondRotations = 3;

            // When
            SimulationResult.BonusAction action = SimulationResult.BonusAction.pushTwice(secondOp, secondRotations);

            // Then
            assertThat(action.bonusType()).isEqualTo(BonusType.PUSH_TWICE);
            assertThat(action.secondPush()).isEqualTo(secondOp);
            assertThat(action.secondPushRotations()).isEqualTo(secondRotations);
            assertThat(action.targetPosition()).isNull();
            assertThat(action.targetPlayerId()).isNull();
            assertThat(action.pushFixedOp()).isNull();
        }
    }

    @Nested
    @DisplayName("Record equality")
    class EqualityTests {

        @Test
        @DisplayName("twoResultsWithSameValues_areEqual")
        void twoResultsWithSameValues_areEqual() {
            // Given
            ShiftOperation shift = ShiftOperation.row(3, Direction.UP);
            Position pos = new Position(2, 2);

            SimulationResult result1 = new SimulationResult(shift, 1, 100, 5, pos);
            SimulationResult result2 = new SimulationResult(shift, 1, 100, 5, pos);

            // Then
            assertThat(result1).isEqualTo(result2);
            assertThat(result1.hashCode()).isEqualTo(result2.hashCode());
        }

        @Test
        @DisplayName("twoBonusActionsWithSameValues_areEqual")
        void twoBonusActionsWithSameValues_areEqual() {
            // Given
            Position pos = new Position(3, 3);
            SimulationResult.BonusAction action1 = SimulationResult.BonusAction.beam(pos);
            SimulationResult.BonusAction action2 = SimulationResult.BonusAction.beam(pos);

            // Then
            assertThat(action1).isEqualTo(action2);
            assertThat(action1.hashCode()).isEqualTo(action2.hashCode());
        }
    }
}
