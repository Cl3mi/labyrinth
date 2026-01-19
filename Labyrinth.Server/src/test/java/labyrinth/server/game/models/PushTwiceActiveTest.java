package labyrinth.server.game.models;

import labyrinth.server.game.enums.BonusTypes;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the PushTwiceActive class.
 * Validates PushTwiceActive state behavior and transitions.
 */
class PushTwiceActiveTest {

    private PushTwiceActive pushTwiceActive;

    @BeforeEach
    void setUp() {
        pushTwiceActive = new PushTwiceActive();
    }

    @Nested
    class InitialState {

        @Test
        void shouldStartWithTwoRemainingPushes() {
            // Assert
            assertEquals(2, pushTwiceActive.getRemainingPushes());
        }

        @Test
        void shouldBeActive() {
            // Assert
            assertTrue(pushTwiceActive.isActive());
        }

        @Test
        void shouldReturnPushTwiceBonusType() {
            // Assert
            assertTrue(pushTwiceActive.getBonusType().isPresent());
            assertEquals(BonusTypes.PUSH_TWICE, pushTwiceActive.getBonusType().get());
        }

        @Test
        void shouldBeOfTypePushTwice() {
            // Assert
            assertTrue(pushTwiceActive.isOfType(BonusTypes.PUSH_TWICE));
        }

        @Test
        void shouldNotBeOfOtherTypes() {
            // Assert
            assertFalse(pushTwiceActive.isOfType(BonusTypes.PUSH_FIXED));
            assertFalse(pushTwiceActive.isOfType(BonusTypes.BEAM));
            assertFalse(pushTwiceActive.isOfType(BonusTypes.SWAP));
        }
    }

    @Nested
    class FirstConsume {

        @Test
        void shouldReturnPushTwiceActiveWithOnePush() {
            // Act
            BonusState consumed = pushTwiceActive.consume();

            // Assert
            assertTrue(consumed instanceof PushTwiceActive);
            assertEquals(1, ((PushTwiceActive) consumed).getRemainingPushes());
        }

        @Test
        void shouldStillBeActive() {
            // Act
            BonusState consumed = pushTwiceActive.consume();

            // Assert
            assertTrue(consumed.isActive());
        }

        @Test
        void shouldStillBeOfTypePushTwice() {
            // Act
            BonusState consumed = pushTwiceActive.consume();

            // Assert
            assertTrue(consumed.isOfType(BonusTypes.PUSH_TWICE));
        }

        @Test
        void shouldReturnNewInstance() {
            // Act
            BonusState consumed = pushTwiceActive.consume();

            // Assert
            assertNotSame(pushTwiceActive, consumed);
        }

        @Test
        void shouldNotModifyOriginal() {
            // Act
            pushTwiceActive.consume();

            // Assert
            assertEquals(2, pushTwiceActive.getRemainingPushes());
        }
    }

    @Nested
    class SecondConsume {

        @Test
        void shouldReturnNoBonusActive() {
            // Arrange
            BonusState firstConsume = pushTwiceActive.consume();

            // Act
            BonusState secondConsume = firstConsume.consume();

            // Assert
            assertSame(NoBonusActive.getInstance(), secondConsume);
        }

        @Test
        void shouldNotBeActive() {
            // Arrange
            BonusState firstConsume = pushTwiceActive.consume();

            // Act
            BonusState secondConsume = firstConsume.consume();

            // Assert
            assertFalse(secondConsume.isActive());
        }

        @Test
        void shouldNotBeOfTypePushTwice() {
            // Arrange
            BonusState firstConsume = pushTwiceActive.consume();

            // Act
            BonusState secondConsume = firstConsume.consume();

            // Assert
            assertFalse(secondConsume.isOfType(BonusTypes.PUSH_TWICE));
        }

        @Test
        void shouldHaveEmptyBonusType() {
            // Arrange
            BonusState firstConsume = pushTwiceActive.consume();

            // Act
            BonusState secondConsume = firstConsume.consume();

            // Assert
            assertTrue(secondConsume.getBonusType().isEmpty());
        }
    }

    @Nested
    class CompleteTransitionChain {

        @Test
        void shouldTransitionFromTwoToOneToNone() {
            // Act
            BonusState state1 = pushTwiceActive.consume();
            BonusState state2 = state1.consume();

            // Assert
            assertEquals(2, pushTwiceActive.getRemainingPushes());
            assertEquals(1, ((PushTwiceActive) state1).getRemainingPushes());
            assertSame(NoBonusActive.getInstance(), state2);
        }

        @Test
        void shouldConsumeThreeTimesWithoutError() {
            // Act
            BonusState state1 = pushTwiceActive.consume();
            BonusState state2 = state1.consume();
            BonusState state3 = state2.consume();

            // Assert
            assertSame(NoBonusActive.getInstance(), state2);
            assertSame(NoBonusActive.getInstance(), state3);
        }
    }

    @Nested
    class IsOfType {

        @ParameterizedTest
        @EnumSource(BonusTypes.class)
        void shouldOnlyMatchPushTwice(BonusTypes type) {
            // Act & Assert
            if (type == BonusTypes.PUSH_TWICE) {
                assertTrue(pushTwiceActive.isOfType(type));
            } else {
                assertFalse(pushTwiceActive.isOfType(type));
            }
        }
    }

    @Nested
    class ToString {

        @Test
        void shouldIncludeClassName() {
            // Act
            String str = pushTwiceActive.toString();

            // Assert
            assertTrue(str.contains("PushTwiceActive"));
        }

        @Test
        void shouldIncludeRemainingPushes() {
            // Act
            String str = pushTwiceActive.toString();

            // Assert
            assertTrue(str.contains("remainingPushes"));
            assertTrue(str.contains("2"));
        }

        @Test
        void shouldUpdateAfterConsume() {
            // Arrange
            BonusState consumed = pushTwiceActive.consume();

            // Act
            String str = consumed.toString();

            // Assert
            assertTrue(str.contains("1"));
        }
    }

    @Nested
    class Immutability {

        @Test
        void shouldNotModifyStateOnConsume() {
            // Arrange
            int initialPushes = pushTwiceActive.getRemainingPushes();

            // Act
            pushTwiceActive.consume();
            pushTwiceActive.consume();
            pushTwiceActive.consume();

            // Assert
            assertEquals(initialPushes, pushTwiceActive.getRemainingPushes());
        }

        @Test
        void shouldCreateNewInstanceOnEachConsume() {
            // Act
            BonusState state1 = pushTwiceActive.consume();
            BonusState state2 = state1.consume();

            // Assert
            assertNotSame(pushTwiceActive, state1);
            assertNotSame(state1, state2);
        }
    }

    @Nested
    class EdgeCases {

        @Test
        void shouldHandleRapidConsumeCalls() {
            // Act
            BonusState result = pushTwiceActive
                    .consume()
                    .consume()
                    .consume()
                    .consume();

            // Assert
            assertFalse(result.isActive());
        }

        @Test
        void shouldReturnSameNoBonusActiveSingleton() {
            // Act
            BonusState result1 = pushTwiceActive.consume().consume();
            BonusState result2 = pushTwiceActive.consume().consume();

            // Assert
            assertSame(result1, result2);
            assertSame(NoBonusActive.getInstance(), result1);
        }
    }

    @Nested
    class ComparisonWithActiveBonus {

        @Test
        void shouldBothReturnPushTwiceBonusType() {
            // Arrange
            ActiveBonus activeBonus = new ActiveBonus(BonusTypes.PUSH_TWICE);

            // Assert
            assertEquals(pushTwiceActive.getBonusType(), activeBonus.getBonusType());
        }

        @Test
        void shouldBothBeActive() {
            // Arrange
            ActiveBonus activeBonus = new ActiveBonus(BonusTypes.PUSH_TWICE);

            // Assert
            assertTrue(pushTwiceActive.isActive());
            assertTrue(activeBonus.isActive());
        }

        @Test
        void shouldDifferInConsumeTransitions() {
            // Arrange
            ActiveBonus activeBonus = new ActiveBonus(BonusTypes.PUSH_TWICE);

            // Act
            BonusState pushTwiceConsumed = pushTwiceActive.consume();
            BonusState activeBonusConsumed = activeBonus.consume();

            // Assert - PushTwiceActive should still be active, ActiveBonus should not
            assertTrue(pushTwiceConsumed.isActive(), "PushTwiceActive should still be active after first consume");
            assertFalse(activeBonusConsumed.isActive(), "ActiveBonus should not be active after consume");
        }
    }
}
