package labyrinth.server.game.models;

import labyrinth.server.game.enums.BonusTypes;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the BonusState pattern implementation.
 * Validates the State pattern behavior for bonus management.
 */
class BonusStateTest {

    // NoBonusActive Tests

    @Test
    void noBonusActive_shouldReturnFalseForIsActive() {
        // Arrange
        BonusState state = NoBonusActive.getInstance();

        // Act & Assert
        assertFalse(state.isActive(), "NoBonusActive should not be active");
    }

    @Test
    void noBonusActive_shouldReturnEmptyBonusType() {
        // Arrange
        BonusState state = NoBonusActive.getInstance();

        // Act & Assert
        assertTrue(state.getBonusType().isEmpty(), "NoBonusActive should return empty Optional");
    }

    @Test
    void noBonusActive_shouldReturnFalseForAnyBonusType() {
        // Arrange
        BonusState state = NoBonusActive.getInstance();

        // Act & Assert
        assertFalse(state.isOfType(BonusTypes.PUSH_TWICE), "Should return false for PUSH_TWICE");
        assertFalse(state.isOfType(BonusTypes.PUSH_FIXED), "Should return false for PUSH_FIXED");
        assertFalse(state.isOfType(BonusTypes.BEAM), "Should return false for BEAM");
        assertFalse(state.isOfType(BonusTypes.SWAP), "Should return false for SWAP");
    }

    @Test
    void noBonusActive_consumeShouldReturnItself() {
        // Arrange
        BonusState state = NoBonusActive.getInstance();

        // Act
        BonusState consumed = state.consume();

        // Assert
        assertSame(state, consumed, "Consuming NoBonusActive should return the same instance");
    }

    @Test
    void noBonusActive_shouldBeSingleton() {
        // Arrange & Act
        BonusState state1 = NoBonusActive.getInstance();
        BonusState state2 = NoBonusActive.getInstance();

        // Assert
        assertSame(state1, state2, "NoBonusActive should be a singleton");
    }

    // ActiveBonus Tests

    @Test
    void activeBonus_shouldReturnTrueForIsActive() {
        // Arrange
        BonusState state = new ActiveBonus(BonusTypes.PUSH_TWICE);

        // Act & Assert
        assertTrue(state.isActive(), "ActiveBonus should be active");
    }

    @Test
    void activeBonus_shouldReturnCorrectBonusType() {
        // Arrange
        BonusState state = new ActiveBonus(BonusTypes.PUSH_FIXED);

        // Act & Assert
        assertTrue(state.getBonusType().isPresent(), "ActiveBonus should have a bonus type");
        assertEquals(BonusTypes.PUSH_FIXED, state.getBonusType().get(), "Should return correct bonus type");
    }

    @Test
    void activeBonus_isOfTypeShouldReturnTrueForMatchingType() {
        // Arrange
        BonusState state = new ActiveBonus(BonusTypes.BEAM);

        // Act & Assert
        assertTrue(state.isOfType(BonusTypes.BEAM), "Should return true for matching type");
        assertFalse(state.isOfType(BonusTypes.SWAP), "Should return false for non-matching type");
    }

    @Test
    void activeBonus_consumeShouldReturnNoBonusActive() {
        // Arrange
        BonusState state = new ActiveBonus(BonusTypes.PUSH_TWICE);

        // Act
        BonusState consumed = state.consume();

        // Assert
        assertFalse(consumed.isActive(), "Consumed bonus should not be active");
        assertSame(NoBonusActive.getInstance(), consumed, "Should return NoBonusActive singleton");
    }

    @Test
    void activeBonus_shouldThrowExceptionForNullBonusType() {
        // Act & Assert
        assertThrows(NullPointerException.class, () -> {
            new ActiveBonus(null);
        }, "Should throw NullPointerException for null bonus type");
    }

    @Test
    void activeBonus_equalsShouldWorkCorrectly() {
        // Arrange
        BonusState state1 = new ActiveBonus(BonusTypes.PUSH_TWICE);
        BonusState state2 = new ActiveBonus(BonusTypes.PUSH_TWICE);
        BonusState state3 = new ActiveBonus(BonusTypes.PUSH_FIXED);

        // Act & Assert
        assertEquals(state1, state2, "Same bonus types should be equal");
        assertNotEquals(state1, state3, "Different bonus types should not be equal");
    }

    @Test
    void activeBonus_hashCodeShouldBeConsistent() {
        // Arrange
        BonusState state1 = new ActiveBonus(BonusTypes.BEAM);
        BonusState state2 = new ActiveBonus(BonusTypes.BEAM);

        // Act & Assert
        assertEquals(state1.hashCode(), state2.hashCode(), "Equal objects should have same hash code");
    }

    // State Transition Tests

    @Test
    void stateTransition_inactiveToActiveToInactive() {
        // Arrange
        BonusState inactive = NoBonusActive.getInstance();

        // Act - activate
        BonusState active = new ActiveBonus(BonusTypes.PUSH_TWICE);

        // Assert - check active state
        assertFalse(inactive.isActive());
        assertTrue(active.isActive());
        assertEquals(BonusTypes.PUSH_TWICE, active.getBonusType().get());

        // Act - consume
        BonusState backToInactive = active.consume();

        // Assert - back to inactive
        assertFalse(backToInactive.isActive());
        assertSame(NoBonusActive.getInstance(), backToInactive);
    }

    @Test
    void stateTransition_activeDifferentBonuses() {
        // Arrange & Act
        BonusState pushTwice = new ActiveBonus(BonusTypes.PUSH_TWICE);
        BonusState pushFixed = new ActiveBonus(BonusTypes.PUSH_FIXED);

        // Assert
        assertTrue(pushTwice.isOfType(BonusTypes.PUSH_TWICE));
        assertFalse(pushTwice.isOfType(BonusTypes.PUSH_FIXED));

        assertTrue(pushFixed.isOfType(BonusTypes.PUSH_FIXED));
        assertFalse(pushFixed.isOfType(BonusTypes.PUSH_TWICE));
    }

    // toString Tests

    @Test
    void noBonusActive_toStringShouldBeDescriptive() {
        // Arrange
        BonusState state = NoBonusActive.getInstance();

        // Act
        String str = state.toString();

        // Assert
        assertNotNull(str);
        assertTrue(str.contains("NoBonusActive"), "toString should contain 'NoBonusActive'");
    }

    @Test
    void activeBonus_toStringShouldIncludeBonusType() {
        // Arrange
        BonusState state = new ActiveBonus(BonusTypes.BEAM);

        // Act
        String str = state.toString();

        // Assert
        assertNotNull(str);
        assertTrue(str.contains("ActiveBonus"), "toString should contain 'ActiveBonus'");
        assertTrue(str.contains("BEAM"), "toString should contain the bonus type");
    }

    // Integration with all bonus types

    @Test
    void activeBonus_shouldWorkWithAllBonusTypes() {
        // Test all bonus types
        for (BonusTypes type : BonusTypes.values()) {
            BonusState state = new ActiveBonus(type);

            assertTrue(state.isActive(), "Should be active for " + type);
            assertTrue(state.isOfType(type), "Should be of type " + type);
            assertEquals(type, state.getBonusType().get(), "Should return correct type for " + type);

            BonusState consumed = state.consume();
            assertFalse(consumed.isActive(), "Should be inactive after consuming " + type);
        }
    }
}
