# Game Model Refactoring Plan

## Overview
This document outlines the step-by-step refactoring of the `Game` class from a 522-line "God Class" into a clean, testable, and maintainable architecture.

## Current Problems

### 1. Single Responsibility Violation
The `Game` class handles:
- Player management (join/leave/AI toggle)
- Turn progression & state machine
- Board manipulation (shift/rotate/move)
- Bonus activation & state management
- Achievement awarding
- Game lifecycle (start/end/reset)
- Logging coordination
- AI strategy orchestration
- Timer management

### 2. Tight Coupling
- Cannot inject `PlayerRegistry`, `TurnController`, `MovementManager`, `AchievementService` (hardcoded in constructor)
- Cannot mock internal services for testing
- No interfaces for dependencies

### 3. Low Testability
- Requires complex setup (board, 4 players, treasures)
- Cannot test services in isolation
- Side effects scattered throughout methods

## Refactoring Strategy

### Phase 1: Extract Interfaces ✅ (Current Phase)
Create interfaces for all internal services to enable dependency injection and mocking.

**Files to create:**
- `IPlayerRegistry.java` - Player management interface
- `ITurnController.java` - Turn coordination interface
- `IMovementManager.java` - Movement validation interface
- `IAchievementService.java` - Achievement awarding interface
- `IBonusManager.java` - Bonus management interface (new)

### Phase 2: Extract Bonus Manager
Move bonus logic from `Game` into dedicated `BonusManager` service.

**Current issues:**
- Bonus effects map (`bonusEffects`) hardcoded in constructor
- Bonus validation logic in `Game.useBonus()`
- Bonus state management scattered

**New structure:**
```
BonusManager
├── BonusRegistry (holds bonus effects)
├── BonusValidator (checks if bonus can be used)
└── BonusStateManager (tracks active bonuses)
```

### Phase 3: Extract Player Action Service
Create dedicated service for player actions (shift, move, rotate).

**Responsibilities:**
- Validate player actions
- Execute board manipulations
- Calculate and apply score rewards
- Log actions

**Benefits:**
- Testable in isolation
- Clear single responsibility
- Reusable across different game modes

### Phase 4: Extract Game Lifecycle Service
Handle game state transitions (LOBBY → IN_GAME → FINISHED).

**Responsibilities:**
- Start game initialization
- Game over detection and processing
- Return to lobby logic
- Reset for new game

### Phase 5: Refactor Game to Facade
Transform `Game` into lightweight coordinator that delegates to services.

**Target size:** ~150 lines (down from 522)

**Structure:**
```java
public class Game {
    // Injected dependencies (all interfaces)
    private final IPlayerRegistry playerRegistry;
    private final ITurnController turnController;
    private final IMovementManager movementManager;
    private final IAchievementService achievementService;
    private final IBonusManager bonusManager;
    private final IGameLifecycleService lifecycleService;
    private final IPlayerActionService playerActionService;

    // Minimal state
    private Board board;
    private RoomState roomState;
    private GameConfig gameConfig;

    // Pure delegation methods
    public Player join(String username) {
        lifecycleService.validateJoin(roomState);
        return playerRegistry.addPlayer(username);
    }

    public ShiftResult shift(int index, Direction dir, Player player) {
        return playerActionService.shift(this, index, dir, player);
    }
}
```

## Success Metrics

### Code Quality
- [ ] `Game` class < 200 lines
- [ ] Each service < 150 lines
- [ ] All services have interfaces
- [ ] No hardcoded dependencies in constructors

### Testability
- [ ] 80%+ unit test coverage for all services
- [ ] Game can be constructed with mock services
- [ ] Each service testable in isolation
- [ ] Integration tests pass

### Maintainability
- [ ] Clear separation of concerns
- [ ] Easy to add new bonus types
- [ ] Easy to add new game rules/modes
- [ ] Reduced cyclomatic complexity

## Implementation Order

1. ✅ **IPlayerRegistry** interface
2. ✅ **ITurnController** interface
3. ✅ **IMovementManager** interface
4. ✅ **IAchievementService** interface
5. **IBonusManager** interface + implementation
6. **IPlayerActionService** interface + implementation
7. **IGameLifecycleService** interface + implementation
8. Refactor `Game` to use interfaces
9. Update `GameService`
10. Create comprehensive unit tests
11. Update integration tests

## Files to Modify

### Core Game Files
- `Game.java` - Refactor to facade
- `GameService.java` - Update to work with new Game structure

### Services (Add Interfaces)
- `PlayerRegistry.java` → `IPlayerRegistry.java`
- `TurnController.java` → `ITurnController.java`
- `MovementManager.java` → `IMovementManager.java`
- `AchievementService.java` → `IAchievementService.java`

### New Services
- `BonusManager.java` + `IBonusManager.java`
- `PlayerActionService.java` + `IPlayerActionService.java`
- `GameLifecycleService.java` + `IGameLifecycleService.java`

### Bonus Effects (Update)
- `BeamBonusEffect.java` - Update to work with new structure
- `SwapBonusEffect.java`
- `PushTwiceBonusEffect.java`
- `PushFixedBonusEffect.java`

## Testing Strategy

### Unit Tests (New)
- `BonusManagerTest.java`
- `PlayerActionServiceTest.java`
- `GameLifecycleServiceTest.java`
- `PlayerRegistryTest.java`
- `TurnControllerTest.java`
- `MovementManagerTest.java`

### Integration Tests (Update)
- `GameServiceTest.java` - Update to work with new structure
- `GameStartTest.java` - Verify game initialization
- `GameOverConditionTest.java` - Verify win conditions

## Migration Path

### Backward Compatibility
- Keep existing public API of `Game` class
- Add `@Deprecated` annotations to methods that will be removed
- Provide migration guide for external consumers

### Rollout Strategy
1. Create new services alongside old code
2. Add feature flag to switch between old/new implementation
3. Run both implementations in parallel during testing
4. Remove old implementation after verification

## Next Steps

1. Start with `IPlayerRegistry` interface extraction
2. Implement and test each interface
3. Gradually refactor `Game` class
4. Add comprehensive tests at each step
