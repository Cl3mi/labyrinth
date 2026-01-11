package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IBonusManager;
import labyrinth.server.game.abstractions.ITurnController;
import labyrinth.server.game.bonuses.*;
import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.GameLogType;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * Manages bonus activation and validation.
 * Encapsulates bonus effect application and turn-based usage tracking.
 */
public class BonusManager implements IBonusManager {

    private final Map<BonusTypes, IBonusEffect> bonusEffects;
    private final ITurnController turnController;
    private final GameLogger gameLogger;

    /**
     * Creates a BonusManager with all bonus effect strategies registered.
     *
     * @param turnController the turn controller for tracking bonus usage per turn
     * @param gameLogger the game logger for logging bonus usage
     */
    public BonusManager(ITurnController turnController, GameLogger gameLogger) {
        this.turnController = turnController;
        this.gameLogger = gameLogger;
        this.bonusEffects = new EnumMap<>(BonusTypes.class);

        // Register all bonus effect strategies
        registerBonusEffects();
    }

    /**
     * Registers all available bonus effect strategies.
     * This method can be extended to support dynamic bonus registration in the future.
     */
    private void registerBonusEffects() {
        bonusEffects.put(BonusTypes.BEAM, new BeamBonusEffect());
        bonusEffects.put(BonusTypes.SWAP, new SwapBonusEffect());
        bonusEffects.put(BonusTypes.PUSH_TWICE, new PushTwiceBonusEffect());
        bonusEffects.put(BonusTypes.PUSH_FIXED, new PushFixedBonusEffect());
    }

    @Override
    public boolean useBonus(BonusTypes type, Game game, Player player, Object... args) {
        // Validate bonus strategy exists
        if (!bonusEffects.containsKey(type)) {
            throw new IllegalArgumentException("No strategy found for bonus type: " + type);
        }

        // Check if a bonus has already been used this turn
        if (turnController.isBonusUsedThisTurn()) {
            throw new IllegalStateException("Only one bonus can be used per turn");
        }

        // Apply the bonus effect
        boolean result = bonusEffects.get(type).apply(game, player, args);

        if (result) {
            // Mark that a bonus has been used this turn
            turnController.markBonusUsed();

            // Award points for using bonus
            player.getStatistics().increaseScore(PointRewards.REWARD_BONUS_USED);

            // Log bonus usage
            Map<String, String> meta = new HashMap<>();
            meta.put("bonusType", type.toString());
            gameLogger.log(GameLogType.USE_BONUS, "Player used bonus " + type, player, meta);
        }

        return result;
    }

    @Override
    public boolean hasStrategy(BonusTypes type) {
        return bonusEffects.containsKey(type);
    }
}
