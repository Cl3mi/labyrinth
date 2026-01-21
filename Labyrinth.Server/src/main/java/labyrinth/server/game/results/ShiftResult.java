package labyrinth.server.game.results;

/**
 * Result of a shift operation.
 *
 * @param shiftSuccess        true if the shift was successful
 * @param pusherAchieved      true if the pusher achievement was unlocked
 * @param blockedByReverseRule true if the shift was blocked because it would reverse the previous shift
 */
public record ShiftResult(boolean shiftSuccess, boolean pusherAchieved, boolean blockedByReverseRule) {

    /**
     * Constructor for backward compatibility - defaults blockedByReverseRule to false.
     */
    public ShiftResult(boolean shiftSuccess, boolean pusherAchieved) {
        this(shiftSuccess, pusherAchieved, false);
    }
}
