package labyrinth.server.game.constants;

public final class PointRewards {
    private PointRewards() {
        throw new UnsupportedOperationException("This is a constants utility class and should not be instantiated.");
    }

    public static final int REWARD_MOVE = 1;
    public static final int REWARD_TREASURE = 10;
    public static final int REWARD_PUSH_TILE = 1;
    public static final int REWARD_ALL_TREASURES_COLLECTED = 100;
    public static final int REWARD_BONUS_USED = 5;
}
