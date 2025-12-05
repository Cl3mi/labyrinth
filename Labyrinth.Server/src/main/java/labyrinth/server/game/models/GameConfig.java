package labyrinth.server.game.models;

public record GameConfig(
        int boardWidth,
        int boardHeight,
        int maxPlayers,
        int treasureCardCount,
        int gameDurationInSeconds,
        int totalBonusCount
) {
    public GameConfig {
        if (maxPlayers < 2 || maxPlayers > 4)
            throw new IllegalArgumentException("Player count must be between 2 and 4");
        if (boardWidth <= 0 || boardHeight <= 0)
            throw new IllegalArgumentException("Board size must be positive");
        if (treasureCardCount <= 0 || treasureCardCount > 100)
            throw new IllegalArgumentException("Treasure card count must be between 1 and 100");
        if (gameDurationInSeconds <= 0 || gameDurationInSeconds > 3600)
            throw new IllegalArgumentException("Game duration must be between 1 and 3600 seconds");
        if (totalBonusCount < 0 || totalBonusCount > 20)
            throw new IllegalArgumentException("Total bonus count must be between 0 and 20");
    }

    public static GameConfig getDefault() {
        return new GameConfig(7, 7,  4, 24, 1800, 3);
    }
}