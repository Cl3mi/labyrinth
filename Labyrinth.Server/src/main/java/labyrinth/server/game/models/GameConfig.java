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
    }

    public static GameConfig getDefault() {
        return new GameConfig(7, 7,  4, 24, 1800, 3);
    }
}