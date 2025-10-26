package labyrinth.game.models;

public record GameConfig(
        int boardWidth,
        int boardHeight,
        int amountOfTreasuresPerPlayer,
        int maxPlayers
) {
    public GameConfig {
        if (maxPlayers < 2 || maxPlayers > 4)
            throw new IllegalArgumentException("Player count must be between 2 and 4");
        if (amountOfTreasuresPerPlayer < 1 || amountOfTreasuresPerPlayer > 10)
            throw new IllegalArgumentException("Players must have between 1 and 10 treasures");
        if (boardWidth <= 0 || boardHeight <= 0)
            throw new IllegalArgumentException("Board size must be positive");
    }
}