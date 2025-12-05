package labyrinth.server.game.models.records;

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

    public int getLastRowIndex(){
        return  boardWidth - 1;
    }

    public int getLastColIndex(){
        return  boardHeight - 1;
    }

    public Position getStartPosition(int playerIndex){
        return switch (playerIndex) {
            case 0 -> new Position(0, 0);
            case 1 -> new Position(0, getLastColIndex());
            case 2 -> new Position(getLastRowIndex(), getLastColIndex());
            case 3 -> new Position(getLastRowIndex() ,0);
            default -> new Position(0, 0); // TODO: throw
        };
    }
}