package labyrinth.server.game.models.records;

public record GameConfig(
        int boardWidth,
        int boardHeight,
        int treasureCardCount,
        int gameDurationInSeconds,
        int totalBonusCount,
        int turnTimeInSeconds
) {
    public GameConfig {
        if (boardWidth < 3 || boardWidth > 11)
            throw new IllegalArgumentException("Board width must be between 3 and 11");
        if (boardHeight < 3 || boardHeight > 11)
            throw new IllegalArgumentException("Board height must be between 3 and 11");
        if (boardWidth % 2 == 0)
            throw new IllegalArgumentException("Board width must be odd");
        if (boardHeight % 2 == 0)
            throw new IllegalArgumentException("Board height must be odd");
        if (treasureCardCount <= 0 || treasureCardCount > 24)
            throw new IllegalArgumentException("Treasure card count must be between 1 and 22");
        if (gameDurationInSeconds <= 0 || gameDurationInSeconds > 3600)
            throw new IllegalArgumentException("Game duration must be between 1 and 3600 seconds");
        if (totalBonusCount < 0 || totalBonusCount > 20)
            throw new IllegalArgumentException("Total bonus count must be between 0 and 20");
    }

    public static GameConfig getDefault() {
        return new GameConfig(7, 7,  24, 1800, 3, 30);
    }

    public int getLastRowIndex(){
        return boardHeight - 1;
    }

    public int getLastColIndex(){
        return boardWidth - 1;
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