package labyrinth.server.persistence;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.enums.Achievement;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

public record GameSnapshot(
        int formatVersion,
        UUID gameId,
        long version,
        OffsetDateTime takenAt,
        Config config,
        OffsetDateTime gameEndTime,
        OffsetDateTime turnEndTime,
        BoardState board,
        List<Treasure> treasures,
        List<PlayerState> players,
        TurnState turn,
        BonusState activeBonus
) {
    public static final int FORMAT_VERSION = 1;

    public record Config(
            int boardWidth,
            int boardHeight,
            int treasureCardCount,
            int gameDurationInSeconds,
            int totalBonusCount,
            int turnTimeInSeconds
    ) {
    }

    public record BoardState(
            int width,
            int height,
            boolean freeRoam,
            List<List<TileState>> rows,
            TileState extraTile
    ) {
    }

    public record TileState(
            Set<Direction> entrances,
            boolean fixed,
            Integer treasureId,
            BonusTypes bonus
    ) {
    }

    public record Treasure(int id, String name, boolean collected) {
    }

    public record Position(int row, int column) {
    }

    public record PlayerState(
            UUID id,
            String username,
            UUID identifierToken,
            PlayerColor color,
            OffsetDateTime joinDate,
            boolean admin,
            boolean aiActive,
            boolean disconnected,
            Position position,
            Position home,
            List<Integer> treasureIds,
            List<BonusTypes> bonuses,
            Statistics statistics
    ) {
    }

    public record Statistics(
            int score,
            int stepsTaken,
            int tilesPushed,
            int treasuresCollected,
            List<Achievement> achievements
    ) {
    }

    public record TurnState(
            int currentPlayerIndex,
            MoveState moveState,
            boolean bonusUsedThisTurn,
            Shift lastShift
    ) {
    }

    public record Shift(int index, Direction direction) {
    }

    public record BonusState(BonusTypes type, int remainingPushes) {
    }
}
