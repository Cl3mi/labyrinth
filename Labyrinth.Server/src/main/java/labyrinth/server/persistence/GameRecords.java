package labyrinth.server.persistence;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

/**
 * Immutable copies of game data handed to the {@link GameRecorder}.
 * They are built while the game lock is held and contain no references to live
 * game objects, so they can be written to the database on another thread.
 */
public final class GameRecords {

    private GameRecords() {
    }

    public record GameStarted(
            UUID gameId,
            OffsetDateTime startedAt,
            int boardWidth,
            int boardHeight,
            int treasureCardCount,
            int bonusCount,
            int turnTimeSeconds,
            int gameDurationSeconds,
            int playerCount
    ) {
    }

    public record PlayerResult(
            UUID playerId,
            String username,
            boolean bot,
            int placement,
            int score,
            int treasuresCollected,
            int stepsTaken,
            int tilesPushed,
            List<String> achievements,
            boolean endedGame
    ) {
    }

    public record GameFinished(
            UUID gameId,
            OffsetDateTime endedAt,
            List<PlayerResult> players
    ) {
    }
}
