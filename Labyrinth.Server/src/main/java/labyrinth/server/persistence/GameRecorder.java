package labyrinth.server.persistence;

import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.UUID;

/**
 * Records games and their results. Implementations must never throw into the
 * game logic and must not block the caller: a database problem may not stop a
 * running game.
 */
public interface GameRecorder {

    void gameStarted(GameRecords.GameStarted game);

    void gameFinished(GameRecords.GameFinished result);

    /** The game ended without a result, e.g. all human players left. */
    void gameAborted(UUID gameId, OffsetDateTime endedAt);

    void gameSnapshot(GameSnapshot snapshot);

    /**
     * Called once at startup, before players connect. Returns the latest running game
     * of this instance if it can be continued and its snapshot is not older than
     * {@code maxAge}; all other running games of this instance are marked aborted.
     * Blocks until done.
     */
    Optional<GameSnapshot> takeOverRunningGame(Duration maxAge);
}
