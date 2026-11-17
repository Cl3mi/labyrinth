package labyrinth.server.persistence;

import java.time.OffsetDateTime;
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
}
