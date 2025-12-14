package labyrinth.server.game.abstractions;

import java.time.OffsetDateTime;

public interface IGameTimer {
    void start(int seconds, Runnable onTimeout);
    void stop();
    OffsetDateTime getExpirationTime();
}
