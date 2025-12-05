package labyrinth.server.messaging;

import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ConnectionCleanupService {
    public IGame game;
    private final IPlayerSessionRegistry IPlayerSessionRegistry;

    @Scheduled(fixedRate = 1000)
    public void cleanupDisconnected() {
        long now = System.currentTimeMillis();

        IPlayerSessionRegistry.getDisconnectedEntries().forEach((playerId, ts) -> {
            if (now - ts >= 30_000) {

                var player = game.getPlayer(playerId);
                game.leave(player);

                IPlayerSessionRegistry.removePlayer(playerId);
            }
        });
    }
}
