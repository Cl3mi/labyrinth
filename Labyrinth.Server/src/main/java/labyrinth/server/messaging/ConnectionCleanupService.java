package labyrinth.server.messaging;

import labyrinth.server.game.abstractions.IGame;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ConnectionCleanupService {
    public IGame game;
    private final PlayerSessionRegistry playerSessionRegistry;

    @Scheduled(fixedRate = 1000)
    public void cleanupDisconnected() {
        long now = System.currentTimeMillis();

        playerSessionRegistry.getDisconnectedEntries().forEach((playerId, ts) -> {
            if (now - ts >= 30_000) {

                var player = game.getPlayer(playerId);
                game.leave(player);

                playerSessionRegistry.removePlayer(playerId);
            }
        });
    }
}
