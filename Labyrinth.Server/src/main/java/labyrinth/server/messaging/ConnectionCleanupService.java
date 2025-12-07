package labyrinth.server.messaging;

import labyrinth.server.game.GameService;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ConnectionCleanupService {
    public GameService gameService;
    private final IPlayerSessionRegistry IPlayerSessionRegistry;

    @Scheduled(fixedRate = 1000)
    public void cleanupDisconnected() {
        long now = System.currentTimeMillis();

        IPlayerSessionRegistry.getDisconnectedEntries().forEach((playerId, ts) -> {
            if (now - ts >= 30_000) {

                var player = gameService.getPlayer(playerId);
                gameService.leave(player);

                IPlayerSessionRegistry.removePlayer(playerId);
            }
        });
    }
}
