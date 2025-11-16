package labyrinth.server.messaging;

import labyrinth.server.game.GameService;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
public class ConnectionCleanupService {
    public GameService gameService;
    private final PlayerSessionRegistry playerSessionRegistry;

    public ConnectionCleanupService(GameService gameService, PlayerSessionRegistry playerSessionRegistry) {
        this.gameService = gameService;
        this.playerSessionRegistry = playerSessionRegistry;
    }


    @Scheduled(fixedRate = 1000)
    public void cleanupDisconnected() {
        long now = System.currentTimeMillis();

        playerSessionRegistry.getDisconnectedEntries().forEach((playerId, ts) -> {
            if (now - ts >= 30_000) {
                gameService.removePlayer(playerId);
                playerSessionRegistry.removePlayer(playerId);
            }
        });
    }
}
