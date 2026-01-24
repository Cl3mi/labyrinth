package labyrinth.server.messaging;

import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ConnectionCleanupService {
    private final GameService gameService;
    private final PlayerSessionRegistry playerSessionRegistry;
    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;

    private static final org.slf4j.Logger log = LoggerFactory.getLogger(ConnectionCleanupService.class);

    /**
     * Proactively checks all registered players to detect stale sessions.
     * This catches "ghost players" where the connection dropped without triggering afterConnectionClosed().
     * Runs every 5 seconds.
     */
    @Scheduled(fixedRate = 5000)
    public void detectStaleConnections() {
        playerSessionRegistry.checkAndMarkStaleConnections();
    }

    /**
     * Handles players who have been disconnected for more than 30 seconds.
     * After 30 seconds, the player permanently becomes AI-controlled.
     * Runs every second.
     */
    @Scheduled(fixedRate = 1000)
    public void cleanupDisconnected() {
        long now = System.currentTimeMillis();

        playerSessionRegistry.getDisconnectedEntries().forEach((playerId, ts) -> {
            if (now - ts < 30_000) {
                return;
            }
            playerSessionRegistry.removePlayer(playerId);

            var player = gameService.getPlayer(playerId);
            if (player != null) {
                if (gameService.getGameState() == RoomState.LOBBY) {
                    gameService.leave(player);
                } else {
                    log.info("[ConnectionCleanupService] Player {} has been disconnected for 30s, permanently converting to AI", player.getUsername());
                    gameService.enableAiAndMarkDisconnected(player);
                }

            } else {
                playerSessionRegistry.removePlayer(playerId);
            }
        });
    }
}
