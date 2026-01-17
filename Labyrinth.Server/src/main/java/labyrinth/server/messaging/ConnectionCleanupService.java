package labyrinth.server.messaging;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
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
     * Handles players who have been disconnected for more than 30 seconds.
     * After 30 seconds, the player permanently becomes AI-controlled.
     * Runs every second.
     */
    @Scheduled(fixedRate = 1000)
    public void cleanupDisconnected() {
        long now = System.currentTimeMillis();

        playerSessionRegistry.getDisconnectedEntries().forEach((playerId, ts) -> {
            if (now - ts >= 30_000) {
                var player = gameService.getPlayer(playerId);
                if (player != null) {
                    if (gameService.getGameState() == RoomState.LOBBY) {
                        gameService.leave(player);
                        broadcastLobbyState();
                        playerSessionRegistry.removePlayer(playerId);
                    } else {
                        log.info("[ConnectionCleanupService] Player {} has been disconnected for 30s, permanently converting to AI", player.getUsername());
                        player.setAiActive(true);
                        player.setDisconnected(false);
                        playerSessionRegistry.removePlayer(playerId);
                    }
                } else {
                    playerSessionRegistry.removePlayer(playerId);
                }
            }
        });
    }

    /**
     * Cleans up expired tokens for players who have been disconnected for more than 24 hours.
     * Prevents memory leaks from abandoned sessions. Runs every hour.
     */
    @Scheduled(fixedRate = 3600000) // Every hour (3600000 ms)
    public void cleanupExpiredTokens() {
        long now = System.currentTimeMillis();

        playerSessionRegistry.getDisconnectedEntries().forEach((playerId, ts) -> {
            // 24 hours = 86400000 milliseconds
            if (now - ts >= 86400000) {
                log.info("[ConnectionCleanupService] Removing expired token for player: {}", playerId);

                var player = gameService.getPlayer(playerId);
                if (player != null) {
                    gameService.leave(player);
                }

                playerSessionRegistry.removePlayer(playerId);
            }
        });
    }

    private void broadcastLobbyState() {
        var players = gameService.getPlayers()
                .stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);

        var lobbyStateUpdated = new LobbyStateEventPayload();
        lobbyStateUpdated.setType(EventType.LOBBY_STATE);
        lobbyStateUpdated.setPlayers(players);

        messageService.broadcastToPlayers(lobbyStateUpdated);
    }
}
