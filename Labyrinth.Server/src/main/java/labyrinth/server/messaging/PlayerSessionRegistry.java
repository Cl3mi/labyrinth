package labyrinth.server.messaging;

import labyrinth.server.game.Player;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.WebSocketSession;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class PlayerSessionRegistry {

    private final Map<String, UUID> playerIdBySessionId = new ConcurrentHashMap<>();
    private final Map<UUID, WebSocketSession> sessionByPlayerId = new ConcurrentHashMap<>();
    private final Map<UUID, Long> disconnectedAt = new ConcurrentHashMap<>();

    public void registerPlayer(Player player, WebSocketSession session) {
        UUID playerId = player.getId();
        String sessionId = session.getId();

        playerIdBySessionId.put(sessionId, playerId);
        sessionByPlayerId.put(playerId, session);

        disconnectedAt.remove(playerId);
    }

    public void removePlayer(UUID playerId) {
        var session  = sessionByPlayerId.remove(playerId);

        if (session != null) {
            playerIdBySessionId.remove(session.getId());
        }

        disconnectedAt.remove(playerId);
    }

    public void markDisconnected(WebSocketSession session) {
        UUID playerId = playerIdBySessionId.get(session.getId());
        if (playerId == null) return;

        disconnectedAt.put(playerId, System.currentTimeMillis());
        sessionByPlayerId.remove(playerId);
        playerIdBySessionId.remove(session.getId());
    }

    public Map<UUID, Long> getDisconnectedEntries() {
        return disconnectedAt;
    }


    public WebSocketSession getSession(Player player) {
        return sessionByPlayerId.get(player.getId());
    }


    public UUID getPlayerId(WebSocketSession session) {
        return playerIdBySessionId.get(session.getId());
    }

    public Collection<WebSocketSession> getAllPlayerSessions() {
        return sessionByPlayerId.values();
    }

    public boolean isSessionRegistered(String sessionId) {
        return playerIdBySessionId.containsKey(sessionId);
    }
}
