package labyrinth.server.messaging;

import labyrinth.server.game.models.Player;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.WebSocketSession;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class PlayerSessionRegistry {

    private final Map<UUID, WebSocketSession> sessionByPlayerId = new ConcurrentHashMap<>();
    private final Map<UUID, Long> disconnectedAt = new ConcurrentHashMap<>();

    private static final String PLAYER_ID_KEY = "PLAYER_ID";

    public void registerPlayer(Player player, WebSocketSession session) {
        UUID playerId = player.getId();

        session.getAttributes().put(PLAYER_ID_KEY, playerId);

        sessionByPlayerId.put(playerId, session);
        disconnectedAt.remove(playerId);
    }

    public void removePlayer(UUID playerId) {
        sessionByPlayerId.remove(playerId);
        disconnectedAt.remove(playerId);
    }

    public void markDisconnected(WebSocketSession session) {
        UUID playerId = getPlayerId(session);
        if (playerId == null) {
            return;
        }

        disconnectedAt.put(playerId, System.currentTimeMillis());
        sessionByPlayerId.remove(playerId);
    }

    public Map<UUID, Long> getDisconnectedEntries() {
        return disconnectedAt;
    }

    public WebSocketSession getSession(Player player) {
        return sessionByPlayerId.get(player.getId());
    }

    public UUID getPlayerId(WebSocketSession session) {
        return (UUID) session.getAttributes().get(PLAYER_ID_KEY);
    }

    public Collection<WebSocketSession> getAllPlayerSessions() {
        return sessionByPlayerId.values();
    }

    public boolean isSessionRegistered(WebSocketSession session) {
        return getPlayerId(session) != null;
    }
}
