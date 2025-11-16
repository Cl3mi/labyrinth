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

    public void registerPlayer(Player player, WebSocketSession session) {
        UUID playerId = player.getId();
        String sessionId = session.getId();

        playerIdBySessionId.put(sessionId, playerId);
        sessionByPlayerId.put(playerId, session);
    }

    public UUID removePlayerBySessionId(String sessionId) {
        UUID playerId = playerIdBySessionId.remove(sessionId);
        if (playerId != null) {
            sessionByPlayerId.remove(playerId);
        }
        return playerId;
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
