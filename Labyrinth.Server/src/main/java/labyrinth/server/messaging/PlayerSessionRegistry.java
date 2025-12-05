package labyrinth.server.messaging;

import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.WebSocketSession;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class PlayerSessionRegistry implements IPlayerSessionRegistry {

    private final Map<UUID, WebSocketSession> sessionByPlayerId = new ConcurrentHashMap<>();
    private final Map<UUID, Long> disconnectedAt = new ConcurrentHashMap<>();

    private static final String PLAYER_ID_KEY = "PLAYER_ID";
    private static final String IDENTIFIER_TOKEN_KEY = "IDENTIFIER_TOKEN";

    @Override
    public void registerPlayer(UUID playerId, UUID identifierToken, WebSocketSession session) {
        session.getAttributes().put(PLAYER_ID_KEY, playerId);
        session.getAttributes().put(IDENTIFIER_TOKEN_KEY, identifierToken);

        sessionByPlayerId.put(playerId, session);
        disconnectedAt.remove(playerId);
    }

    @Override
    public void removePlayer(UUID playerId) {
        sessionByPlayerId.remove(playerId);
        disconnectedAt.remove(playerId);
    }

    @Override
    public void markDisconnected(WebSocketSession session) {
        UUID playerId = getPlayerId(session);
        if (playerId == null) {
            return;
        }

        disconnectedAt.put(playerId, System.currentTimeMillis());
        sessionByPlayerId.remove(playerId);
    }

    @Override
    public Map<UUID, Long> getDisconnectedEntries() {
        return disconnectedAt;
    }

    @Override
    public WebSocketSession getSession(UUID playerId) {
        return sessionByPlayerId.get(playerId);
    }

    @Override
    public UUID getPlayerId(WebSocketSession session) {
        return (UUID) session.getAttributes().get(PLAYER_ID_KEY);
    }

    @Override
    public UUID getPlayerIdByIdentifierToken(UUID identifierToken) {
        for (Map.Entry<UUID, WebSocketSession> entry : sessionByPlayerId.entrySet()) {
            WebSocketSession session = entry.getValue();
            UUID token = (UUID) session.getAttributes().get(IDENTIFIER_TOKEN_KEY);
            if (identifierToken.equals(token)) {
                return entry.getKey();
            }
        }
        return null;
    }

    @Override
    public Collection<WebSocketSession> getAllPlayerSessions() {
        return sessionByPlayerId.values();
    }

    @Override
    public boolean isSessionRegistered(WebSocketSession session) {
        return getPlayerId(session) != null;
    }
}
