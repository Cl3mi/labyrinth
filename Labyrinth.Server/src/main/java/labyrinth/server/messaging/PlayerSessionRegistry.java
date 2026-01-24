package labyrinth.server.messaging;

import labyrinth.server.messaging.models.PlayerRegistration;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.WebSocketSession;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Service
public class PlayerSessionRegistry {

    private final Map<UUID, PlayerRegistration> registrations = new ConcurrentHashMap<>();

    private static final String PLAYER_ID_KEY = "PLAYER_ID";

    public void registerPlayer(UUID playerId, UUID identifierToken, WebSocketSession session) {
        session.getAttributes().put(PLAYER_ID_KEY, playerId);
        registrations.put(playerId, new PlayerRegistration(playerId, identifierToken, session));
    }

    public void removePlayer(UUID playerId) {
        registrations.remove(playerId);
    }

    public void markDisconnected(WebSocketSession session) {
        UUID playerId = getPlayerId(session);
        if (playerId == null) return;

        PlayerRegistration reg = registrations.get(playerId);
        if (reg != null) {
            reg.markDisconnected();
        }
    }

    public UUID getPlayerIdByIdentifierToken(UUID identifierToken) {
        return registrations.values().stream()
                .filter(reg -> reg.getIdentifierToken().equals(identifierToken))
                .map(PlayerRegistration::getPlayerId)
                .findFirst()
                .orElse(null);
    }

    public WebSocketSession getSession(UUID playerId) {
        PlayerRegistration reg = registrations.get(playerId);
        if (reg != null && reg.isConnected()) {
            WebSocketSession session = reg.getSession();
            // Validate session is still open
            if (session != null && session.isOpen()) {
                return session;
            }
            // Session is closed but registry thinks it's connected - fix state
            reg.markDisconnected();
        }
        return null;
    }

    public Map<UUID, Long> getDisconnectedEntries() {
        Map<UUID, Long> result = new HashMap<>();
        for (PlayerRegistration reg : registrations.values()) {
            if (!reg.isConnected()) {
                result.put(reg.getPlayerId(), reg.getDisconnectedAt());
            }
        }
        return result;
    }

    public Collection<WebSocketSession> getAllPlayerSessions() {
        return registrations.values().stream()
                .map(PlayerRegistration::getSession)
                .filter(Objects::nonNull)
                .filter(WebSocketSession::isOpen)  // Only return open sessions
                .collect(Collectors.toList());
    }

    public UUID getPlayerId(WebSocketSession session) {
        return (UUID) session.getAttributes().get(PLAYER_ID_KEY);
    }

    public boolean isSessionRegistered(WebSocketSession session) {
        return getPlayerId(session) != null;
    }

    /**
     * Checks if a player is currently connected.
     * @param playerId The player's UUID
     * @return true if player is connected, false if disconnected or not registered
     */
    public boolean isPlayerConnected(UUID playerId) {
        PlayerRegistration reg = registrations.get(playerId);
        return reg != null && reg.isConnected();
    }

    /**
     * Proactively checks all registrations for stale connections.
     * This detects "ghost players" where the WebSocket closed without triggering afterConnectionClosed().
     */
    public void checkAndMarkStaleConnections() {
        for (PlayerRegistration reg : registrations.values()) {
            if (reg.isConnected()) {
                WebSocketSession session = reg.getSession();
                if (session == null || !session.isOpen()) {
                    reg.markDisconnected();
                }
            }
        }
    }
}
