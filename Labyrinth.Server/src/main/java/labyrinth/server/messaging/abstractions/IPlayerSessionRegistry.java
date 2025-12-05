package labyrinth.server.messaging.abstractions;

import org.springframework.web.socket.WebSocketSession;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;

public interface IPlayerSessionRegistry {
    void registerPlayer(UUID playerId, UUID identifierToken, WebSocketSession session);

    void removePlayer(UUID playerId);

    void markDisconnected(WebSocketSession session);

    Map<UUID, Long> getDisconnectedEntries();

    WebSocketSession getSession(UUID playerId);

    UUID getPlayerId(WebSocketSession session);

    UUID getPlayerIdByIdentifierToken(UUID identifierToken);

    Collection<WebSocketSession> getAllPlayerSessions();

    boolean isSessionRegistered(WebSocketSession session);
}
