package labyrinth.server.messaging;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class SessionManager {
     private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    public void addSession(WebSocketSession session) {
        sessions.put(session.getId(), session);
    }

    public void removeSession(WebSocketSession session) {
        sessions.remove(session.getId());
    }

    public WebSocketSession getSession(String sessionId) {
        return sessions.get(sessionId);
    }

    public Collection<WebSocketSession> getAllSessions() {
        return sessions.values();
    }

    public int count() {
        return sessions.size();
    }
}
