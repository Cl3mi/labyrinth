package labyrinth.server.messaging.abstractions;

import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;

public interface IMessageService {
    void sendToSession(WebSocketSession session, Object payload);

    void sendToPlayer(UUID playerId, Object payload);

    void broadcastToPlayers(Object payload);
}
