package labyrinth.server.messaging.models;

import lombok.Getter;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;

public class PlayerRegistration {

    @Getter
    private final UUID playerId;

    @Getter
    private final UUID identifierToken;

    @Getter
    private WebSocketSession session;

    @Getter
    private Long disconnectedAt;

    public PlayerRegistration(UUID playerId, UUID identifierToken, WebSocketSession session) {
        this.playerId = playerId;
        this.identifierToken = identifierToken;
        this.session = session;
        this.disconnectedAt = null;
    }

    public void markDisconnected() {
        this.disconnectedAt = System.currentTimeMillis();
        this.session = null;
    }

    public boolean isConnected() {
        return this.disconnectedAt == null;
    }
}