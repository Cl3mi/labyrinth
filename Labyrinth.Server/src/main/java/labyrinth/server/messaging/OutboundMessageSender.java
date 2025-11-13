package labyrinth.server.messaging;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.Collection;

@Component
public class OutboundMessageSender {

    private final ObjectMapper mapper;

    public OutboundMessageSender(ObjectMapper mapper) {
        this.mapper = mapper;
    }

    public void send(WebSocketSession session, Object dto) {
        try {
            String json = mapper.writeValueAsString(dto);
            session.sendMessage(new TextMessage(json));
        } catch (IOException e) {
            // optional log
        }
    }

    public void broadcast(Collection<WebSocketSession> sessions, Object dto) {
        sessions.forEach(s -> send(s, dto));
    }
}