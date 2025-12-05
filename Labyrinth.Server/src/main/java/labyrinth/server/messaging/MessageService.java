package labyrinth.server.messaging;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class MessageService {

    private static final Logger log = LoggerFactory.getLogger(MessageService.class);
    private final PlayerSessionRegistry playerSessionService;
    private final ObjectMapper objectMapper;

    public void sendToSession(WebSocketSession session, Object payload) {
        if (session != null && session.isOpen()) {
            try {
                String jsonPayload = objectMapper.writeValueAsString(payload);
                session.sendMessage(new TextMessage(jsonPayload));
            } catch (IOException e) {
                log.error("IOException at sendToSession (SessionId: {}).", session.getId(), e);
            }
        }
    }


    public void sendToPlayer(UUID playerId, Object payload) {
        WebSocketSession session = playerSessionService.getSession(playerId);
        if (session != null && session.isOpen()) {
            try {
                String jsonPayload = objectMapper.writeValueAsString(payload);
                session.sendMessage(new TextMessage(jsonPayload));
            } catch (IOException e) {
                log.error("IOException at sendToPlayer (SessionId: {}).", session.getId(), e);
            }
        }
    }

    public void broadcastToPlayers(Object payload) {
        String jsonPayload;
        try {
            jsonPayload = objectMapper.writeValueAsString(payload);
        } catch (JsonProcessingException e) {
            return;
        }

        TextMessage textMessage = new TextMessage(jsonPayload);

        for (WebSocketSession session : playerSessionService.getAllPlayerSessions()) {
            if (session.isOpen()) {
                try {
                    session.sendMessage(textMessage);
                } catch (IOException e) {
                    log.error("IOException at broadcastToPlayers (SessionId: {}).", session.getId(), e);
                }
            }
        }
    }
}
