package labyrinth.server.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.io.IOException;

@Component
public class GameWebSocketHandler extends TextWebSocketHandler {

    private final ObjectMapper mapper = new ObjectMapper();

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, TextMessage message) throws Exception {
        JsonNode json = mapper.readTree(message.getPayload());

        if (!json.has("type")) {
            session.sendMessage(new TextMessage("{\"error\":\"Missing type field\"}"));
            return;
        }

        String type = json.get("type").asText();

        switch (type) {
            case "joinGame":
                handleJoinGame(session, json);
                break;

            case "move":
                handleMove(session, json);
                break;

            case "leaveGame":
                handleLeaveGame(session, json);
                break;

            default:
                session.sendMessage(new TextMessage("{\"error\":\"Unknown type: " + type + "\"}"));
        }
    }

    private void handleJoinGame(WebSocketSession session, JsonNode json) throws IOException {
        // TODO: Implementieren
        session.sendMessage(new TextMessage("handleJoinGame"));
    }

    private void handleMove(WebSocketSession session, JsonNode json) throws IOException {
        // TODO: Implementieren
        session.sendMessage(new TextMessage("handleMove"));
    }

    private void handleLeaveGame(WebSocketSession session, JsonNode json) throws IOException {
        // TODO: Implementieren
        session.sendMessage(new TextMessage("handleLeaveGame"));
    }
}
