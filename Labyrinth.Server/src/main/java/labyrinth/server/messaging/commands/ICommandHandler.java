package labyrinth.server.messaging.commands;

import labyrinth.contracts.models.CommandType;
import org.springframework.web.socket.WebSocketSession;

public interface ICommandHandler<T> {
    CommandType type();
    void handle(WebSocketSession session, T payload) throws Exception;
}
