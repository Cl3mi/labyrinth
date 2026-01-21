package labyrinth.server.messaging.commands;

import labyrinth.contracts.models.CommandType;
import labyrinth.server.exceptions.ActionErrorException;
import org.springframework.web.socket.WebSocketSession;

public interface ICommandHandler<T> {
    CommandType type();
    void handle(WebSocketSession session, T payload) throws ActionErrorException;
}
