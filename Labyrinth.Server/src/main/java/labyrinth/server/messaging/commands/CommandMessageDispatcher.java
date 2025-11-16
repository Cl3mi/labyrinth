package labyrinth.server.messaging.commands;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.server.exceptions.ActionErrorException;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Component
public class CommandMessageDispatcher {

    private final Map<CommandType, ICommandHandler<?>> handlers;

    public CommandMessageDispatcher(Set<ICommandHandler<?>> handlerSet) {
        this.handlers = handlerSet.stream()
                .collect(Collectors.toMap(ICommandHandler::type, h -> h));
    }

    public <T> void dispatch(WebSocketSession session, CommandEnvelope<T> envelope) throws Exception {
        CommandType type = envelope.type();

        ICommandHandler<?> rawHandler = handlers.get(type);
        if (rawHandler == null) {
            throw new ActionErrorException("Unknown command: " + type, ErrorCode.GENERAL);
        }

        @SuppressWarnings("unchecked")
        ICommandHandler<T> handler = (ICommandHandler<T>) rawHandler;

        handler.handle(session, envelope.payload());
    }

}
