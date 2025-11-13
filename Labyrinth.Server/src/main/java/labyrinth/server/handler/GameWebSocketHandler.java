package labyrinth.server.handler;

import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.ActionErrorEventPayload;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.ServerInfoPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.messaging.CommandMessageDispatcher;
import labyrinth.server.messaging.CommandMessageParser;
import labyrinth.server.messaging.OutboundMessageSender;
import labyrinth.server.messaging.SessionManager;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.time.OffsetDateTime;

@Component
public class GameWebSocketHandler extends TextWebSocketHandler {
    private final ObjectMapper mapper = new ObjectMapper();

    private final CommandMessageParser messageParser;
    private final CommandMessageDispatcher dispatcher;
    private final SessionManager sessionManager;
    private final OutboundMessageSender outboundMessageSender;

    public GameWebSocketHandler(CommandMessageParser messageParser,
                                CommandMessageDispatcher dispatcher,
                                SessionManager sessionManager,
                                OutboundMessageSender outboundMessageSender) {
        this.messageParser = messageParser;
        this.dispatcher = dispatcher;
        this.sessionManager = sessionManager;
        this.outboundMessageSender = outboundMessageSender;
    }

    @Override
    public void afterConnectionEstablished(@NonNull WebSocketSession session) {
        sessionManager.addSession(session);

        var serverInfo = new ServerInfoPayload();
        serverInfo.setType(EventType.SERVER_INFO);
        serverInfo.setServerTime(OffsetDateTime.now());
        serverInfo.setMotd("Welcome to Labyrinth Game Server!");
        serverInfo.setProtocolVersion("1.0.0");
        serverInfo.setServerVersion("");

        outboundMessageSender.send(session, serverInfo);

    }

    @Override
    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull CloseStatus status) {
        sessionManager.removeSession(session);
    }

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, @NonNull TextMessage message) throws Exception {

        try {
            var parsedMessage = messageParser.parse(message.getPayload());
            dispatcher.dispatch(parsedMessage.type(), parsedMessage.payload());
        } catch (ActionErrorException ex) {
            var actionError = new ActionErrorEventPayload();
            actionError.setType(EventType.ACTION_ERROR);
            actionError.setMessage(ex.getMessage());
            actionError.setErrorCode(ex.getErrorCode());

            outboundMessageSender.send(session, actionError);
        }
        catch(Exception ex) {
            var actionError = new ActionErrorEventPayload();
            actionError.setType(EventType.ACTION_ERROR);
            actionError.setMessage(ex.getMessage());
            actionError.setErrorCode(ErrorCode.GENERAL);

            outboundMessageSender.send(session, actionError);
        }
    }
}

