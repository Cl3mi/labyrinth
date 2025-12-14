package labyrinth.server.messaging;

import labyrinth.contracts.models.ActionErrorEventPayload;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.ServerInfoPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.messaging.commands.CommandMessageDispatcher;
import labyrinth.server.messaging.commands.CommandMessageParser;
import lombok.RequiredArgsConstructor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.time.OffsetDateTime;

@Component
@RequiredArgsConstructor
public class GameWebSocketHandler extends TextWebSocketHandler {
    private final CommandMessageParser messageParser;
    private final CommandMessageDispatcher dispatcher;
    private final PlayerSessionRegistry IPlayerSessionRegistry;
    private final MessageService messageService;


    @Override
    public void afterConnectionEstablished(@NonNull WebSocketSession session) {
        var serverInfo = new ServerInfoPayload();
        serverInfo.setType(EventType.SERVER_INFO);
        serverInfo.setServerTime(OffsetDateTime.now());
        serverInfo.setMotd("Welcome to Labyrinth Game Server!");
        serverInfo.setProtocolVersion("1.0.0");
        serverInfo.setServerVersion("");

        messageService.sendToSession(session, serverInfo);

    }

    @Override
    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull CloseStatus status) {
        IPlayerSessionRegistry.markDisconnected(session);
    }

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, @NonNull TextMessage message) {

        try {
            var envelope = messageParser.parse(message.getPayload());
            dispatcher.dispatch(session, envelope);
        } catch (ActionErrorException ex) {
            var actionError = new ActionErrorEventPayload();
            actionError.setType(EventType.ACTION_ERROR);
            actionError.setMessage(ex.getMessage());
            actionError.setErrorCode(ex.getErrorCode());

            messageService.sendToSession(session, actionError);
        } catch (Exception ex) {
            var actionError = new ActionErrorEventPayload();
            actionError.setType(EventType.ACTION_ERROR);
            actionError.setMessage(ex.getMessage());
            actionError.setErrorCode(ErrorCode.GENERAL);

            messageService.sendToSession(session, actionError);
        }
    }
}

