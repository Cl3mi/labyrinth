package labyrinth.server.messaging.commands;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.server.exceptions.ActionErrorException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.socket.WebSocketSession;

import java.util.Collections;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CommandMessageDispatcherTest {

    private CommandMessageDispatcher dispatcher;

    @Mock
    private ICommandHandler<String> connectHandler;

    @Mock
    private ICommandHandler<Integer> moveHandler;

    @Mock
    private WebSocketSession session;

    @Test
    void dispatch_shouldDelegateToRegisteredHandler() throws Exception {
        when(connectHandler.type()).thenReturn(CommandType.CONNECT);
        dispatcher = new CommandMessageDispatcher(Set.of(connectHandler));

        String payload = "player-name";
        CommandEnvelope<String> envelope = new CommandEnvelope<>(CommandType.CONNECT, payload);

        dispatcher.dispatch(session, envelope);

        verify(connectHandler).handle(session, payload);
    }

    @Test
    void dispatch_shouldThrowException_whenCommandTypeUnknown() {
        dispatcher = new CommandMessageDispatcher(Collections.emptySet());

        CommandEnvelope<String> envelope = new CommandEnvelope<>(CommandType.START_GAME, "data");

        ActionErrorException ex = assertThrows(ActionErrorException.class, () ->
                dispatcher.dispatch(session, envelope)
        );

        assertEquals(ErrorCode.GENERAL, ex.getErrorCode());
        assertEquals("Unknown command: START_GAME", ex.getMessage());
    }

    @Test
    void dispatch_shouldRouteToCorrectHandler_whenMultipleRegistered() throws Exception {
        when(connectHandler.type()).thenReturn(CommandType.CONNECT);
        when(moveHandler.type()).thenReturn(CommandType.MOVE_PAWN);

        dispatcher = new CommandMessageDispatcher(Set.of(connectHandler, moveHandler));

        // Test first handler
        CommandEnvelope<String> connectEnv = new CommandEnvelope<>(CommandType.CONNECT, "Login");
        dispatcher.dispatch(session, connectEnv);
        verify(connectHandler).handle(session, "Login");

        // Test second handler
        CommandEnvelope<Integer> moveEnv = new CommandEnvelope<>(CommandType.MOVE_PAWN, 5);
        dispatcher.dispatch(session, moveEnv);
        verify(moveHandler).handle(session, 5);
    }

    @Test
    void dispatch_shouldPropagateExceptionsFromHandler() throws Exception {
        when(connectHandler.type()).thenReturn(CommandType.CONNECT);
        dispatcher = new CommandMessageDispatcher(Set.of(connectHandler));

        CommandEnvelope<String> envelope = new CommandEnvelope<>(CommandType.CONNECT, "fail");

        doThrow(new RuntimeException("Handler failed")).when(connectHandler).handle(any(), any());

        assertThrows(RuntimeException.class, () ->
                dispatcher.dispatch(session, envelope)
        );
    }
}