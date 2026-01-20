package labyrinth.server.messaging;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.Arrays;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class MessageServiceTest {

    @Mock
    private PlayerSessionRegistry playerSessionRegistry;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private WebSocketSession session;

    @InjectMocks
    private MessageService messageService;
    

    @Test
    void sendToSession_shouldSendMessage_whenSessionIsOpen() throws IOException {
        Object payload = new TestPayload("test");
        String jsonString = "{\"data\":\"test\"}";

        when(session.isOpen()).thenReturn(true);
        when(objectMapper.writeValueAsString(payload)).thenReturn(jsonString);
        
        messageService.sendToSession(session, payload);

        ArgumentCaptor<TextMessage> messageCaptor = ArgumentCaptor.forClass(TextMessage.class);
        verify(session, times(1)).sendMessage(messageCaptor.capture());
        assertEquals(jsonString, messageCaptor.getValue().getPayload());
    }

    @Test
    void sendToSession_shouldDoNothing_whenSessionIsNull() throws IOException {
        
        messageService.sendToSession(null, new Object());

        verifyNoInteractions(objectMapper);
    }

    @Test
    void sendToSession_shouldDoNothing_whenSessionIsClosed() throws IOException {
        
        when(session.isOpen()).thenReturn(false);
        
        messageService.sendToSession(session, new Object());

        verify(session, never()).sendMessage(any());
    }

    @Test
    void sendToSession_shouldHandleIOException_gracefully() throws IOException {
        Object payload = new Object();
        when(session.isOpen()).thenReturn(true);
        when(objectMapper.writeValueAsString(payload)).thenReturn("{}");
        doThrow(new IOException("Network error")).when(session).sendMessage(any(TextMessage.class));

        messageService.sendToSession(session, payload);

        verify(session).sendMessage(any(TextMessage.class));
    }


    @Test
    void sendToPlayer_shouldSendMessage_whenPlayerExistsAndSessionOpen() throws IOException {
        
        UUID playerId = UUID.randomUUID();
        Object payload = new TestPayload("player-data");
        String jsonString = "{\"data\":\"player-data\"}";

        when(playerSessionRegistry.getSession(playerId)).thenReturn(session);
        when(session.isOpen()).thenReturn(true);
        when(objectMapper.writeValueAsString(payload)).thenReturn(jsonString);

        messageService.sendToPlayer(playerId, payload);
        
        verify(session).sendMessage(any(TextMessage.class));
    }

    @Test
    void sendToPlayer_shouldDoNothing_whenPlayerHasNoSession() throws IOException {
        
        UUID playerId = UUID.randomUUID();
        when(playerSessionRegistry.getSession(playerId)).thenReturn(null);

        
        messageService.sendToPlayer(playerId, new Object());

        
        verify(objectMapper, never()).writeValueAsString(any());
    }


    @Test
    void broadcastToPlayers_shouldSendToAllOpenSessions() throws IOException {
        
        Object payload = new TestPayload("broadcast");
        String jsonString = "broadcast-json";

        WebSocketSession session1 = mock(WebSocketSession.class);
        WebSocketSession session2 = mock(WebSocketSession.class);
        WebSocketSession session3 = mock(WebSocketSession.class);

        when(objectMapper.writeValueAsString(payload)).thenReturn(jsonString);
        when(playerSessionRegistry.getAllPlayerSessions()).thenReturn(Arrays.asList(session1, session2, session3));

        when(session1.isOpen()).thenReturn(true);
        when(session2.isOpen()).thenReturn(false);
        when(session3.isOpen()).thenReturn(true);

        
        messageService.broadcastToPlayers(payload);

        verify(session1).sendMessage(any(TextMessage.class));
        verify(session2, never()).sendMessage(any());
        verify(session3).sendMessage(any(TextMessage.class));

        verify(objectMapper, times(1)).writeValueAsString(payload);
    }

    @Test
    void broadcastToPlayers_shouldStop_whenSerializationFails() throws IOException {
        
        Object payload = new Object();
        when(objectMapper.writeValueAsString(payload)).thenThrow(new JsonProcessingException("Error") {});

        
        messageService.broadcastToPlayers(payload);

        
        verify(playerSessionRegistry, never()).getAllPlayerSessions();
    }


    private static class TestPayload {
        public String data;
        public TestPayload(String data) { this.data = data; }
    }
}