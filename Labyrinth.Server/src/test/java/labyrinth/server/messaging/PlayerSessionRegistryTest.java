package labyrinth.server.messaging;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.socket.WebSocketSession;


import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class PlayerSessionRegistryTest {

    private PlayerSessionRegistry registry;

    @Mock
    private WebSocketSession session;

    private Map<String, Object> sessionAttributes;

    @BeforeEach
    void setUp() {
        registry = new PlayerSessionRegistry();
        sessionAttributes = new HashMap<>();
    }

    @Test
    void registerPlayer_shouldStoreSessionAndSetAttribute() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();
        when(session.getAttributes()).thenReturn(sessionAttributes);
        when(session.isOpen()).thenReturn(true);

        registry.registerPlayer(playerId, token, session);

        assertTrue(registry.isPlayerConnected(playerId));
        assertEquals(playerId, sessionAttributes.get("PLAYER_ID"));
        assertEquals(session, registry.getSession(playerId));
    }

    @Test
    void removePlayer_shouldRemoveFromRegistry() {
        UUID playerId = UUID.randomUUID();
        when(session.getAttributes()).thenReturn(sessionAttributes);
        registry.registerPlayer(playerId, UUID.randomUUID(), session);

        registry.removePlayer(playerId);

        assertFalse(registry.isPlayerConnected(playerId));
        assertNull(registry.getSession(playerId));
    }

    @Test
    void markDisconnected_shouldUpdatePlayerStatus() {
        UUID playerId = UUID.randomUUID();
        when(session.getAttributes()).thenReturn(sessionAttributes);

        // Register first
        registry.registerPlayer(playerId, UUID.randomUUID(), session);

        // Mark disconnected
        registry.markDisconnected(session);

        assertFalse(registry.isPlayerConnected(playerId));
    }

    @Test
    void markDisconnected_shouldDoNothing_whenSessionNotRegistered() {
        when(session.getAttributes()).thenReturn(sessionAttributes);
        // No player ID in attributes

        registry.markDisconnected(session);

        // Should not throw exception
        assertTrue(registry.getDisconnectedEntries().isEmpty());
    }

    @Test
    void getPlayerIdByIdentifierToken_shouldReturnCorrectId() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();
        when(session.getAttributes()).thenReturn(sessionAttributes);

        registry.registerPlayer(playerId, token, session);

        assertEquals(playerId, registry.getPlayerIdByIdentifierToken(token));
    }

    @Test
    void getPlayerIdByIdentifierToken_shouldReturnNull_whenTokenUnknown() {
        assertNull(registry.getPlayerIdByIdentifierToken(UUID.randomUUID()));
    }

    @Test
    void getSession_shouldReturnSession_whenOpen() {
        UUID playerId = UUID.randomUUID();
        when(session.getAttributes()).thenReturn(sessionAttributes);
        when(session.isOpen()).thenReturn(true);

        registry.registerPlayer(playerId, UUID.randomUUID(), session);

        WebSocketSession result = registry.getSession(playerId);
        assertNotNull(result);
        assertEquals(session, result);
    }

    @Test
    void getSession_shouldMarkDisconnected_whenUnderlyingSessionClosed() {
        UUID playerId = UUID.randomUUID();
        when(session.getAttributes()).thenReturn(sessionAttributes);

        registry.registerPlayer(playerId, UUID.randomUUID(), session);

        // Simulate connection drop at socket level
        when(session.isOpen()).thenReturn(false);

        WebSocketSession result = registry.getSession(playerId);

        assertNull(result);
        assertFalse(registry.isPlayerConnected(playerId));
    }

    @Test
    void getDisconnectedEntries_shouldReturnOnlyDisconnectedPlayers() {
        UUID p1 = UUID.randomUUID();
        UUID p2 = UUID.randomUUID();

        WebSocketSession s1 = mock(WebSocketSession.class);
        WebSocketSession s2 = mock(WebSocketSession.class);
        Map<String, Object> attr1 = new HashMap<>();
        Map<String, Object> attr2 = new HashMap<>();

        when(s1.getAttributes()).thenReturn(attr1);
        when(s2.getAttributes()).thenReturn(attr2);

        registry.registerPlayer(p1, UUID.randomUUID(), s1);
        registry.registerPlayer(p2, UUID.randomUUID(), s2);

        registry.markDisconnected(s1);

        Map<UUID, Long> disconnected = registry.getDisconnectedEntries();

        assertTrue(disconnected.containsKey(p1));
        assertFalse(disconnected.containsKey(p2));
    }

    @Test
    void getAllPlayerSessions_shouldReturnOnlyOpenSessions() {
        WebSocketSession openSession = mock(WebSocketSession.class);
        WebSocketSession closedSession = mock(WebSocketSession.class);

        when(openSession.getAttributes()).thenReturn(new HashMap<>());
        when(closedSession.getAttributes()).thenReturn(new HashMap<>());

        when(openSession.isOpen()).thenReturn(true);
        // closedSession.isOpen() defaults to false or we can explicitly set it
        when(closedSession.isOpen()).thenReturn(false);

        registry.registerPlayer(UUID.randomUUID(), UUID.randomUUID(), openSession);
        registry.registerPlayer(UUID.randomUUID(), UUID.randomUUID(), closedSession);

        Collection<WebSocketSession> sessions = registry.getAllPlayerSessions();

        assertEquals(1, sessions.size());
        assertTrue(sessions.contains(openSession));
    }

    @Test
    void getPlayerId_shouldReturnIdFromAttributes() {
        UUID playerId = UUID.randomUUID();
        sessionAttributes.put("PLAYER_ID", playerId);
        when(session.getAttributes()).thenReturn(sessionAttributes);

        assertEquals(playerId, registry.getPlayerId(session));
    }

    @Test
    void isSessionRegistered_shouldReturnTrueIfIdPresent() {
        sessionAttributes.put("PLAYER_ID", UUID.randomUUID());
        when(session.getAttributes()).thenReturn(sessionAttributes);

        assertTrue(registry.isSessionRegistered(session));
    }
}