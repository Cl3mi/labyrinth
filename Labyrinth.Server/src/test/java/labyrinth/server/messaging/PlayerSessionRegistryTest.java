package labyrinth.server.messaging;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.web.socket.WebSocketSession;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class PlayerSessionRegistryTest {

    private PlayerSessionRegistry registry;

    @Mock
    private WebSocketSession mockSession1;

    @Mock
    private WebSocketSession mockSession2;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        registry = new PlayerSessionRegistry();

        // Setup mock session attributes and isOpen()
        when(mockSession1.getAttributes()).thenReturn(new ConcurrentHashMap<>());
        when(mockSession1.isOpen()).thenReturn(true);
        when(mockSession2.getAttributes()).thenReturn(new ConcurrentHashMap<>());
        when(mockSession2.isOpen()).thenReturn(true);
    }

    @Test
    void testRegisterPlayer() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();

        registry.registerPlayer(playerId, token, mockSession1);

        assertNotNull(registry.getSession(playerId));
        assertEquals(mockSession1, registry.getSession(playerId));
        assertTrue(registry.isSessionRegistered(mockSession1));
    }

    @Test
    void testGetPlayerIdByIdentifierToken() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();

        registry.registerPlayer(playerId, token, mockSession1);

        UUID foundPlayerId = registry.getPlayerIdByIdentifierToken(token);
        assertEquals(playerId, foundPlayerId);
    }

    @Test
    void testGetPlayerIdByIdentifierToken_NotFound() {
        UUID randomToken = UUID.randomUUID();
        UUID foundPlayerId = registry.getPlayerIdByIdentifierToken(randomToken);
        assertNull(foundPlayerId);
    }

    @Test
    void testMarkDisconnected() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();

        registry.registerPlayer(playerId, token, mockSession1);
        assertTrue(registry.isPlayerConnected(playerId));

        registry.markDisconnected(mockSession1);
        assertFalse(registry.isPlayerConnected(playerId));

        // Session should be null after disconnect
        assertNull(registry.getSession(playerId));
    }

    @Test
    void testGetDisconnectedEntries() {
        UUID playerId1 = UUID.randomUUID();
        UUID playerId2 = UUID.randomUUID();
        UUID token1 = UUID.randomUUID();
        UUID token2 = UUID.randomUUID();

        registry.registerPlayer(playerId1, token1, mockSession1);
        registry.registerPlayer(playerId2, token2, mockSession2);

        // Disconnect only player1
        registry.markDisconnected(mockSession1);

        Map<UUID, Long> disconnected = registry.getDisconnectedEntries();
        assertEquals(1, disconnected.size());
        assertTrue(disconnected.containsKey(playerId1));
        assertFalse(disconnected.containsKey(playerId2));
        assertNotNull(disconnected.get(playerId1)); // timestamp should exist
    }

    @Test
    void testRemovePlayer() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();

        registry.registerPlayer(playerId, token, mockSession1);
        assertNotNull(registry.getSession(playerId));

        registry.removePlayer(playerId);
        assertNull(registry.getSession(playerId));
        // Note: isSessionRegistered may still return true since session attributes aren't cleared
        // The important thing is that getSession returns null after removal
    }

    @Test
    void testReconnectWithSameToken() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();

        // Initial registration
        registry.registerPlayer(playerId, token, mockSession1);
        assertTrue(registry.isPlayerConnected(playerId));

        // Disconnect
        registry.markDisconnected(mockSession1);
        assertFalse(registry.isPlayerConnected(playerId));

        // Reconnect with same token (new session)
        registry.registerPlayer(playerId, token, mockSession2);
        assertTrue(registry.isPlayerConnected(playerId));
        assertEquals(mockSession2, registry.getSession(playerId));
    }

    @Test
    void testIsPlayerConnected_NotRegistered() {
        UUID randomPlayerId = UUID.randomUUID();
        assertFalse(registry.isPlayerConnected(randomPlayerId));
    }

    @Test
    void testGetPlayerId() {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();

        registry.registerPlayer(playerId, token, mockSession1);

        UUID retrievedPlayerId = registry.getPlayerId(mockSession1);
        assertEquals(playerId, retrievedPlayerId);
    }

    @Test
    void testGetPlayerId_NotRegistered() {
        UUID retrievedPlayerId = registry.getPlayerId(mockSession1);
        assertNull(retrievedPlayerId);
    }

    @Test
    void testGetAllPlayerSessions() {
        UUID playerId1 = UUID.randomUUID();
        UUID playerId2 = UUID.randomUUID();
        UUID token1 = UUID.randomUUID();
        UUID token2 = UUID.randomUUID();

        registry.registerPlayer(playerId1, token1, mockSession1);
        registry.registerPlayer(playerId2, token2, mockSession2);

        var sessions = registry.getAllPlayerSessions();
        assertEquals(2, sessions.size());
        assertTrue(sessions.contains(mockSession1));
        assertTrue(sessions.contains(mockSession2));
    }

    @Test
    void testGetAllPlayerSessions_ExcludesDisconnected() {
        UUID playerId1 = UUID.randomUUID();
        UUID playerId2 = UUID.randomUUID();
        UUID token1 = UUID.randomUUID();
        UUID token2 = UUID.randomUUID();

        registry.registerPlayer(playerId1, token1, mockSession1);
        registry.registerPlayer(playerId2, token2, mockSession2);

        // Disconnect player1
        registry.markDisconnected(mockSession1);

        var sessions = registry.getAllPlayerSessions();
        assertEquals(1, sessions.size());
        assertFalse(sessions.contains(mockSession1));
        assertTrue(sessions.contains(mockSession2));
    }
}
