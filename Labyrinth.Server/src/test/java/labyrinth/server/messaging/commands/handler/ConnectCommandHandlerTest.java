package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.web.socket.WebSocketSession;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ConnectCommandHandlerTest {

    private ConnectCommandHandler handler;

    @Mock
    private GameService gameService;

    @Mock
    private PlayerSessionRegistry playerSessionRegistry;

    @Mock
    private MessageService messageService;

    @Mock
    private PlayerInfoMapper playerInfoMapper;

    @Mock
    private GameMapper gameMapper;

    @Mock
    private WebSocketSession mockSession;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        handler = new ConnectCommandHandler(
            gameService,
            playerSessionRegistry,
            messageService,
            playerInfoMapper,
            gameMapper
        );

        // Default mock behaviors
        when(mockSession.getAttributes()).thenReturn(new ConcurrentHashMap<>());
        when(playerSessionRegistry.isSessionRegistered(mockSession)).thenReturn(false);
        when(gameService.getGameState()).thenReturn(RoomState.LOBBY);
        when(gameService.getPlayers()).thenReturn(new ArrayList<>());
    }

    @Test
    void testConnectNewPlayer() throws Exception {
        String username = "TestPlayer";
        UUID playerId = UUID.randomUUID();
        Player player = new Player(playerId, username);

        ConnectCommandPayload payload = new ConnectCommandPayload();
        payload.setUsername(username);

        when(gameService.join(username)).thenReturn(player);

        handler.handle(mockSession, payload);

        // Verify player was joined
        verify(gameService).join(username);

        // Verify registration
        verify(playerSessionRegistry).registerPlayer(
            eq(playerId),
            any(UUID.class),  // identifierToken
            eq(mockSession)
        );

        // Verify CONNECT_ACK was sent
        ArgumentCaptor<ConnectAckEventPayload> ackCaptor =
            ArgumentCaptor.forClass(ConnectAckEventPayload.class);
        verify(messageService).sendToPlayer(eq(playerId), ackCaptor.capture());

        ConnectAckEventPayload ack = ackCaptor.getValue();
        assertEquals(EventType.CONNECT_ACK, ack.getType());
        assertEquals(playerId.toString(), ack.getPlayerId());
        assertNotNull(ack.getIdentifierToken());

        // Verify LOBBY_STATE was broadcast
        verify(messageService).broadcastToPlayers(any(LobbyStateEventPayload.class));
    }

    @Test
    void testReconnectWithToken() throws Exception {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();
        Player player = new Player(playerId, "TestPlayer");

        ConnectCommandPayload payload = new ConnectCommandPayload();
        payload.setIdentifierToken(token.toString());

        when(playerSessionRegistry.getPlayerIdByIdentifierToken(token)).thenReturn(playerId);
        when(gameService.getPlayer(playerId)).thenReturn(player);

        handler.handle(mockSession, payload);

        // Verify NO new join
        verify(gameService, never()).join(anyString());

        // Verify registration with existing playerId and token
        verify(playerSessionRegistry).registerPlayer(playerId, token, mockSession);

        // Verify CONNECT_ACK sent
        ArgumentCaptor<ConnectAckEventPayload> ackCaptor =
            ArgumentCaptor.forClass(ConnectAckEventPayload.class);
        verify(messageService).sendToPlayer(eq(playerId), ackCaptor.capture());

        ConnectAckEventPayload ack = ackCaptor.getValue();
        assertEquals(playerId.toString(), ack.getPlayerId());
        assertEquals(token.toString(), ack.getIdentifierToken());
    }

    @Test
    void testReconnectWithToken_PlayerNotFound() {
        UUID token = UUID.randomUUID();
        UUID playerId = UUID.randomUUID();

        ConnectCommandPayload payload = new ConnectCommandPayload();
        payload.setIdentifierToken(token.toString());

        when(playerSessionRegistry.getPlayerIdByIdentifierToken(token)).thenReturn(playerId);
        when(gameService.getPlayer(playerId)).thenReturn(null);

        ActionErrorException exception = assertThrows(
            ActionErrorException.class,
            () -> handler.handle(mockSession, payload)
        );

        assertEquals(ErrorCode.PLAYER_NOT_FOUND, exception.getErrorCode());
    }

    @Test
    void testReconnectDuringGame() throws Exception {
        UUID playerId = UUID.randomUUID();
        UUID token = UUID.randomUUID();
        Player player = new Player(playerId, "TestPlayer");

        ConnectCommandPayload payload = new ConnectCommandPayload();
        payload.setIdentifierToken(token.toString());

        when(playerSessionRegistry.getPlayerIdByIdentifierToken(token)).thenReturn(playerId);
        when(gameService.getPlayer(playerId)).thenReturn(player);
        when(gameService.getGameState()).thenReturn(RoomState.IN_GAME);

        GameStateEventPayload gameState = new GameStateEventPayload();
        when(gameService.withGameReadLock(any())).thenReturn(gameState);

        handler.handle(mockSession, payload);

        // Verify GAME_STATE_UPDATE sent instead of LOBBY_STATE
        ArgumentCaptor<GameStateEventPayload> gameStateCaptor =
            ArgumentCaptor.forClass(GameStateEventPayload.class);
        verify(messageService).sendToPlayer(eq(playerId), gameStateCaptor.capture());

        GameStateEventPayload sentGameState = gameStateCaptor.getValue();
        assertEquals(EventType.GAME_STATE_UPDATE, sentGameState.getType());

        // Verify LOBBY_STATE was NOT broadcast
        verify(messageService, never()).broadcastToPlayers(any(LobbyStateEventPayload.class));
    }

    @Test
    void testConnectNewPlayerDuringGame() throws Exception {
        String username = "TestPlayer";
        UUID playerId = UUID.randomUUID();
        Player player = new Player(playerId, username);

        ConnectCommandPayload payload = new ConnectCommandPayload();
        payload.setUsername(username);

        when(gameService.join(username)).thenReturn(player);
        when(gameService.getGameState()).thenReturn(RoomState.IN_GAME);

        GameStateEventPayload gameState = new GameStateEventPayload();
        when(gameService.withGameReadLock(any())).thenReturn(gameState);

        handler.handle(mockSession, payload);

        // Verify GAME_STATE_UPDATE sent
        verify(messageService).sendToPlayer(eq(playerId), any(GameStateEventPayload.class));

        // Verify LOBBY_STATE was NOT broadcast
        verify(messageService, never()).broadcastToPlayers(any(LobbyStateEventPayload.class));
    }

    @Test
    void testConnectAlreadyConnected() {
        when(playerSessionRegistry.isSessionRegistered(mockSession)).thenReturn(true);

        ConnectCommandPayload payload = new ConnectCommandPayload();
        payload.setUsername("TestPlayer");

        ActionErrorException exception = assertThrows(
            ActionErrorException.class,
            () -> handler.handle(mockSession, payload)
        );

        assertTrue(exception.getMessage().contains("already connected"));
        assertEquals(ErrorCode.GENERAL, exception.getErrorCode());
    }

    @Test
    void testCommandType() {
        assertEquals(CommandType.CONNECT, handler.type());
    }
}
