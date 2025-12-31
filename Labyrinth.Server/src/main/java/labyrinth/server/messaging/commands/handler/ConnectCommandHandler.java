package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.UUID;


@Component
public class ConnectCommandHandler extends AbstractCommandHandler<ConnectCommandPayload> {

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;
    private final GameMapper gameMapper;


    public ConnectCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry,
                                 MessageService messageService,
                                 PlayerInfoMapper playerInfoMapper,
                                 GameMapper gameMapper) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.playerInfoMapper = playerInfoMapper;
        this.gameMapper = gameMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.CONNECT;
    }

    @Override
    public void handle(WebSocketSession session, ConnectCommandPayload payload) throws Exception {
        if (playerSessionRegistry.isSessionRegistered(session)) {
            throw new ActionErrorException("Session is already connected", ErrorCode.GENERAL);
        }

        if (payload.getIdentifierToken() != null) {
            var identifierToken = UUID.fromString(payload.getIdentifierToken());

            var playerId = playerSessionRegistry.getPlayerIdByIdentifierToken(identifierToken);

            // If token is invalid or player was removed, reject the reconnection attempt
            if (playerId == null) {
                throw new ActionErrorException("Reconnection token is invalid or expired", ErrorCode.PLAYER_NOT_FOUND);
            }

            var player = gameService.getPlayer(playerId);
            if (player == null) {
                // Player was removed from game but token still exists in registry - clean it up
                playerSessionRegistry.removePlayer(playerId);
                throw new ActionErrorException("Player session has expired", ErrorCode.PLAYER_NOT_FOUND);
            }

            playerSessionRegistry.registerPlayer(playerId, identifierToken, session);

            var ackPayload = createAckPayload(player.getId(), identifierToken);
            messageService.sendToPlayer(player.getId(), ackPayload);

            // Check if game is in progress - send game state instead of lobby state
            RoomState roomState = gameService.getGameState();
            if (roomState == RoomState.IN_GAME) {
                // Player is reconnecting to ongoing game - send game state
                var gameStateDto = gameService.withGameReadLock(gameMapper::toGameStateDto);
                gameStateDto.setType(EventType.GAME_STATE_UPDATE);
                messageService.sendToPlayer(player.getId(), gameStateDto);
            } else {
                // Game not started - broadcast lobby state
                var lobbyStatePayload = createLobbyStatePayload();
                messageService.broadcastToPlayers(lobbyStatePayload);
            }
            return;
        }

        var player = gameService.join(payload.getUsername());

        var identifierToken = UUID.randomUUID();
        playerSessionRegistry.registerPlayer(player.getId(), identifierToken, session);

        var ackPayload = createAckPayload(player.getId(), identifierToken);
        messageService.sendToPlayer(player.getId(), ackPayload);

        // Check if game is in progress - send appropriate state
        RoomState roomState = gameService.getGameState();
        if (roomState == RoomState.IN_GAME) {
            // Rejoining ongoing game by username - send game state
            var gameStateDto = gameService.withGameReadLock(gameMapper::toGameStateDto);
            gameStateDto.setType(EventType.GAME_STATE_UPDATE);
            messageService.sendToPlayer(player.getId(), gameStateDto);
        } else {
            // New player in lobby - broadcast lobby state
            var lobbyStatePayload = createLobbyStatePayload();
            messageService.broadcastToPlayers(lobbyStatePayload);
        }
    }

    private  ConnectAckEventPayload createAckPayload(UUID playerId, UUID identifierToken) {
        var ackPayload = new ConnectAckEventPayload();
        ackPayload.setType(EventType.CONNECT_ACK);
        ackPayload.setPlayerId(playerId.toString());
        ackPayload.setIdentifierToken(identifierToken.toString());
        return ackPayload;
    }

    private LobbyStateEventPayload createLobbyStatePayload() {
        var players = gameService.getPlayers()
                .stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);

        var lobbyStateUpdated = new LobbyStateEventPayload();
        lobbyStateUpdated.setType(EventType.LOBBY_STATE);
        lobbyStateUpdated.setPlayers(players);

        return lobbyStateUpdated;
    }
}
