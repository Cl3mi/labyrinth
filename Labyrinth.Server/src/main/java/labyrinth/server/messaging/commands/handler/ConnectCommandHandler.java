package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.exceptions.GameAlreadyStartedException;
import labyrinth.server.exceptions.UsernameTakenException;
import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import labyrinth.server.messaging.mapper.TreasureMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.Optional;
import java.util.UUID;


@Component
public class ConnectCommandHandler extends AbstractCommandHandler<ConnectCommandPayload> {

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;
    private final GameMapper gameMapper;
    private final TreasureMapper treasureMapper;
    private static final Logger log = LoggerFactory.getLogger(ConnectCommandHandler.class);

    public ConnectCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry,
                                 MessageService messageService,
                                 PlayerInfoMapper playerInfoMapper,
                                 GameMapper gameMapper,
                                 TreasureMapper treasureMapper) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.playerInfoMapper = playerInfoMapper;
        this.gameMapper = gameMapper;
        this.treasureMapper = treasureMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.CONNECT;
    }

    @Override
    public void handle(WebSocketSession session, ConnectCommandPayload payload) throws ActionErrorException {
        if (playerSessionRegistry.isSessionRegistered(session)) {
            throw new ActionErrorException("Session is already connected", ErrorCode.GENERAL);
        }

        Optional<UUID> maybeToken = parseIdentifierToken(payload.getIdentifierToken());
        Player reconnectedPlayer = null;

        if (maybeToken.isPresent()) {
            reconnectedPlayer = handleReconnection(session, maybeToken.get(), payload);
        } else {
            connectNewPlayer(session, payload.getUsername());
        }

        if (gameService.getGameState() == RoomState.LOBBY) {
            broadcastLobbyState();
        } else {
            broadcastGameStateUpdateEvent();
            if (reconnectedPlayer != null) {
                sendPlayerUpdatedEvent(reconnectedPlayer);
                sendNextTreasureEvent(reconnectedPlayer);
            }
        }
    }

    private Optional<UUID> parseIdentifierToken(String tokenStr) {
        try {

            if (tokenStr == null || tokenStr.isBlank()) {
                return Optional.empty();
            }

            return Optional.of(UUID.fromString(tokenStr));
        } catch (IllegalArgumentException ex) {
            return Optional.empty();
        }
    }

    private Player handleReconnection(WebSocketSession session, UUID identifierToken, ConnectCommandPayload payload) throws ActionErrorException {

        var playerId = playerSessionRegistry.getPlayerIdByIdentifierToken(identifierToken);

        if (playerId == null) {
            log.warn("Player with identifier token {} not found. Trying to create a new user with given username", identifierToken);

            if (payload.getUsername() == null || payload.getUsername().isBlank()) {
                throw new ActionErrorException("Reconnect not possible because no player was found with given token", ErrorCode.PLAYER_NOT_FOUND);
            }

            connectNewPlayer(session, payload.getUsername());
            return null;
        }

        var player = gameService.getPlayer(playerId);
        if (player == null) {
            playerSessionRegistry.removePlayer(playerId);
            throw new ActionErrorException("Player session has expired", ErrorCode.PLAYER_NOT_FOUND);
        }

        // Clear disconnected flag on successful reconnection
        player.setDisconnected(false);
        registerAndAcknowledge(playerId, identifierToken, session);
        return player;
    }


    private void connectNewPlayer(WebSocketSession session, String username) throws ActionErrorException {
        try {
            var newPlayer = gameService.join(username);
            registerAndAcknowledge(newPlayer.getId(), UUID.randomUUID(), session);
        } catch (UsernameTakenException e) {
            throw new ActionErrorException("Username already taken", ErrorCode.USERNAME_TAKEN);
        } catch (GameAlreadyStartedException e) {
            throw new ActionErrorException("Game already started", ErrorCode.GAME_ALREADY_STARTED);
        }
    }

    private void registerAndAcknowledge(UUID playerId, UUID identifierToken, WebSocketSession session) {
        playerSessionRegistry.registerPlayer(playerId, identifierToken, session);
        sendConnectionAcknowledgement(playerId, identifierToken);
    }

    private void sendConnectionAcknowledgement(UUID playerId, UUID identifierToken) {
        var ackPayload = createAckPayload(playerId, identifierToken);
        messageService.sendToPlayer(playerId, ackPayload);
    }


    private ConnectAckEventPayload createAckPayload(UUID playerId, UUID identifierToken) {
        var ackPayload = new ConnectAckEventPayload();
        ackPayload.setType(EventType.CONNECT_ACK);
        ackPayload.setPlayerId(playerId.toString());
        ackPayload.setIdentifierToken(identifierToken.toString());
        return ackPayload;
    }


    private void broadcastLobbyState() {
        var players = gameService.getPlayers()
                .stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);

        var lobbyStateUpdated = new LobbyStateEventPayload();
        lobbyStateUpdated.setType(EventType.LOBBY_STATE);
        lobbyStateUpdated.setPlayers(players);

        messageService.broadcastToPlayers(lobbyStateUpdated);
    }

    private void broadcastGameStateUpdateEvent() {
        var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
        messageService.broadcastToPlayers(gameState);
    }

    private void sendPlayerUpdatedEvent(Player player) {
        var payload = new PlayerUpdatedEventPayload();
        payload.setType(EventType.PLAYER_UPDATED);
        payload.setPlayer(playerInfoMapper.toDto(player));
        messageService.broadcastToPlayers(payload);
    }

    private void sendNextTreasureEvent(Player player) {
        var treasureCard = player.getCurrentTreasureCard();
        if (treasureCard == null) {
            return;
        }

        var payload = new NextTreasureCardEventPayload();
        payload.setType(EventType.NEXT_TREASURE);
        payload.setTreasure(treasureMapper.toDto(treasureCard));
        messageService.sendToPlayer(player.getId(), payload);
    }
}
