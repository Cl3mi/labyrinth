package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.Optional;
import java.util.UUID;


@Component
public class ConnectCommandHandler extends AbstractCommandHandler<ConnectCommandPayload> {

    private final MessageService messageService;
    private static final Logger log = LoggerFactory.getLogger(ConnectCommandHandler.class);

    public ConnectCommandHandler(GameService gameService,
                                 PlayerSessionRegistry playerSessionRegistry,
                                 MessageService messageService) {

        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
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
            Optional<UUID> maybeToken = parseIdentifierToken(payload.getIdentifierToken());

            if (maybeToken.isPresent()) {
                handleReconnectWithToken(session, maybeToken.get(), payload);
            } else {
                if (payload.getUsername() == null || payload.getUsername().isBlank()) {
                    throw new ActionErrorException("Username not given", ErrorCode.GENERAL);
                }

                connectNewPlayer(session, payload.getUsername());
            }

            return;
        }

        connectNewPlayer(session, payload.getUsername());
    }

    private Optional<UUID> parseIdentifierToken(String tokenStr) {
        try {
            return Optional.of(UUID.fromString(tokenStr));
        } catch (IllegalArgumentException ex) {
            return Optional.empty();
        }
    }

    private void handleReconnectWithToken(WebSocketSession session, UUID identifierToken, ConnectCommandPayload payload) throws ActionErrorException {
        var playerId = playerSessionRegistry.getPlayerIdByIdentifierToken(identifierToken);

        if (playerId == null) {
            log.warn("Player with identifier token {} not found. Trying to create a new user with given username", identifierToken);

            if (payload.getUsername() == null || payload.getUsername().isBlank()) {
                throw new ActionErrorException("Reconnect not possible because no player was found with given token", ErrorCode.PLAYER_NOT_FOUND);
            }

            connectNewPlayer(session, payload.getUsername());
            return;
        }

        var player = gameService.getPlayer(playerId);
        if (player == null) {
            playerSessionRegistry.removePlayer(playerId);
            throw new ActionErrorException("Player session has expired", ErrorCode.PLAYER_NOT_FOUND);
        }

        registerAndAcknowledge(playerId, identifierToken, session);
    }


    private void connectNewPlayer(WebSocketSession session, String username) {
        var newPlayer = gameService.join(username);
        registerAndAcknowledge(newPlayer.getId(), UUID.randomUUID(), session);
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
}
