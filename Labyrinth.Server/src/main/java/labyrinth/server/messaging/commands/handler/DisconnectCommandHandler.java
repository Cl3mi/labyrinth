package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.PlayerUpdatedEventPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
public class DisconnectCommandHandler extends AbstractCommandHandler<DisconnectCommandPayload> {

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;

    public DisconnectCommandHandler(GameService gameService,
                                    PlayerSessionRegistry playerSessionRegistry,
                                    MessageService messageService,
                                    PlayerInfoMapper playerInfoMapper) {
        super(gameService, playerSessionRegistry);

        this.messageService = messageService;
        this.playerInfoMapper = playerInfoMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.DISCONNECT;
    }

    @Override
    public void handle(WebSocketSession session, DisconnectCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);

        // Don't remove player from game - keep them for reconnection
        // They are automatically marked as disconnected in afterConnectionClosed()
        // gameService.leave(player);

        // Note: We don't broadcast PLAYER_UPDATED or LOBBY_STATE here
        // The player stays in the lobby and can reconnect seamlessly
        // If we want to show disconnected status in UI, we would need to:
        // 1. Add a 'disconnected' flag to Player model
        // 2. Set it here and broadcast LOBBY_STATE
        // For now, keep it simple - player just temporarily disconnects
    }
}
