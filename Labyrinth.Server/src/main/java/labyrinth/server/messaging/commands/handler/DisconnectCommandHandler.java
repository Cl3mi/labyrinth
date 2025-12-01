package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
public class DisconnectCommandHandler implements ICommandHandler<DisconnectCommandPayload> {
    private final GameService gameService;
    private final PlayerSessionRegistry playerSessionRegistry;
    private final MessageService messageService;

    public DisconnectCommandHandler(
            GameService gameService,
            PlayerSessionRegistry playerSessionRegistry, MessageService messageService) {
        this.gameService = gameService;
        this.playerSessionRegistry = playerSessionRegistry;
        this.messageService = messageService;
    }

    @Override
    public CommandType type() {
        return CommandType.DISCONNECT;
    }

    @Override
    public void handle(WebSocketSession session, DisconnectCommandPayload payload) throws Exception {
        var playerId = playerSessionRegistry.getPlayerId(session);
        gameService.disconnectPlayer(playerId);

        var gameState = gameService.getGameState();
        messageService.broadcastToPlayers(gameState);
    }
}
