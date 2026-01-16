package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
import labyrinth.server.game.GameService;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;


@Component
public class DisconnectCommandHandler extends AbstractCommandHandler<DisconnectCommandPayload> {

    private final MessageService messageService;
    private final PlayerInfoMapper playerInfoMapper;

    private static final Logger log = LoggerFactory.getLogger(DisconnectCommandHandler.class);

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

        // If the game is in progress or finished, reset it so a new game can be started
        // This handles both: leaving during a game AND leaving after game over
        if (gameService.isGameFinished() || gameService.isGameInProgress()) {
            log.info("[DisconnectCommandHandler] Game is in progress or finished, resetting for new game");
            gameService.resetForNewGame();
        }

        // Remove the player from the session registry using their ID
        var playerId = playerSessionRegistry.getPlayerId(session);
        if (playerId != null) {
            playerSessionRegistry.removePlayer(playerId);
        }

        // Remove the player from the game
        var leaveResult = gameService.leave(player);

        // If only bots remain in the lobby, remove all bots and reset the game
        if (leaveResult.shouldShutdown() && gameService.getRoomState() == labyrinth.server.game.enums.RoomState.LOBBY) {
            log.info("[DisconnectCommandHandler] Only bots remain in lobby, removing all bots and resetting game");
            gameService.resetForNewGame();
        }

        // Broadcast updated lobby state
        var lobbyState = new LobbyStateEventPayload();
        var playerInfoArray = gameService.getPlayers().stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);
        lobbyState.setPlayers(playerInfoArray);
        messageService.broadcastToPlayers(lobbyState);
    }
}
