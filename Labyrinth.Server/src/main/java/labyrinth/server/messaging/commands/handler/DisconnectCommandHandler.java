package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.*;
import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Player;
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

        if (gameService.isGameFinished()) {
            System.out.println("[DisconnectCommandHandler] Game is in progress or finished, resetting for new game");
            gameService.resetForNewGame();
        }

        var playerId = playerSessionRegistry.getPlayerId(session);
        if (playerId != null) {
            playerSessionRegistry.removePlayer(playerId);
        }

        gameService.leave(player);
        broadcastPlayerUpdated(player);

        if(gameService.getGameState() == RoomState.LOBBY) {
            broadcastLobbyState();
        }
    }

    private void broadcastLobbyState() {
        var lobbyState = new LobbyStateEventPayload();
        var playerInfoArray = gameService.getPlayers().stream()
                .map(playerInfoMapper::toDto)
                .toArray(PlayerInfo[]::new);
        lobbyState.setPlayers(playerInfoArray);
        messageService.broadcastToPlayers(lobbyState);
    }

    private void broadcastPlayerUpdated(Player player) {
        var playerUpdatedEventPayload = new PlayerUpdatedEventPayload();
        playerUpdatedEventPayload.setType(EventType.PLAYER_UPDATED);
        playerUpdatedEventPayload.setPlayer(playerInfoMapper.toDto(player));

        messageService.broadcastToPlayers(playerUpdatedEventPayload);
    }
}
