package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.DisconnectCommandPayload;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
import labyrinth.contracts.models.PlayerUpdatedEventPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
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

        // Check if we're in the lobby - if so, permanently remove the player
        if (gameService.getGameState() == RoomState.LOBBY) {
            System.out.println("[Disconnect] Player " + player.getUsername() + " is leaving the lobby");

            // Remove player from game and get result
            var result = gameService.leaveLobby(player);

            if (!result.playerRemoved()) {
                System.out.println("[Disconnect] Player was not in the game");
                return;
            }

            // Unregister player session
            playerSessionRegistry.removePlayer(player.getId());
            System.out.println("[Disconnect] Unregistered player " + player.getUsername());

            if (result.shouldShutdown()) {
                System.out.println("[Disconnect] Last non-AI player left - shutting down lobby");

                // Remove all remaining players (AI players)
                var remainingPlayers = gameService.getPlayers();
                for (var p : remainingPlayers) {
                    gameService.leave(p);
                    playerSessionRegistry.removePlayer(p.getId());
                    System.out.println("[Disconnect] Removed AI player " + p.getUsername());
                }

                System.out.println("[Disconnect] Lobby shutdown complete - all players removed");
            } else {
                // Broadcast updated lobby state to remaining players
                var lobbyStatePayload = createLobbyStatePayload();
                messageService.broadcastToPlayers(lobbyStatePayload);

                if (result.newAdmin() != null) {
                    System.out.println("[Disconnect] New admin assigned: " + result.newAdmin().getUsername());
                }

                System.out.println("[Disconnect] Broadcasted lobby state update");
            }
        } else {
            // In-game disconnect: Don't remove player from game - keep them for reconnection
            // They are automatically marked as disconnected in afterConnectionClosed()
            System.out.println("[Disconnect] Player " + player.getUsername() + " disconnected during game - keeping for reconnection");
        }
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
