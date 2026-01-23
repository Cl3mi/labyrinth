package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
import labyrinth.server.game.GameService;
import labyrinth.server.game.events.ReturnedToLobbyEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ReturnedToLobbyEventListener {
    private static final Logger log = LoggerFactory.getLogger(ReturnedToLobbyEventListener.class);

    private final MessageService messageService;
    private final GameService gameService;
    private final PlayerInfoMapper playerInfoMapper;

    @EventListener
    public void onEvent(ReturnedToLobbyEvent ignoreEvent) {
        try {
            broadcastLobbyState();
        } catch (Exception ex) {
            log.error("Error while handling LobbyUpdatedEvent", ex);
        }
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
}
