package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
import labyrinth.server.game.GameService;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.events.PlayerLeftEvent;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.GameMapper;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PlayerLeftEventListener {
    private static final Logger log = LoggerFactory.getLogger(PlayerLeftEventListener.class);

    private final MessageService messageService;
    private final GameService gameService;
    private final PlayerInfoMapper playerInfoMapper;
    private final GameMapper gameMapper;

    @EventListener
    public void onEvent(PlayerLeftEvent ignoreEvent) {
        try {
            if (gameService.getRoomState() == RoomState.LOBBY) {
                broadcastLobbyState();
            }

            if (gameService.getRoomState() == RoomState.IN_GAME) {
                broadcastGameStateUpdateEvent();
            }
        } catch (Exception ex) {
            log.error("Error while handling PlayerLeftEvent", ex);
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

    private void broadcastGameStateUpdateEvent() {
        var gameState = gameService.withGameReadLock(gameMapper::toGameStateDto);
        messageService.broadcastToPlayers(gameState);
    }
}
