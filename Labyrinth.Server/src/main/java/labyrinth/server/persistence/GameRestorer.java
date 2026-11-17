package labyrinth.server.persistence;

import labyrinth.server.game.GameService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.SmartInitializingSingleton;
import org.springframework.stereotype.Component;

import java.time.OffsetDateTime;

/**
 * Continues the game this instance was running before a restart. Runs after all
 * beans are created but before the web server accepts connections, so clients
 * reconnecting with their identifier token find their game again.
 */
@Component
public class GameRestorer implements SmartInitializingSingleton {

    private static final Logger log = LoggerFactory.getLogger(GameRestorer.class);

    private final GameRecorder recorder;
    private final GameService gameService;
    private final PlayerSessionRegistry sessions;

    public GameRestorer(GameRecorder recorder, GameService gameService, PlayerSessionRegistry sessions) {
        this.recorder = recorder;
        this.gameService = gameService;
        this.sessions = sessions;
    }

    @Override
    public void afterSingletonsInstantiated() {
        recorder.takeOverRunningGame().ifPresent(this::restore);
    }

    private void restore(GameSnapshot snapshot) {
        try {
            var restored = GameSnapshotMapper.restore(snapshot);
            for (var player : restored.players()) {
                var token = restored.tokensByPlayerId().get(player.getId());
                if (token != null) {
                    sessions.registerDisconnected(player.getId(), token);
                } else if (!player.isBot()) {
                    // cannot reconnect without a token, so the AI plays on
                    player.setAiActive(true);
                    player.setDisconnected(true);
                }
            }
            gameService.restore(snapshot.gameId(), snapshot.version(), restored);
        } catch (Exception ex) {
            log.error("Could not restore game {}, marking it aborted: {}", snapshot.gameId(), ex.getMessage(), ex);
            forgetSessions(snapshot);
            recorder.gameAborted(snapshot.gameId(), OffsetDateTime.now());
        }
    }

    private void forgetSessions(GameSnapshot snapshot) {
        for (var p : snapshot.players()) {
            sessions.removePlayer(p.id());
        }
    }
}
