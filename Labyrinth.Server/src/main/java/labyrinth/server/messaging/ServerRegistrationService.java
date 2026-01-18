package labyrinth.server.messaging;

import jakarta.annotation.PreDestroy;
import labyrinth.managementclient.api.ServersApi;
import labyrinth.managementclient.model.GameServer;
import labyrinth.managementclient.model.GameServerRegistration;
import labyrinth.managementclient.model.GameServerUpdate;
import labyrinth.server.game.GameService;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.concurrent.atomic.AtomicBoolean;

@Service
@RequiredArgsConstructor
public class ServerRegistrationService {

    private static final Logger log = LoggerFactory.getLogger(ServerRegistrationService.class);

    private final ServersApi serversApi;
    private final GameService gameService;
    private volatile GameServer gameServer;
    private final AtomicBoolean shuttingDown = new AtomicBoolean(false);

    @Value("${server.name:Random Server}")
    private String serverName;

    @Value("${server.publichost:localhost}")
    private String publicHost;

    @Value("${server.port:8080}")
    private int serverPort;

    private final AtomicBoolean isRegistering = new AtomicBoolean(false);

    private int tries;

    @EventListener(ApplicationReadyEvent.class)
    public void registerServer() {
        if (shuttingDown.get()) {
            log.info("Skipping registerServer: shutdown in progress");
            return;
        }

        if (gameServer != null) {
            return;
        }

        if (!isRegistering.compareAndSet(false, true)) {
            log.info("Registration already in progress, skipping duplicate attempt.");
            return;
        }

        try {
            String fullUri = String.format("ws://%s:%d", publicHost, serverPort);

            var gameServerRegistration = new GameServerRegistration();
            gameServerRegistration.setMaxPlayers(gameService.getMaxPlayers());
            gameServerRegistration.setName(serverName);
            gameServerRegistration.setUri(fullUri);

            gameServer = serversApi.createServer(gameServerRegistration);
            log.info("Server successfully registered. Assigned Server-ID: {}", gameServer.getId());

        } catch (Exception e) {
            log.error("Failed to register server. Is the Management API reachable?", e);

        } finally {
            isRegistering.set(false);
        }
    }

    @PreDestroy
    public void shutdown() {
        shuttingDown.set(true);
        unregisterServer();
    }


    public void unregisterServer() {
        if (gameServer != null) {
            try {
                log.info("Deregistering server (ID: {})...", gameServer.getId());
                serversApi.deleteServer(gameServer.getId());
                log.info("Server deregistered successfully.");
            } catch (Exception e) {
                log.warn("Could not deregister server. It might already be gone.", e);
            } finally {
                gameServer = null;
            }
        } else {
            log.info("No Server-ID present, skipping deregistration.");
        }
    }

    @Scheduled(fixedRate = 1000)
    public void sendHeartbeat() {

        if (shuttingDown.get()) {
            log.debug("Not attempting to send heartbeat during shutdown");
            return;
        }

        var currentServer = gameServer;

        if (currentServer != null) {
            try {
                var gameServerUpdate = getGameServerUpdate();
                serversApi.updateServer(currentServer.getId(), gameServerUpdate);
            } catch (Exception e) {
                log.error("Failed to send heartbeat to Management API.", e);
                tries++;

                if(tries > 5) {
                    log.error("Failed do send heartbeat to Management 5 times. Registering again...");
                    tries = 0;
                    gameServer = null;
                    registerServer();
                }
            }
        } else {
            log.warn("Server not registered yet, retrying...");
            registerServer();
        }

    }

    private @NonNull GameServerUpdate getGameServerUpdate() {
        var gameServerUpdate = new GameServerUpdate();

        var gameState = gameService.getGameState();

        switch (gameState) {
            case LOBBY -> gameServerUpdate.setStatus(GameServerUpdate.StatusEnum.LOBBY);
            case IN_GAME -> gameServerUpdate.setStatus(GameServerUpdate.StatusEnum.IN_GAME);
            case FINISHED -> gameServerUpdate.setStatus(GameServerUpdate.StatusEnum.FINISHED);
        }

        gameServerUpdate.setCurrentPlayerCount(gameService.getPlayers().size());
        return gameServerUpdate;
    }
}
