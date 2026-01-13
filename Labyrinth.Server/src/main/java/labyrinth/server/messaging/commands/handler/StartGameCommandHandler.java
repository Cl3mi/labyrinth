package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class StartGameCommandHandler extends AbstractCommandHandler<StartGameCommandPayload> {

    private final MessageService messageService;
    private final GameMapper gameMapper;

    private static final org.slf4j.Logger log = LoggerFactory.getLogger(StartGameCommandHandler.class);

    public StartGameCommandHandler(GameService gameService,
                                   PlayerSessionRegistry playerSessionRegistry,
                                   MessageService messageService,
                                   GameMapper gameMapper) {
        super(gameService, playerSessionRegistry);
        this.messageService = messageService;
        this.gameMapper = gameMapper;
    }

    @Override
    public CommandType type() {
        return CommandType.START_GAME;
    }

    @Override
    public void handle(WebSocketSession session, StartGameCommandPayload payload) throws Exception {
        log.info(">>> START_GAME received from session: {}", session.getId());
        log.info(">>> Payload: {}", payload);
        var player = requireExistingPlayer(session);
        requireAdmin(player);

        validateConfig(payload);

        var gameConfig = createGameConfig(payload);
        gameService.startGame(gameConfig);

        var gameStateDto = gameService.withGameReadLock(gameMapper::toGameStateDto);
        gameStateDto.setType(EventType.GAME_STARTED);

        messageService.broadcastToPlayers(gameStateDto);

        log.info(">>> About to broadcast GAME_STARTED");
        log.info(">>> GameStateDto type: {}", gameStateDto.getType());

        messageService.broadcastToPlayers(gameStateDto);

        log.info(">>> GAME_STARTED broadcast completed");
    }

    private GameConfig createGameConfig(StartGameCommandPayload payload) {
        var boardWidth = payload.getBoardSize().getCols();
        var boardHeight = payload.getBoardSize().getRows();
        var gameDurationInSeconds = payload.getGameDurationInSeconds();

        if(gameDurationInSeconds == null) {
            gameDurationInSeconds = 0;
        }

        var treasureCardCount = payload.getTreasureCardCount();
        var totalBonusCount = payload.getTotalBonusCount();

        // Extract turnTimeInSeconds from additionalProperties (not in standard Contracts)
        var turnTimeInSeconds = 30; // default
        if (payload.getAdditionalProperties() != null && payload.getAdditionalProperties().containsKey("turnTimeInSeconds")) {
            Object value = payload.getAdditionalProperties().get("turnTimeInSeconds");
            if (value instanceof Integer) {
                turnTimeInSeconds = (Integer) value;
            } else if (value instanceof Number) {
                turnTimeInSeconds = ((Number) value).intValue();
            }
        }

        log.info("=== GAME CONFIG ===");
        log.info("Board: {}x{}", boardWidth, boardHeight);
        log.info("Treasures to win: {}", treasureCardCount);
        log.info("Game duration: {}s", gameDurationInSeconds);
        log.info("Turn time: {}s", turnTimeInSeconds);
        log.info("==================");

        return new GameConfig(boardWidth, boardHeight,  treasureCardCount, gameDurationInSeconds, totalBonusCount, turnTimeInSeconds);
    }

    private void validateConfig(StartGameCommandPayload payload) {
        // Extract turnTimeInSeconds from additionalProperties for validation
        Integer turnTime = null;
        if (payload.getAdditionalProperties() != null && payload.getAdditionalProperties().containsKey("turnTimeInSeconds")) {
            Object value = payload.getAdditionalProperties().get("turnTimeInSeconds");
            if (value instanceof Integer) {
                turnTime = (Integer) value;
            } else if (value instanceof Number) {
                turnTime = ((Number) value).intValue();
            }
        }
        if (turnTime != null && (turnTime < 15 || turnTime > 120)) {
            throw new IllegalArgumentException("Turn time must be between 15 and 120 seconds");
        }

        var gameDuration = payload.getGameDurationInSeconds();
        if (gameDuration != null && (gameDuration < 600 || gameDuration > 7200)) {
            throw new IllegalArgumentException("Game duration must be between 10 and 120 minutes");
        }

        var treasures = payload.getTreasureCardCount();
        if (treasures < 1 || treasures > 24) {
            throw new IllegalArgumentException("Treasures to win must be between 1 and 24");
        }
    }
}
