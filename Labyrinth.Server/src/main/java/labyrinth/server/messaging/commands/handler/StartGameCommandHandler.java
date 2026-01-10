package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.game.GameService;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.mapper.GameMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class StartGameCommandHandler extends AbstractCommandHandler<StartGameCommandPayload> {

    private final MessageService messageService;
    private final GameMapper gameMapper;

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
        System.out.println(">>> START_GAME received from session: " + session.getId());
        System.out.println(">>> Payload: " + payload);
        var player = requireExistingPlayer(session);
        requireAdmin(player);

        validateConfig(payload);

        var gameConfig = createGameConfig(payload);
        gameService.startGame(gameConfig);

        var gameStateDto = gameService.withGameReadLock(gameMapper::toGameStateDto);
        gameStateDto.setType(EventType.GAME_STARTED);

        messageService.broadcastToPlayers(gameStateDto);

        System.out.println(">>> About to broadcast GAME_STARTED");
        System.out.println(">>> GameStateDto type: " + gameStateDto.getType());

        messageService.broadcastToPlayers(gameStateDto);

        System.out.println(">>> GAME_STARTED broadcast completed");
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
        Integer turnTimeInSeconds = 30; // default
        if (payload.getAdditionalProperties() != null && payload.getAdditionalProperties().containsKey("turnTimeInSeconds")) {
            Object value = payload.getAdditionalProperties().get("turnTimeInSeconds");
            if (value instanceof Integer) {
                turnTimeInSeconds = (Integer) value;
            } else if (value instanceof Number) {
                turnTimeInSeconds = ((Number) value).intValue();
            }
        }

        System.out.println("=== GAME CONFIG ===");
        System.out.println("Board: " + boardWidth + "x" + boardHeight);
        System.out.println("Treasures to win: " + treasureCardCount);
        System.out.println("Game duration: " + gameDurationInSeconds + "s");
        System.out.println("Turn time: " + turnTimeInSeconds + "s");
        System.out.println("==================");

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
