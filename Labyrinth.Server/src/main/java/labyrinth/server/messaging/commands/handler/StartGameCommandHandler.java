package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
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
        var player = requireExistingPlayer(session);
        requireAdmin(player);

        var gameConfig = createGameConfig(payload);
        gameService.startGame(gameConfig);

        var gameStateDto = gameService.withGameReadLock(gameMapper::toGameStateDto);
        messageService.broadcastToPlayers(gameStateDto);
    }

    private GameConfig createGameConfig(StartGameCommandPayload payload) {
        var boardWidth = payload.getBoardSize().getCols();
        var boardHeight = payload.getBoardSize().getRows();
        var gameDurationInSeconds = payload.getGameDurationInSeconds();
        var treasureCardCount = payload.getTreasureCardCount();
        var totalBonusCount = payload.getTotalBonusCount();

        return new GameConfig(boardWidth, boardHeight,  treasureCardCount, gameDurationInSeconds, totalBonusCount, 30);
    }
}
