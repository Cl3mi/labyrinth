package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.GameService;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.messaging.PlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class StartGameCommandHandler extends AbstractCommandHandler<StartGameCommandPayload> {

    public StartGameCommandHandler(GameService gameService,
                                   PlayerSessionRegistry playerSessionRegistry) {
        super(gameService, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.START_GAME;
    }

    @Override
    public void handle(WebSocketSession session, StartGameCommandPayload payload) throws ActionErrorException {
        var player = requireExistingPlayer(session);
        requireAdmin(player);

        var gameConfig = createGameConfig(payload);
        gameService.startGame(gameConfig);
    }

    private GameConfig createGameConfig(StartGameCommandPayload payload) {
        var boardWidth = payload.getBoardSize().getCols();
        var boardHeight = payload.getBoardSize().getRows();
        var gameDurationInSeconds = payload.getGameDurationInSeconds();
        var totalBonusCount = payload.getTotalBonusCount();
        var treasureCardCount = payload.getTreasureCardCount();

        if (boardWidth <= 0) {
            boardWidth = 7;
        }

        if (boardHeight <= 0) {
            boardHeight = 7;
        }

        if (treasureCardCount == null) {
            var maxTreasures = boardWidth * boardHeight - 4; //not on home position
            treasureCardCount = Math.min(maxTreasures, 24);
        }

        if (gameDurationInSeconds == null || gameDurationInSeconds <= 0) {
            gameDurationInSeconds = 3600;
        }

        if (totalBonusCount == null) {
            totalBonusCount = 0;
        }

        var turnTimeInSeconds = 120; //not available in contract
        return new GameConfig(boardWidth, boardHeight, treasureCardCount, gameDurationInSeconds, totalBonusCount, turnTimeInSeconds);
    }
}
