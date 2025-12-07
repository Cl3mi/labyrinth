package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class StartGameCommandHandler extends AbstractCommandHandler<StartGameCommandPayload> {

    public StartGameCommandHandler(IGame game, IPlayerSessionRegistry playerSessionRegistry) {
        super(game, playerSessionRegistry);
    }

    @Override
    public CommandType type() {
        return CommandType.START_GAME;
    }

    @Override
    public void handle(WebSocketSession session, StartGameCommandPayload payload) throws Exception {
        var player = requireExistingPlayer(session);
        requireAdmin(player);

        var startGameOptions = createGameConfig(payload);
        game.startGame(startGameOptions);
    }

    private GameConfig createGameConfig(StartGameCommandPayload payload) {
        var boardWidth = payload.getBoardSize().getCols();
        var boardHeight = payload.getBoardSize().getRows();
        var gameDurationInSeconds = payload.getGameDurationInSeconds();
        var treasureCardCount = payload.getTreasureCardCount();
        var totalBonusCount = payload.getTotalBonusCount();

        //TODO: max player settable?
        return new GameConfig(boardWidth, boardHeight, 4, treasureCardCount, gameDurationInSeconds, totalBonusCount);
    }
}
