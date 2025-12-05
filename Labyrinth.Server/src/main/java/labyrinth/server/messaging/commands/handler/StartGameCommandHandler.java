package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class StartGameCommandHandler implements ICommandHandler<StartGameCommandPayload> {
    private final IGame game;

    @Override
    public CommandType type() {
        return CommandType.START_GAME;
    }

    @Override
    public void handle(WebSocketSession session, StartGameCommandPayload payload) throws Exception {
        var boardWidth = payload.getBoardSize().getCols();
        var boardHeight = payload.getBoardSize().getRows();
        var gameDurationInSeconds = payload.getGameDurationInSeconds();
        var treasureCardCount = payload.getTreasureCardCount();
        var totalBonusCount = payload.getTotalBonusCount();

        //TODO: max player settable?
        var startGameOptions = new GameConfig(boardWidth, boardHeight, 4, treasureCardCount, gameDurationInSeconds, totalBonusCount);
        game.startGame(startGameOptions);
    }
}
