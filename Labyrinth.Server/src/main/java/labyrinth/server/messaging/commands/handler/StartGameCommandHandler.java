package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.ErrorCode;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
@RequiredArgsConstructor
public class StartGameCommandHandler implements ICommandHandler<StartGameCommandPayload> {
    private final IGame game;
    private final PlayerSessionRegistry playerSessionRegistry;

    @Override
    public CommandType type() {
        return CommandType.START_GAME;
    }

    @Override
    public void handle(WebSocketSession session, StartGameCommandPayload payload) throws Exception {
        var playerId = playerSessionRegistry.getPlayerId(session);

        if (playerId == null) {
            throw new ActionErrorException("Session is not connected to a player", ErrorCode.GENERAL); //TODO: error code?
        }

        var player = game.getPlayer(playerId);

        if (player == null) {
            throw new ActionErrorException("Player with ID " + playerId + " not found", ErrorCode.GENERAL); //TODO: error code?
        }

        if (player.isAdmin()) {
            throw new ActionErrorException("Only admin players can start the game", ErrorCode.NOT_ADMIN);
        }

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
