package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.game.abstractions.IBoardFactory;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.game.abstractions.ITreasureCardFactory;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.messaging.abstractions.IPlayerSessionRegistry;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class StartGameCommandHandler extends AbstractCommandHandler<StartGameCommandPayload> {

    private final IBoardFactory boardFactory;
    private final ITreasureCardFactory treasureCardFactory;

    public StartGameCommandHandler(IGame game,
                                   IBoardFactory boardFactory,
                                   ITreasureCardFactory treasureCardFactory,
                                   IPlayerSessionRegistry playerSessionRegistry) {
        super(game, playerSessionRegistry);

        this.boardFactory = boardFactory;
        this.treasureCardFactory = treasureCardFactory;
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

        var board = boardFactory.createBoard(gameConfig.boardWidth(), gameConfig.boardHeight());
        var cards = treasureCardFactory.createTreasureCards(gameConfig.treasureCardCount(), game.getPlayers().size());
        game.startGame(gameConfig, cards, board);
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
