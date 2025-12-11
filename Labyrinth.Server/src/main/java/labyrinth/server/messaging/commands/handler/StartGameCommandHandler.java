package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.BoardSize;
import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.Coordinates;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.GameBoard;
import labyrinth.contracts.models.GameStartedEventPayload;
import labyrinth.contracts.models.GameStateUpdateEventPayload;
import labyrinth.contracts.models.PlayerColor;
import labyrinth.contracts.models.PlayerState;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.TurnState;
import labyrinth.server.game.GameService;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.PlayerSessionRegistry;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.List;

@Component
public class StartGameCommandHandler implements ICommandHandler<StartGameCommandPayload> {

    private final GameService gameService;
    private final PlayerSessionRegistry playerSessionRegistry;
    private final MessageService messageService;

    public StartGameCommandHandler(GameService gameService,
                                   PlayerSessionRegistry playerSessionRegistry,
                                   MessageService messageService) {
        this.gameService = gameService;
        this.playerSessionRegistry = playerSessionRegistry;
        this.messageService = messageService;
    }

    @Override
    public CommandType type() {
        return CommandType.START_GAME;
    }

    @Override
    public void handle(WebSocketSession session, StartGameCommandPayload payload) throws Exception {

        // ---------------------------------------------------------
        // 1) Grundparameter aus Payload
        // ---------------------------------------------------------
        BoardSize bs = payload.getBoardSize();
        int rows = bs.getRows();
        int cols = bs.getCols();
        int treasuresPerPlayer = payload.getTreasureCardCount();

        System.out.println("START_GAME received: " + rows + "x" + cols +
                ", treasures=" + treasuresPerPlayer);

        // TODO: Optional: prüfen, ob der Spieler, der START_GAME sendet, Admin ist
        // (z.B. PlayerSessionRegistry -> Player für sessionId suchen und isAdmin() prüfen)

        // ---------------------------------------------------------
        // 2) Board erzeugen (ein sehr simples Dummy-Board)
        //    – später durch deine echte Game-Logik ersetzen
        // ---------------------------------------------------------
        Tile[][] tiles = new Tile[rows][cols];

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                Tile t = new Tile();

                // ganz simple Logik:
                // abwechselnd I- und L-Tiles
                if ((r + c) % 2 == 0) {
                    t.setEntrances(new Direction[]{Direction.UP, Direction.DOWN});          // I
                } else {
                    t.setEntrances(new Direction[]{Direction.UP, Direction.RIGHT});        // L
                }

                t.setIsFixed(Boolean.FALSE);
                tiles[r][c] = t;
            }
        }

        GameBoard gameBoard = new GameBoard();
        gameBoard.setRows(rows);
        gameBoard.setCols(cols);
        gameBoard.setTiles(tiles);
        gameBoard.setLastPush(null);

        // ---------------------------------------------------------
        // 3) Spieler in PlayerState[] umwandeln
        // ---------------------------------------------------------
        List<Player> lobbyPlayers = gameService.getPlayersInLobby();

        PlayerState[] playerStates = lobbyPlayers.stream()
                .map(p -> {
                    PlayerState ps = new PlayerState();
                    ps.setId(p.getId().toString());
                    ps.setName(p.getUsername());
                    ps.setColor(p.getColor());
                    ps.setIsAdmin(p.isAdmin());
                    ps.setIsReady(true);

                    // Startpositionen: vier Ecken (Beispiel)
                    Coordinates pos = new Coordinates();
                    if (p.getColor() == PlayerColor.RED) {
                        pos.setX(0);
                        pos.setY(0);
                    } else if (p.getColor() == PlayerColor.BLUE) {
                        pos.setX(cols - 1);
                        pos.setY(0);
                    } else if (p.getColor() == PlayerColor.GREEN) {
                        pos.setX(0);
                        pos.setY(rows - 1);
                    } else { // z.B. YELLOW oder andere
                        pos.setX(cols - 1);
                        pos.setY(rows - 1);
                    }

                    ps.setCurrentPosition(pos);
                    ps.setHomePosition(pos);   // Home == Start (einfaches Modell)

                    // Schätze, Achievements, Boni etc. kannst du später füllen
                    return ps;
                })
                .toArray(PlayerState[]::new);

        // ---------------------------------------------------------
        // 4) GAME_STARTED-Event schicken
        // ---------------------------------------------------------
        GameStartedEventPayload started = new GameStartedEventPayload();
        started.setType(EventType.GAME_STARTED);
        started.setInitialBoard(gameBoard);
        started.setPlayers(playerStates);

        messageService.broadcastToPlayers(started);

        // ---------------------------------------------------------
        // 5) Erstes GAME_STATE_UPDATE schicken
        // ---------------------------------------------------------
        if (playerStates.length > 0) {
            GameStateUpdateEventPayload state = new GameStateUpdateEventPayload();
            state.setType(EventType.GAME_STATE_UPDATE);
            state.setBoard(gameBoard);
            state.setPlayers(playerStates);
            state.setCurrentPlayerId(playerStates[0].getId());      // erster Spieler am Zug
            state.setCurrentTurnState(TurnState.WAITING_FOR_PUSH);  // Beispiel

            messageService.broadcastToPlayers(state);
        }
    }
}
