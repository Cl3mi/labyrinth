package labyrinth.client;

import labyrinth.client.abstractions.IBoardFactory;
import labyrinth.client.abstractions.ITreasureCardFactory;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.factories.TreasureCardFactory;
import labyrinth.client.models.Game;
import labyrinth.client.models.LabyrinthApplication;
import labyrinth.client.models.Player;

public class Application {
    public static void main(String[] args) throws Exception {
        if (args.length > 0 && "debug".equalsIgnoreCase(args[0])) {
            debug();           // alter Offline-Prototyp
        } else {
            startOnline();     // neuer Server-Client
        }
    }

    private static void startOnline() throws Exception {
        LabyrinthApplication app = new LabyrinthApplication();
        app.start();
    }

    public static void debug(){

        var game = Game.getInstance();
        ITreasureCardFactory treasureCardsFactory = new TreasureCardFactory();
        IBoardFactory boardFactory = new BoardFactory();

        var p1 = new Player("P1", "Alice");
        game.join(p1);

        game.setMaxPlayers(4);
        game.setAmountOfTreasuresPerPlayer(7);
        game.setBoardHeight(7);
        game.setBoardWidth(7);

        Player p2 = new Player("P2", "Bob");
        Player p3 = new Player("P3", "Charlie");
        Player p4 = new Player("P4", "Dover");

        game.join(p2);
        game.join(p3);
        game.join(p4);

        var board = boardFactory.createBoardForGame(game);
        var treasureCards = treasureCardsFactory.createCardsForGame(game);
        game.setBoard(board);
        game.startGame(treasureCards);

        LabyrinthViewer.viewSwing(game);
    }
}
