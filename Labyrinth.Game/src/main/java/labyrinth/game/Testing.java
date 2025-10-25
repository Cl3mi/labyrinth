package labyrinth.game;

import labyrinth.game.abstractions.IBoardFactory;
import labyrinth.game.abstractions.ITreasureCardFactory;
import labyrinth.game.factories.BoardFactory;
import labyrinth.game.factories.TreasureCardFactory;
import labyrinth.game.models.*;

public class Testing {
    public static void main(String[] args) {
        debug();
    }

    public static void debug(){
        // Lets Simulate creating a room here. Player presses something like "create lobby"
        var game = Game.getInstance();
        ITreasureCardFactory treasureCardsFactory = new TreasureCardFactory();
        IBoardFactory boardFactory = new BoardFactory();

        var p1 = new Player("P1", "Alice");
        game.join(p1);

        // Different settings are made in the lobby screen
        game.setMaxPlayers(4);
        game.setAmountOfTreasuresPerPlayer(7);
        game.setBoardHeight(7);
        game.setBoardWidth(7);

        // More Players join the lobby
        Player p2 = new Player("P2", "Bob");
        Player p3 = new Player("P3", "Charlie");
        Player p4 = new Player("P4", "Dover");

        game.join(p2);
        game.join(p3);
        game.join(p4);

        // Request to start the game is sent
        var board = boardFactory.createBoardForGame(game);
        var treasureCards = treasureCardsFactory.createCardsForGame(game);
        game.setBoard(board);
        game.startGame(treasureCards);

        // Open Debug Viewer
        LabyrinthViewer.viewSwing(game);
    }
}
