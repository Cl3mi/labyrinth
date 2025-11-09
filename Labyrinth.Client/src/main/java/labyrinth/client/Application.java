package labyrinth.client;

import labyrinth.client.abstractions.IBoardFactory;
import labyrinth.client.abstractions.ITreasureCardFactory;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.factories.TreasureCardFactory;
import labyrinth.client.models.Game;
import labyrinth.client.models.Player;

public class Application {
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
