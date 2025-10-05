package labyrinth.game;

import labyrinth.game.factories.BoardFactory;
import labyrinth.game.models.*;

import java.util.ArrayList;
import java.util.List;

public class Testing {
    public static void main(String[] args) {
        debug();
    }

    public static void debug(){
        // Lets Simulate creating a room here. Player presses something like "create lobby"
        Room room = new Room("Room-123");
        Player p1 = new Player("P1", "Alice");
        room.join(p1);

        // Different settings are made in the lobby screen
        room.setMaxPlayers(4);
        room.setAmountOfTreasuresPerPlayer(7);
        room.setBoardHeight(7);
        room.setBoardWidth(7);

        // More Players join the lobby
        Player p2 = new Player("P2", "Bob");
        Player p3 = new Player("P3", "Charlie");
        Player p4 = new Player("P1", "Alice");

        room.join(p2);
        room.join(p3);
        room.join(p4);


        // Request to start the game is sent
        Board board = BoardFactory.createRandomBoard(room.getBoardWidth(), room.getBoardHeight());
        room.setBoard(board);

        // Start the game: distribute cards and set up players
        room.startGame();


        // DEBUG:
        // Show players with their assigned cards
        for (Player player : room.getPlayers()) {
            System.out.println(player.getName() + " has treasures:");
            for (TreasureCard card : player.getAssignedTreasureCards()) {
                System.out.println("  - " + card.getTreasureName());
            }
        }

        // Open Debug Viewer
        LabyrinthViewer.viewSwing(room);
    }
}
