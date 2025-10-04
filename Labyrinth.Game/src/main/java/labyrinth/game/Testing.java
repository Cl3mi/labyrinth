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
        int width = 7;
        int height = 7;

        // Create a random board (7x7)
        Board board = BoardFactory.createRandomBoard(width, height);
        System.out.println("Board created with size: " + board.getWidth() + "x" + board.getHeight());

        // Create a room for up to 4 players, each needs 3 treasures
        Room room = new Room("Room-123", 4, 3, board);

        // Create some players with starting positions
        Player p1 = new Player("P1", "Alice", new ArrayList<>(), new Position(0, 0));
        Player p2 = new Player("P2", "Bob", new ArrayList<>(), new Position(0, width - 1));
        Player p3 = new Player("P3", "Charlie", new ArrayList<>(), new Position(height - 1, width - 1));
        Player p4 = new Player("P1", "Alice", new ArrayList<>(), new Position(height - 1, 0));

        // Add them to the room
        room.join(p1);
        room.join(p2);
        room.join(p3);
        room.join(p4);

        // Start the game: distribute cards and set up players
        room.startGame();

        // Show players with their assigned cards
        for (Player player : room.getPlayers()) {
            System.out.println(player.getName() + " has treasures:");
            for (TreasureCard card : player.getAssignedTreasureCards()) {
                System.out.println("  - " + card.getTreasureName());
            }
        }

        // Check reachable tiles for one player
        System.out.println("\nReachable tiles for Alice from (0,0):");
        var reachable = board.getReachableTiles(p1);
        System.out.println("Alice can reach " + reachable.size() + " tiles. (graph)");
        var reachable2 = board.getReachableTilesArrayBased(p1);
        System.out.println("Alice can reach " + reachable2.size() + " tiles. (array)");
        LabyrinthViewer.viewSwing(room);

    }
}
