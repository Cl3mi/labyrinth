package labyrinth.game;

import labyrinth.game.abstractions.IBoardFactory;
import labyrinth.game.abstractions.ITreasureCardFactory;
import labyrinth.game.enums.Direction;
import labyrinth.game.factories.BoardFactory;
import labyrinth.game.factories.TreasureCardFactory;
import labyrinth.game.models.*;

import java.util.ArrayList;

public class Testing {
    public static void main(String[] args) {
        //simulateGameStart();
        simulateGameMoves(2000);
    }

    public static void simulateGameStart(){
        // Lets Simulate creating a room here. Player presses something like "create lobby"
        var game = Game.getInstance();
        ITreasureCardFactory treasureCardsFactory = new TreasureCardFactory();
        IBoardFactory boardFactory = new BoardFactory();

        var p1 = new Player("P1", "Alice");
        game.join(p1);

        // Different settings are made in the lobby screen
        game.setGameConfig(
                new GameConfig(7, 7, 7, 4)
        );

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

    public static void simulateGameMoves(long delayMillis) {
        simulateGameStart();
        var game = Game.getInstance();
        var players = game.getPlayers();
        var board = game.getBoard();
        var random = new java.util.Random();
        board.setFreeRoam(true);

        boolean paused = false;
        int move = 0;
        while (true) {
            try {
                if (System.in.available() > 0) {
                    int key = System.in.read();
                    if (key == 's' || key == 'S') {
                        paused = !paused;
                        System.out.println(paused ? "PAUSED" : "RESUMED");
                    }
                }
            } catch (java.io.IOException e) {
                e.printStackTrace();
            }

            if (paused) {
                try {
                    Thread.sleep(200);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
                continue;
            }

            // Get current player in round-robin style
            var currentPlayer = players.get(move % players.size());

            // Perform a random shift
            int shiftIndex = random.nextInt(board.getHeight());
            Direction direction = Direction.values()[random.nextInt(Direction.values().length)];

            game.shift(shiftIndex, direction, currentPlayer);

            // Determine reachable tiles after shifting
            var reachableTiles = board.getReachableTiles(currentPlayer);
            if (reachableTiles.isEmpty()) {
                System.out.println("No reachable tiles for player: " + currentPlayer.getName());
                continue;
            }

            // Pick a random reachable tile
            var reachableList = new ArrayList<>(reachableTiles);
            var chosenTile = reachableList.get(random.nextInt(reachableList.size()));

            // Get position via BiMap safely
            var positionOpt = board.getTileMap().getBackward(chosenTile);
            if (positionOpt == null) { // adjust if getBackward returns Optional
                System.out.println("Tile has no position mapping, skipping move.");
                continue;
            }

            // Move player
            game.movePlayerToTile(positionOpt.getRow(), positionOpt.getColumn(), currentPlayer);

            System.out.printf(
                    "Move %d: Player %s -> shifted row/col %d %s and moved to (%d,%d)%n",
                    move + 1,
                    currentPlayer.getName(),
                    shiftIndex,
                    direction,
                    positionOpt.getRow(),
                    positionOpt.getColumn()
            );

            LabyrinthViewer.repaintView();
            // Wait between moves
            try {
                Thread.sleep(delayMillis);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
            move++;
        }
    }
}
