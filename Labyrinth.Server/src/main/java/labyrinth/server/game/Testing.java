package labyrinth.server.game;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.factories.BoardFactory;
import labyrinth.server.game.factories.TreasureCardFactory;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.util.GameTimer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

public class Testing {
    private static Game game;

    public static void main(String[] args) {
        ThreadPoolTaskScheduler scheduler = new ThreadPoolTaskScheduler();
        scheduler.setPoolSize(1);
        scheduler.initialize();

        IGameTimer gameTimer = new GameTimer(scheduler);
        var gameLogger = new labyrinth.server.game.services.GameLogger();
        game = new Game(gameTimer, new labyrinth.server.game.ai.SimpleAiStrategy(), gameLogger);
        simulateGameStart();
        // simulateGameMoves(1000);
    }

    public static void simulateGameStart() {
        // Lets Simulate creating a room here. Player presses something like "create
        // lobby"
        game.join("Alice");
        // More Players join the lobby
        game.join("Bob");
        game.join("Charlie");
        game.join("Dover");

        // Request to start the game is sent
        var treasureCardFactory = new TreasureCardFactory();
        var boardFactory = new BoardFactory();

        var gameConfig = new GameConfig(7, 7, 24, 1800, 3, 30);
        var board = boardFactory.createBoard(gameConfig.boardWidth(), gameConfig.boardHeight(),
                gameConfig.totalBonusCount());
        var cards = treasureCardFactory.createTreasureCards(gameConfig.treasureCardCount(), game.getPlayers().size());

        var p2 = game.getPlayers().get(1);

        var p1 = game.getPlayers().get(0);

        var p3 = game.getPlayers().get(2);

        var p4 = game.getPlayers().get(3);

         game.toggleAiForPlayer(p1);
         game.toggleAiForPlayer(p2);
         game.toggleAiForPlayer(p3);
         game.toggleAiForPlayer(p4);

        game.startGame(gameConfig, cards, board);

        // Open Debug Viewer
        LabyrinthViewer.viewSwing(game);

        int delay = 500;
        ActionListener taskPerformer = new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                LabyrinthViewer.repaintView();
            }
        };

        Timer timer = new Timer(delay, taskPerformer);
        timer.setRepeats(true);
        timer.start();
    }

    public static void simulateGameMoves(Game game, long delayMillis) {
        simulateGameStart();
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
                System.out.println("No reachable tiles for player: " + currentPlayer.getUsername());
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
            game.movePlayerToTile(positionOpt.row(), positionOpt.column(), currentPlayer);

            System.out.printf(
                    "Move %d: Player %s -> shifted row/col %d %s and moved to (%d,%d)%n",
                    move + 1,
                    currentPlayer.getUsername(),
                    shiftIndex,
                    direction,
                    positionOpt.row(),
                    positionOpt.column());

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
