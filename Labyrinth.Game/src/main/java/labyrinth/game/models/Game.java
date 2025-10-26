package labyrinth.game.models;

import labyrinth.game.enums.Direction;
import labyrinth.game.enums.MoveState;
import labyrinth.game.enums.RoomState;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a game room for the Labyrinth game.
 * Each room has a unique code, a board, and manages 2–4 players.
 */
public class Game {
    //#region singleton
    private static final Game INSTANCE = new Game();

    public static Game getInstance() {
        return INSTANCE;
    }
    //#endregion

    //#region fields
    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;

    private int maxPlayers;
    private int amountOfTreasuresPerPlayer;
    private Board board;
    private final List<Player> players;
    private RoomState roomState;

    private int boardWidth;
    private int boardHeight;
    //#endregion

    //#region ctor
    /**
     * Creates a new game room.
     *
     */
    private Game() {
        this.maxPlayers = 4;
        this.amountOfTreasuresPerPlayer = 6;
        this.players = new ArrayList<>();
        this.roomState = RoomState.LOBBY;

        this.board = null;
        this.boardWidth = 7;
        this.boardHeight = 7;

        this.currentPlayerIndex = 0;
    }
    //#endregion

    //#region getters and setters
    public int getMaxPlayers() {
        return maxPlayers;
    }

    public int getAmountOfTreasuresPerPlayer() {
        return amountOfTreasuresPerPlayer;
    }

    public Board getBoard() {
        return board;
    }

    public int getCurrentPlayerIndex() {
        return currentPlayerIndex;
    }

    public List<Player> getPlayers() {
        return new ArrayList<>(players);
    }

    public void setMaxPlayers(int maxPlayers) {
        if (maxPlayers < 2) {
            throw new IllegalArgumentException("Max players can't be less than 2");
        }
        this.maxPlayers = maxPlayers;
    }

    public int getBoardWidth() {
        return boardWidth;
    }

    public void setBoardWidth(int boardWidth) {
        this.boardWidth = boardWidth;
    }

    public int getBoardHeight() {
        return boardHeight;
    }

    public void setBoardHeight(int boardHeight) {
        this.boardHeight = boardHeight;
    }

    public void setAmountOfTreasuresPerPlayer(int amountOfTreasuresPerPlayer) {
        if (amountOfTreasuresPerPlayer < 1) {
            throw new IllegalArgumentException("Amount of treasures can't be less than 1");
        }
        this.amountOfTreasuresPerPlayer = amountOfTreasuresPerPlayer;
    }

    public void setBoard(Board board) {
        this.board = board;
    }
    //#endregion

    //#region methods
    /**
     * Adds a player to the room.
     *
     * @param player the player to join
     * @throws IllegalStateException if the room is full
     */
    public void join(Player player) {
        if(roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot join a game that is in progress!");
        }

        if (players.size() >= maxPlayers) {
            throw new IllegalStateException("Room is full");
        }
        players.add(player);
    }

    /**
     * Starts the game. This method could be extended to initialize
     * player positions, shuffle treasure cards, and set up the board.
     */
    public void startGame(List<TreasureCard> cards) {
        if(roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot start a game that is in progress or finished!");
        }

        if (players.size() < 2) {
            throw new IllegalStateException("At least 2 players required to start the game");
        }

        if (cards.size() != amountOfTreasuresPerPlayer * players.size()) {
            throw new IllegalStateException("Not the right amount of treasure cards supplied. Got " + cards.size() + ", expected " + amountOfTreasuresPerPlayer * players.size());
        }

        System.out.println(cards.size() + " cards have been created");
        do {
            TreasureCard card = cards.getFirst();
            board.placeRandomTreasure(card);
            for (Player player : players) {
                if(player.getAssignedTreasureCards().size() < amountOfTreasuresPerPlayer) {
                    player.getAssignedTreasureCards().add(card);
                    break;
                }
            }
            cards.removeFirst();
        } while (!cards.isEmpty());

        // Assign starting positions to each player by placing them on the four corners of the board.
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            // Determine the corner coordinates based on player index
            int row;
            int col;
            switch (i) {
                case 0 -> { row = 0; col = 0; }
                case 1 -> { row = 0; col = boardWidth - 1; }
                case 2 -> { row = boardHeight - 1; col = boardWidth - 1; }
                case 3 -> { row = boardHeight - 1; col = 0; }
                default -> { row = 0; col = 0; }
            }
            Tile startingTile = board.getTileAt(row, col);
            System.out.println(player.getName() + " starts on tile: " + row + "/" + col);
            // Set the player's current tile to the tile at the determined coordinates
            player.setCurrentTile(startingTile);
        }

        // Register the players with the board and synchronize their tile references
        this.board.setPlayers(players);
        System.out.println("Game started in GameLobby" + " with " + players.size() + " players.");
    }


    public void shift(int index, Direction direction, Player player) {
        guardFor(MoveState.PLACE_TILE);
        guardFor(player);

        boolean res = false;
        switch (direction) {
            case UP:
                res = board.shiftColumnUp(index);
                break;
            case DOWN:
                res = board.shiftColumnDown(index);
                break;
            case LEFT:
                res = board.shiftRowLeft(index);
                break;
            case RIGHT:
                res = board.shiftRowRight(index);
                break;
        }

        if(!res)
        {
            return;
        }
        currentMoveState = MoveState.MOVE;
        board.initializeGraph();
    }

    public boolean movePlayerToTile(int row, int col, Player player) {
        guardFor(MoveState.MOVE);
        guardFor(player);

        board.movePlayerToTile(player, row, col);

        currentMoveState = MoveState.PLACE_TILE;

        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        return true;
    }

    private void guardFor(MoveState moveState) {
        if(board.getFreeRoam()) {
            return;
        }

        if(this.currentMoveState != moveState) {
            throw new IllegalStateException("Illegal move state");
        }
    }

    private void guardFor(Player playerToMove){
        if(board.getFreeRoam()) {
            return;
        }
        if(!players.get(currentPlayerIndex).equals(playerToMove)) {
            throw new IllegalStateException("Illegal player. Expected " + players.get(currentPlayerIndex).getId() + " but got " + playerToMove.getId());
        }
    }

    @Override
    public String toString() {
        return "Room{" +
                ", maxPlayers=" + maxPlayers +
                ", treasuresToCollect=" + amountOfTreasuresPerPlayer +
                ", players=" + players +
                '}';
    }
    //#endregion
}
