package labyrinth.game.models;

import labyrinth.game.enums.RoomState;
import labyrinth.game.factories.TreasureCardFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Represents a game room for the Labyrinth game.
 * Each room has a unique code, a board, and manages 2–4 players.
 */
public class Room {

    private final String roomCode;
    private int maxPlayers;
    private int amountOfTreasuresPerPlayer;
    private Board board;
    private final List<Player> players;
    private RoomState roomState;

    private int boardWidth;
    private int boardHeight;

    /**
     * Creates a new game room.
     *
     * @param roomCode          unique identifier for the room
     */
    public Room(String roomCode) {
        this.roomCode = Objects.requireNonNull(roomCode);
        this.maxPlayers = 4;
        this.amountOfTreasuresPerPlayer = 6;
        this.players = new ArrayList<>();
        this.roomState = RoomState.LOBBY;

        this.board = null;
        this.boardWidth = 7;
        this.boardHeight = 7;
    }

    public String getRoomCode() {
        return roomCode;
    }

    public int getMaxPlayers() {
        return maxPlayers;
    }

    public int getAmountOfTreasuresPerPlayer() {
        return amountOfTreasuresPerPlayer;
    }

    public Board getBoard() {
        return board;
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
    public void startGame() {
        if (players.size() < 2) {
            throw new IllegalStateException("At least 2 players required to start the game");
        }

        List<TreasureCard> cards = TreasureCardFactory.createRandomCards(amountOfTreasuresPerPlayer * players.size());

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

        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            player.getAssignedTreasureCards().clear();
            player.getAssignedTreasureCards().addAll(cards);

            Position position;

            switch (i) {
                case 0 -> position = new Position(0, 0);
                case 1 -> position = new Position(0, boardWidth - 1);
                case 2 -> position = new Position(boardHeight - 1, boardWidth - 1);
                case 3 -> position = new Position(boardHeight - 1, 0);
                default -> position = new Position(0, 0);
            }

            player.setCurrentPosition(position);
        }

        System.out.println("Game started in room " + roomCode + " with " + players.size() + " players.");
    }

    @Override
    public String toString() {
        return "Room{" +
                "roomCode='" + roomCode + '\'' +
                ", maxPlayers=" + maxPlayers +
                ", treasuresToCollect=" + amountOfTreasuresPerPlayer +
                ", players=" + players +
                '}';
    }
}
