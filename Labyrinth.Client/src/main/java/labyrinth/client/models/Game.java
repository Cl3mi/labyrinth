package labyrinth.client.models;

import labyrinth.client.enums.RoomState;
import labyrinth.contracts.models.Treasure;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a game room for the Labyrinth game.
 * Each room has a unique code, a board, and manages 2–4 players.
 */

@Getter
@Setter
public class Game {
    //#region singleton
    private static final Game INSTANCE = new Game();

    public static Game getInstance() {
        return INSTANCE;
    }
    //#endregion

    //#region fields
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
    }
    //#endregion


    public void setMaxPlayers(int maxPlayers) {
        if (maxPlayers < 2) {
            throw new IllegalArgumentException("Max players can't be less than 2");
        }
        this.maxPlayers = maxPlayers;
    }


    public void setAmountOfTreasuresPerPlayer(int amountOfTreasuresPerPlayer) {
        if (amountOfTreasuresPerPlayer < 1) {
            throw new IllegalArgumentException("Amount of treasures can't be less than 1");
        }
        this.amountOfTreasuresPerPlayer = amountOfTreasuresPerPlayer;
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
    public void startGame(List<Treasure> cards) {
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
            Treasure card = cards.getFirst();
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

            Position position;

            switch (i) {
                case 0 -> position = new Position(0, 0);
                case 1 -> position = new Position(0, boardWidth - 1);
                case 2 -> position = new Position(boardHeight - 1, boardWidth - 1);
                case 3 -> position = new Position(boardHeight - 1, 0);
                default -> position = new Position(0, 0);
            }

            System.out.println(player.getName() + " gets position: " + position.getRow() + "/" + position.getColumn());
            player.setCurrentPosition(position);
        }

        this.board.setPlayers(players);
        System.out.println("Game started in GameLobby" + "with " + players.size() + " players.");
    }

    @Override
    public String toString() {
        return "Room{" +
                ", maxPlayers=" + maxPlayers +
                ", treasuresToCollect=" + amountOfTreasuresPerPlayer +
                ", players=" + players +
                '}';
    }
}
