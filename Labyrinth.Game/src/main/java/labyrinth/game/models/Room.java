package labyrinth.game.models;

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
    private final int maxPlayers;
    private final int treasuresToCollect;
    private final Board board;
    private final List<Player> players;

    /**
     * Creates a new game room.
     *
     * @param roomCode          unique identifier for the room
     * @param maxPlayers        maximum number of players (2–4)
     * @param amountOfTreasuresPerPlayer number of treasures each player must collect
     */
    public Room(String roomCode, int maxPlayers, int amountOfTreasuresPerPlayer, Board board) {
        if (maxPlayers < 2 || maxPlayers > 4) {
            throw new IllegalArgumentException("Room must have 2 to 4 players");
        }
        this.roomCode = Objects.requireNonNull(roomCode);
        this.maxPlayers = maxPlayers;
        this.treasuresToCollect = amountOfTreasuresPerPlayer;
        this.players = new ArrayList<>();
        this.board = board;
    }

    public String getRoomCode() {
        return roomCode;
    }

    public int getMaxPlayers() {
        return maxPlayers;
    }

    public int getTreasuresToCollect() {
        return treasuresToCollect;
    }

    public Board getBoard() {
        return board;
    }

    public List<Player> getPlayers() {
        return new ArrayList<>(players);
    }

    /**
     * Adds a player to the room.
     *
     * @param player the player to join
     * @throws IllegalStateException if the room is full
     */
    public void join(Player player) {
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

        List<TreasureCard> cards = TreasureCardFactory.createRandomCards(treasuresToCollect * players.size());

        do {
            TreasureCard card = cards.getFirst();
            board.placeRandomTreasure(card);
            for (Player player : players) {
                if(player.getAssignedTreasureCards().size() < treasuresToCollect) {
                    player.getAssignedTreasureCards().add(card);
                    break;
                }
            }
            cards.removeFirst();
        } while (!cards.isEmpty());

        for (Player player : players) {
            player.getAssignedTreasureCards().clear();
            player.getAssignedTreasureCards().addAll(cards);
        }

        // TODO: Place players on starting positions on the board
        // TODO: Any other game initialization logic
        System.out.println("Game started in room " + roomCode + " with " + players.size() + " players.");
    }

    @Override
    public String toString() {
        return "Room{" +
                "roomCode='" + roomCode + '\'' +
                ", maxPlayers=" + maxPlayers +
                ", treasuresToCollect=" + treasuresToCollect +
                ", players=" + players +
                '}';
    }
}
