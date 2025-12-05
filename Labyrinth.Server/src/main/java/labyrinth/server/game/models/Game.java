package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.abstractions.IBoardFactory;
import labyrinth.server.game.abstractions.IGame;
import labyrinth.server.game.abstractions.ITreasureCardFactory;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import lombok.Getter;
import lombok.Setter;
import org.springframework.stereotype.Component;

import java.time.OffsetDateTime;
import java.util.*;

/**
 * Represents a game room for the Labyrinth game.
 * Each game has a unique code, a board, and manages 2–4 players.
 */
@Component
@Getter
@Setter
public class Game implements IGame {

    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;

    @Setter(lombok.AccessLevel.NONE)
    private Board board;

    private final List<Player> players;
    private RoomState roomState;

    @Setter(lombok.AccessLevel.NONE)
    @Getter(lombok.AccessLevel.NONE)
    private GameConfig gameConfig;

    private final ITreasureCardFactory treasureCardFactory;
    private final IBoardFactory boardFactory;


    public Game(ITreasureCardFactory treasureCardFactory, IBoardFactory boardFactory) {
        this.treasureCardFactory = treasureCardFactory;
        this.boardFactory = boardFactory;

        this.players = new ArrayList<>();
        this.roomState = RoomState.LOBBY;
        this.board = null;

        // start with a default config
        this.gameConfig = GameConfig.getDefault();

        this.currentPlayerIndex = 0;
    }

    /**
     * Adds a player to the room.
     *
     * @param username the username of the player joining the room
     * @throws IllegalStateException if the room is full
     */
    public Player join(String username) {
        if (roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot join a game that is in progress!");
        }

        if (isFull()) {
            throw new IllegalStateException("Room is full");
        }

        if (!isUsernameAvailable(username)) {
            throw new IllegalArgumentException("Username is already taken");
        }

        Player player = new Player(UUID.randomUUID(), username);
        player.setColor(getNextColor());

        if (players.isEmpty()) {
            //first player becomes admin
            player.setAdmin(true);
        }

        player.setJoinDate(OffsetDateTime.now());
        players.add(player);
        return player;
    }

    @Override
    public void leave(Player player) {
        //TODO: handle leaving during game
        players.removeIf(p -> p.getId().equals(player.getId()));
    }

    @Override
    public Player getPlayer(UUID playerId) {
        return players.stream()
                .filter(p -> p.getId().equals(playerId))
                .findFirst()
                .orElse(null);
    }

    /**
     * Starts the game. This method could be extended to initialize
     * player positions, shuffle treasure cards, and set up the board.
     */
    @Override
    public void startGame(GameConfig gameConfig) {
        if (roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot start a game that is in progress or finished!");
        }

        if (players.size() < 2) {
            throw new IllegalStateException("At least 2 players required to start the game");
        }

        this.gameConfig = Objects.requireNonNullElseGet(gameConfig, GameConfig::getDefault);
        board = boardFactory.createBoard(gameConfig.boardWidth(), gameConfig.boardHeight());

        var cards = treasureCardFactory.createTreasureCards(gameConfig.treasureCardCount(), players.size());

        System.out.println(cards.size() + " cards have been created");

        var currentPlayerIndex = 0;
        do {
            TreasureCard card = cards.getFirst();
            board.placeRandomTreasure(card);

            Player player = players.get(currentPlayerIndex);
            player.getAssignedTreasureCards().add(card);
            currentPlayerIndex++;
            if (currentPlayerIndex >= players.size()) {
                currentPlayerIndex = 0;
            }

            cards.removeFirst();
        } while (!cards.isEmpty());

        // Assign starting positions to each player by placing them on the four corners of the board.
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            var position = gameConfig.getStartPosition(i) ;

            Tile startingTile = board.getTileAt(position);
            System.out.println(player.getUsername() + " starts on tile: " + position.row() + "/" + position.column());
            player.setCurrentTile(startingTile);
        }

        this.board.setPlayers(players);
        System.out.println("Game started in GameLobby" + " with " + players.size() + " players.");
    }

    @Override
    public Player getCurrentPlayer() {
        return players.get(currentPlayerIndex);
    }

    @Override
    public Position getCurrentPositionOfPlayer(Player player) {
        var tileOfPlayer = player.getCurrentTile();
        return board.getPositionOfTile(tileOfPlayer);
    }

    public void shift(int index, Direction direction, Set<Direction> entrances, Player player) {
        guardFor(MoveState.PLACE_TILE);
        guardFor(player);

        //TODO: consider entrances (rotation)

        boolean res = switch (direction) {
            case UP -> board.shiftColumnUp(index);
            case DOWN -> board.shiftColumnDown(index);
            case LEFT -> board.shiftRowLeft(index);
            case RIGHT -> board.shiftRowRight(index);
        };

        if (!res) {
            return;
        }
        currentMoveState = MoveState.MOVE;
    }

    public boolean movePlayerToTile(int row, int col, Player player) {
        guardFor(MoveState.MOVE);
        guardFor(player);

        var moved = board.movePlayerToTile(player, row, col);

        if (!moved) {
            return false;
        }
        currentMoveState = MoveState.PLACE_TILE;

        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        return true;
    }

    private void guardFor(MoveState moveState) {
        if (board.isFreeRoam()) {
            return;
        }

        if (this.currentMoveState != moveState) {
            throw new IllegalStateException("Illegal move state");
        }
    }

    private void guardFor(RoomState roomState) {
        if (this.roomState != roomState) {
            throw new IllegalStateException("Illegal room state");
        }
    }

    private void guardFor(Player playerToMove) {
        if (board.isFreeRoam()) {
            return;
        }
        if (!players.get(currentPlayerIndex).equals(playerToMove)) {
            throw new IllegalStateException("Illegal player. Expected " + players.get(currentPlayerIndex).getId() + " but got " + playerToMove.getId());
        }
    }

    private boolean isUsernameAvailable(String username) {
        return players.stream()
                .noneMatch(p -> p.getUsername().equalsIgnoreCase(username));
    }

    private boolean isFull() {
        return players.size() >= gameConfig.maxPlayers();
    }


    @Override
    public String toString() {
        return "Room{" +
                ", players=" + players +
                '}';
    }

    private PlayerColor getNextColor() {
        for (PlayerColor color : PlayerColor.values()) {
            boolean used = players.stream()
                    .anyMatch(p -> p.getColor() == color);
            if (!used) {
                return color;
            }
        }
        throw new IllegalStateException("No available colors left");
    }
}
