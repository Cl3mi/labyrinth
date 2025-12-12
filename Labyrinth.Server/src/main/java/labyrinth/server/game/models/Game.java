package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import lombok.Getter;
import lombok.Setter;

import java.time.OffsetDateTime;
import java.util.*;

/**
 * Represents a game room for the Labyrinth game.
 * Each game has a unique code, a board, and manages 2–4 players.
 */
@Getter
@Setter
public class Game {

    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;

    @Setter(lombok.AccessLevel.NONE)
    private Board board;

    private final List<Player> players;
    private RoomState roomState;

    private BonusTypes activeBonus;

    @Setter(lombok.AccessLevel.NONE)
    @Getter(lombok.AccessLevel.NONE)
    private GameConfig gameConfig;

    public Game() {
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
            // first player becomes admin
            player.setAdmin(true);
        }

        player.setJoinDate(OffsetDateTime.now());
        players.add(player);
        return player;
    }

    public void leave(Player player) {
        // TODO: handle leaving during game
        players.removeIf(p -> p.getId().equals(player.getId()));
    }

    public Player getPlayer(UUID playerId) {
        return players.stream()
                .filter(p -> p.getId().equals(playerId))
                .findFirst()
                .orElse(null);
    }

    /**
     * Starts the game. This method could be extended to initialize
     * player positions, shuffle treasure treasureCards, and set up the board.
     */

    public void startGame(GameConfig gameConfig, List<TreasureCard> treasureCards, Board board) {
        if (roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot start a game that is in progress or finished!");
        }

        if (players.size() < 2) {
            throw new IllegalStateException("At least 2 players required to start the game");
        }

        this.gameConfig = Objects.requireNonNullElseGet(gameConfig, GameConfig::getDefault);
        System.out.println(treasureCards.size() + " treasureCards have been created");

        var playerToAssignCardsToIndex = 0;
        do {
            TreasureCard card = treasureCards.getFirst();
            board.placeRandomTreasure(card);

            Player player = players.get(playerToAssignCardsToIndex);
            player.getAssignedTreasureCards().add(card);
            playerToAssignCardsToIndex++;
            if (playerToAssignCardsToIndex >= players.size()) {
                playerToAssignCardsToIndex = 0;
            }

            treasureCards.removeFirst();
        } while (!treasureCards.isEmpty());

        // Assign starting positions to each player by placing them on the four corners
        // of the board.
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            var position = gameConfig.getStartPosition(i);

            Tile startingTile = board.getTileAt(position);
            System.out.println(player.getUsername() + " starts on tile: " + position.row() + "/" + position.column());
            player.setCurrentTile(startingTile);
        }

        this.board = board;
        this.board.setPlayers(players);
        System.out.println("Game started in GameLobby" + " with " + players.size() + " players.");

        if (getCurrentPlayer().isAiActive()) {
            new labyrinth.server.game.ai.AdvancedAiStrategy().performTurn(this, getCurrentPlayer());
        }
    }

    public Player getCurrentPlayer() {
        return players.get(currentPlayerIndex);
    }

    public Position getCurrentPositionOfPlayer(Player player) {
        var tileOfPlayer = player.getCurrentTile();
        return board.getPositionOfTile(tileOfPlayer);
    }

    public boolean shift(int index, Direction direction, Set<Direction> entrances, Player player) {
        guardFor(MoveState.PLACE_TILE);
        guardFor(player);

        // TODO: consider entrances (rotation)

        var fixedBonusActive = activeBonus == BonusTypes.PUSH_FIXED;

        boolean res = switch (direction) {
            case UP -> board.shiftColumnUp(index, fixedBonusActive);
            case DOWN -> board.shiftColumnDown(index, fixedBonusActive);
            case LEFT -> board.shiftRowLeft(index, fixedBonusActive);
            case RIGHT -> board.shiftRowRight(index, fixedBonusActive);
        };

        if (!res) {
            return false;
        }

        if(fixedBonusActive){
            activeBonus = null;
        }

        currentMoveState = MoveState.MOVE;

        if(activeBonus == BonusTypes.PUSH_TWICE){
            currentMoveState = MoveState.PLACE_TILE;
            activeBonus = null;
        }

        return true;
    }

    public void toggleAiForPlayer(Player player) {
        player.setAiActive(!player.isAiActive());
    }

    public void useBeamBonus(int row, int col, Player player) {
        guardFor(player);
        guardFor(MoveState.PLACE_TILE);

        Tile targetTile = board.getTileMap().getForward(new Position(row, col));

        for (Player other : players) {
            if (other != player && other.getCurrentTile() == targetTile) {
                System.out.println("Cant move a player is already on the target tile!");
                return;
            }
        }

        var allowedToUse = player.useBonus(BonusTypes.SWAP);

        player.setCurrentTile(targetTile);
    }


    public void useSwapBonus(Player currentPlayer, Player targetPlayer) {
        guardFor(currentPlayer);
        guardFor(MoveState.PLACE_TILE);
        var allowedToUse = currentPlayer.useBonus(BonusTypes.SWAP);

        if(!allowedToUse){
            // return false?
            return;
        }

        var currentPlayerTile = currentPlayer.getCurrentTile();
        var targetPlayerTile = targetPlayer.getCurrentTile();

        currentPlayer.setCurrentTile(targetPlayerTile);
        targetPlayer.setCurrentTile(currentPlayerTile);

        // return true;
    }

    public void usePushTwiceBonus(Player player) {
        guardFor(player);
        var allowedToUse = player.useBonus(BonusTypes.PUSH_TWICE);

        if(!allowedToUse){
            return;
        }

        activeBonus = BonusTypes.PUSH_TWICE;
    }

    public void usePushFixedBonus(Player player) {
        guardFor(player);
        var allowedToUse = player.useBonus(BonusTypes.PUSH_FIXED);

        if(!allowedToUse){
            return;
        }

        activeBonus = BonusTypes.PUSH_FIXED;
    }

    public boolean movePlayerToTile(int row, int col, Player player) {
        guardFor(MoveState.MOVE);
        guardFor(player);

        var moved = board.movePlayerToTile(player, row, col);

        if (!moved) {
            return false;
        }

        nextPlayer();
        return true;
    }

    private void nextPlayer() {
        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        System.out.println("New Player to move: " + getCurrentPlayer().getUsername());
        currentMoveState = MoveState.PLACE_TILE;

        if (getCurrentPlayer().isAiActive()) {
            // Using AdvancedAiStrategy for smarter gameplay
            new labyrinth.server.game.ai.AdvancedAiStrategy().performTurn(this, getCurrentPlayer());
        }
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
            throw new IllegalStateException("Illegal player. Expected " + players.get(currentPlayerIndex).getId()
                    + " but got " + playerToMove.getId());
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
