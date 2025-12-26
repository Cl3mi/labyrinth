package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

/**
 * Represents a game room for the Labyrinth game.
 * Each game has a unique code, a board, and manages 2–4 players.
 */
@Getter
@Setter
public class Game {

    private final int MAX_PLAYERS = 4;

    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;

    private IGameTimer nextTurnTimer;

    @Setter(lombok.AccessLevel.NONE)
    private Board board;

    private final List<Player> players;
    private RoomState roomState;

    private BonusTypes activeBonus;

    @Setter(AccessLevel.NONE)
    @Getter(AccessLevel.NONE)
    private GameConfig gameConfig;

    @Setter(AccessLevel.PRIVATE)
    @Getter(AccessLevel.NONE)
    private OffsetDateTime gameStartTime;

    public Game(IGameTimer nextTurnTimer) {
        this.nextTurnTimer = nextTurnTimer;
        this.players = new ArrayList<>();
        this.roomState = RoomState.LOBBY;
        this.board = null;
        this.gameConfig = GameConfig.getDefault();
        this.currentPlayerIndex = 0;
    }

    /**
     * Adds a player to the room, or rejoins if player with same username exists.
     *
     * @param username the username of the player joining the room
     * @throws IllegalStateException if the room is full (when adding new player)
     */
    public Player join(String username) {
        // Allow rejoining by username if player already exists (for reconnection fallback)
        Player existingPlayer = players.stream()
                .filter(p -> p.getUsername().equals(username))
                .findFirst()
                .orElse(null);

        if (existingPlayer != null) {
            // Player with this username exists - allow rejoining
            System.out.println("Player '" + username + "' rejoining by username (reconnection fallback)");
            return existingPlayer;
        }

        // New player joining
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

    private void addAiPlayer() {
        Player player = new Player(UUID.randomUUID(), "Bot " + (players.size() + 1));
        player.setColor(getNextColor());
        player.setAiActive(true);

        player.setJoinDate(OffsetDateTime.now());
        players.add(player);
    }

    public void leave(Player player) {
        boolean wasAdmin = player.isAdmin();

        // Remove player from game
        players.removeIf(p -> p.getId().equals(player.getId()));

        // If admin left and players remain, reassign admin to first remaining player
        if (wasAdmin && !players.isEmpty()) {
            // Clear all admin flags first (defensive programming)
            players.forEach(p -> p.setAdmin(false));

            // Assign admin to first non-AI player if possible, otherwise first player
            Player newAdmin = players.stream()
                .filter(p -> !p.isAiActive())
                .findFirst()
                .orElse(players.get(0));

            newAdmin.setAdmin(true);
            System.out.println("Admin reassigned from " + player.getUsername() +
                              " to " + newAdmin.getUsername());
        }

        // TODO: handle leaving during active game (future enhancement)
        // - If game is IN_GAME state, may need different behavior
        // - Consider making AI take over, or ending game if too few players
    }

    /**
     * Resets the game to lobby state after game ends.
     * Removes all AI players and reassigns admin to the first remaining player.
     */
    public void resetToLobby() {
        // Remove all AI players
        players.removeIf(Player::isAiActive);

        // Reset room state to lobby
        roomState = RoomState.LOBBY;

        // Reset all players' admin status
        players.forEach(p -> p.setAdmin(false));

        // Assign admin to first player if any remain
        if (!players.isEmpty()) {
            players.get(0).setAdmin(true);
        }

        // Clear board and game state
        board = null;
        currentPlayerIndex = 0;
        currentMoveState = MoveState.PLACE_TILE;
        activeBonus = null;
    }

    public Player getPlayer(UUID playerId) {
        return players.stream()
                .filter(p -> p.getId().equals(playerId))
                .findFirst()
                .orElse(null);
    }

    public List<Player> getPlayers() {
        return players;
    }

    public RoomState getRoomState() {
        return roomState;
    }

    /**
     * Starts the game. This method could be extended to initialize
     * player positions, shuffle treasure treasureCards, and set up the board.
     */

    public void startGame(GameConfig gameConfig, List<TreasureCard> treasureCards, Board board) {
        if (roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot start a game that is in progress or finished!");
        }

        if (players.isEmpty()) {
            throw new IllegalStateException("At least 1 player is required to start the game");
        }

        while (players.size() < MAX_PLAYERS) {
            addAiPlayer();
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
            player.setHomeTile(startingTile);
            player.setCurrentTile(startingTile);
        }

        this.board = board;
        this.board.setPlayers(players);

        // Set room state to IN_GAME
        roomState = RoomState.IN_GAME;

        System.out.println("Game started in GameLobby" + " with " + players.size() + " players.");

        if (getCurrentPlayer().isAiActive()) {
            new labyrinth.server.game.ai.SimpleAiStrategy().performTurn(this, getCurrentPlayer());
        }

        gameStartTime = OffsetDateTime.now();
    }

    public Player getCurrentPlayer() {
        return players.get(currentPlayerIndex);
    }

    public Position getCurrentPositionOfPlayer(Player player) {
        var tileOfPlayer = player.getCurrentTile();
        return board.getPositionOfTile(tileOfPlayer);
    }

    public void rotateExtraTileClockwise(Player player) {
        //TODO: check if correct
        guardFor(MoveState.PLACE_TILE);
        guardFor(player);

        var board = getBoard();
        board.getExtraTile().rotate();
    }

    public boolean shift(int index, Direction direction, Player player) {
        guardFor(MoveState.PLACE_TILE);
        guardFor(player);

        var fixedBonusActive = activeBonus == BonusTypes.PUSH_FIXED;

        if (fixedBonusActive) {
            //TODO: do not allow to shift border rows/columns because it would move home tiles
        }

        boolean res = switch (direction) {
            case UP -> board.shiftColumnUp(index, fixedBonusActive);
            case DOWN -> board.shiftColumnDown(index, fixedBonusActive);
            case LEFT -> board.shiftRowLeft(index, fixedBonusActive);
            case RIGHT -> board.shiftRowRight(index, fixedBonusActive);
        };

        if (!res) {
            return false;
        }

        if (fixedBonusActive) {
            activeBonus = null;
        }

        currentMoveState = MoveState.MOVE;

        if (activeBonus == BonusTypes.PUSH_TWICE) {
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

        if (!allowedToUse) {
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

        if (!allowedToUse) {
            return;
        }

        activeBonus = BonusTypes.PUSH_TWICE;
    }

    public void usePushFixedBonus(Player player) {
        guardFor(player);
        var allowedToUse = player.useBonus(BonusTypes.PUSH_FIXED);

        if (!allowedToUse) {
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

    private synchronized void nextPlayer() {
        nextTurnTimer.stop();

        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        System.out.println("New Player to move: " + getCurrentPlayer().getUsername());
        currentMoveState = MoveState.PLACE_TILE;

        if (getCurrentPlayer().isAiActive()) {
            // Using AdvancedAiStrategy for smarter gameplay
            new labyrinth.server.game.ai.SimpleAiStrategy().performTurn(this, getCurrentPlayer());
        } else {
            nextTurnTimer.start(gameConfig.turnTimeInSeconds(), this::nextPlayer);
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
        return players.size() >= MAX_PLAYERS;
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

    public OffsetDateTime getGameEndTime() {
        if (gameStartTime == null) {
            return null;
        }
        return gameStartTime.plusSeconds(gameConfig.gameDurationInSeconds());
    }

    public OffsetDateTime getTurnEndTime() {
        return nextTurnTimer.getExpirationTime();
    }
}
