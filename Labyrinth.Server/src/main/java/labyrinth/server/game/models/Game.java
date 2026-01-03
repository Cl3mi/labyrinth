package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.AiStrategy;
import labyrinth.server.game.bonuses.IBonusEffect;
import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.*;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import labyrinth.server.game.results.LeaveResult;
import labyrinth.server.game.results.MovePlayerToTileResult;
import labyrinth.server.game.results.ShiftResult;
import labyrinth.server.game.services.GameLogger;
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

    private IGameTimer nextTurnTimer;

    private final labyrinth.server.game.services.TurnController turnController;

    @Setter(lombok.AccessLevel.NONE)
    private Board board;

    private final List<Player> players;
    private RoomState roomState;

    private BonusState activeBonusState;

    @Setter(AccessLevel.NONE)
    @Getter(AccessLevel.NONE)
    private GameConfig gameConfig;

    @Setter(AccessLevel.PRIVATE)
    @Getter(AccessLevel.NONE)
    private OffsetDateTime gameStartTime;

    private final labyrinth.server.game.ai.AiStrategy aiStrategy;

    private final labyrinth.server.game.services.GameLogger gameLogger;

    private final labyrinth.server.game.services.MovementManager movementManager;

    private final labyrinth.server.game.services.AchievementService achievementService;

    public java.util.List<labyrinth.server.game.models.records.GameLogEntry> getExecutionLogs() {
        return gameLogger.getExecutionLogs();
    }

    private final java.util.Map<BonusTypes, IBonusEffect> bonusEffects = new java.util.EnumMap<>(BonusTypes.class);

    public Game(
            IGameTimer nextTurnTimer,
            AiStrategy aiStrategy,
            GameLogger gameLogger
    ) {
        this.nextTurnTimer = nextTurnTimer;
        this.aiStrategy = aiStrategy;
        this.gameLogger = gameLogger;
        this.players = new ArrayList<>();
        this.roomState = RoomState.LOBBY;
        this.board = null;
        this.gameConfig = GameConfig.getDefault();
        this.activeBonusState = NoBonusActive.getInstance();
        this.turnController = new labyrinth.server.game.services.TurnController(nextTurnTimer, gameLogger);
        this.movementManager = new labyrinth.server.game.services.MovementManager();
        this.achievementService = new labyrinth.server.game.services.AchievementService();

        // Initialize Bonus Strategies
        bonusEffects.put(BonusTypes.BEAM, new labyrinth.server.game.bonuses.BeamBonusEffect());
        bonusEffects.put(BonusTypes.SWAP, new labyrinth.server.game.bonuses.SwapBonusEffect());
        bonusEffects.put(BonusTypes.PUSH_TWICE, new labyrinth.server.game.bonuses.PushTwiceBonusEffect());
        bonusEffects.put(BonusTypes.PUSH_FIXED, new labyrinth.server.game.bonuses.PushFixedBonusEffect());
    }

    public boolean useBonus(BonusTypes type, Object... args) {
        if (!bonusEffects.containsKey(type)) {
            throw new IllegalArgumentException("No strategy found for bonus type: " + type);
        }
        boolean result = bonusEffects.get(type).apply(this, getCurrentPlayer(), args);
        if (result) {
            java.util.Map<String, String> meta = new java.util.HashMap<>();
            meta.put("bonusType", type.toString());
            gameLogger.log(GameLogType.USE_BONUS, "Player used bonus " + type, getCurrentPlayer(), meta);
        }
        return result;
    }

    // Kept for backward compatibility / API contract compliance, but delegates to strategy
    // Should be cleaned up I guess?
    public boolean useBeamBonus(int row, int col, Player player) {
        guardFor(RoomState.IN_GAME);
        guardFor(player);
        guardFor(MoveState.PLACE_TILE);
        return useBonus(BonusTypes.BEAM, row, col);
    }

    public boolean useSwapBonus(Player currentPlayer, Player targetPlayer) {
        guardFor(RoomState.IN_GAME);
        guardFor(currentPlayer);
        guardFor(MoveState.PLACE_TILE);
        return useBonus(BonusTypes.SWAP, targetPlayer);
    }

    public boolean usePushTwiceBonus(Player player) {
        guardFor(RoomState.IN_GAME);
        guardFor(player);
        return useBonus(BonusTypes.PUSH_TWICE);
    }

    public boolean usePushFixedBonus(Player player) {
        guardFor(player);
        guardFor(RoomState.IN_GAME);
        return useBonus(BonusTypes.PUSH_FIXED);
    }

    /**
     * Adds a player to the room. The first player that joins will become the admin.
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

    /**
     * Fills the game with AI players up to MAX_PLAYERS.
     * Called before creating treasure cards to ensure correct player count.
     */
    public void fillWithAiPlayers() {
        while (players.size() < MAX_PLAYERS) {
            addAiPlayer();
        }
    }

    public LeaveResult leave(Player player) {
        // Check if player exists in the list
        boolean wasRemoved = players.removeIf(p -> p.getId().equals(player.getId()));

        if (!wasRemoved) {
            return new LeaveResult(false, null, false);
        }

        // Check if we need to reassign admin
        Player newAdmin = null;
        if (player.isAdmin()) {
            // Find first non-AI player to become new admin
            var nextAdmin = players.stream()
                    .filter(p -> !p.isAiActive())
                    .findFirst();

            if (nextAdmin.isPresent()) {
                nextAdmin.get().setAdmin(true);
                newAdmin = nextAdmin.get();
            }
        }

        // Check if only AI players remain (or no players at all)
        boolean shouldShutdown = players.stream().noneMatch(p -> !p.isAiActive());

        return new LeaveResult(true, newAdmin, shouldShutdown);
    }

    public Player getPlayer(UUID playerId) {
        return players.stream()
                .filter(p -> p.getId().equals(playerId))
                .findFirst()
                .orElse(null);
    }

    /**
     * Starts the game. This method initializes players, distributes treasure cards,
     * sets up the board, and transitions the game to IN_GAME state.
     */
    public void startGame(GameConfig gameConfig, List<TreasureCard> treasureCards, Board board) {
        if (roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot start a game that is in progress or finished!");
        }

        if (players.isEmpty()) {
            throw new IllegalStateException("At least 1 player is required to start the game");
        }

        // AI players are now filled before this method is called (in GameService)
        // to ensure correct treasure count calculation

        this.gameConfig = Objects.requireNonNullElseGet(gameConfig, GameConfig::getDefault);

        distributeTreasureCards(treasureCards, board);

        labyrinth.server.game.factories.BonusFactory bonusFactory = new labyrinth.server.game.factories.BonusFactory();
        var bonuses = bonusFactory.createBonuses(this.gameConfig.totalBonusCount());
        board.placeRandomBonuses(bonuses);

        initializePlayerPositions(board, this.gameConfig);

        this.board = board;
        this.board.setPlayers(players);

        logGameStart(board, this.gameConfig);

        if (getCurrentPlayer().isAiActive()) {
            this.aiStrategy.performTurn(this, getCurrentPlayer());
        }

        gameStartTime = OffsetDateTime.now();
        this.roomState = RoomState.IN_GAME;
    }

    public Player getCurrentPlayer() {
        return turnController.getCurrentPlayer(players);
    }

    public int getCurrentPlayerIndex() {
        return turnController.getCurrentPlayerIndex();
    }

    public MoveState getCurrentMoveState() {
        return turnController.getCurrentMoveState();
    }

    public Position getCurrentPositionOfPlayer(Player player) {
        var tileOfPlayer = player.getCurrentTile();
        return board.getPositionOfTile(tileOfPlayer);
    }

    public void rotateExtraTileClockwise(Player player) {
        guardFor(MoveState.PLACE_TILE);
        guardFor(RoomState.IN_GAME);
        guardFor(player);

        var board = getBoard();
        board.getExtraTile().rotate();
    }

    public ShiftResult shift(int index, Direction direction, Player player) {
        guardFor(MoveState.PLACE_TILE);
        guardFor(player);
        guardFor(RoomState.IN_GAME);

        var fixedBonusActive = activeBonusState.isOfType(BonusTypes.PUSH_FIXED);

        if (fixedBonusActive) {
            // TODO: do not allow to shift border rows/columns because it would move home
            // tiles
        }

        boolean res = switch (direction) {
            case UP -> board.shiftColumnUp(index, fixedBonusActive);
            case DOWN -> board.shiftColumnDown(index, fixedBonusActive);
            case LEFT -> board.shiftRowLeft(index, fixedBonusActive);
            case RIGHT -> board.shiftRowRight(index, fixedBonusActive);
        };

        if (!res) {
            return new ShiftResult(false, false);
        }

        handleBonusAfterShift();

        player.getStatistics().increaseScore(PointRewards.REWARD_SHIFT_TILE);
        player.getStatistics().increaseTilesPushed(1);

        var pusherAchieved = achievementService.checkPusherAchievement(player).isPresent();

        logShiftAction(index, direction, player);

        return new ShiftResult(true, pusherAchieved);
    }

    public void toggleAiForPlayer(Player player) {
        player.setAiActive(!player.isAiActive());
    }

    public MovePlayerToTileResult movePlayerToTile(int row, int col, Player player) {
        guardFor(RoomState.IN_GAME);
        guardFor(MoveState.MOVE);
        guardFor(player);

        var currentTreasureCardBeforeMove = player.getCurrentTreasureCard();
        var distanceMoved = board.movePlayerToTile(player, row, col, movementManager);

        if (distanceMoved == -1) {
            // Move failed - no logging needed for failed attempt
            return new MovePlayerToTileResult(false, distanceMoved, false, false, false);
        }

        java.util.Map<String, String> meta = new java.util.HashMap<>();
        meta.put("toRow", String.valueOf(row));
        meta.put("toCol", String.valueOf(col));
        meta.put("distance", String.valueOf(distanceMoved));
        gameLogger.log(GameLogType.MOVE_PLAYER, "Player moved to " + row + "/" + col + " (" + distanceMoved + " steps)",
                player, meta);

        player.getStatistics().increaseScore(PointRewards.REWARD_MOVE * distanceMoved);
        player.getStatistics().increaseStepsTaken(distanceMoved);

        var currentTreasureCardAfterMove = player.getCurrentTreasureCard();

        if (currentTreasureCardAfterMove == null) {
            player.getStatistics().increaseScore(PointRewards.REWARD_ALL_TREASURES_COLLECTED);
        }

        var gameOver = false;
        if (currentTreasureCardAfterMove == null) {
            System.out.println("[GAME OVER DEBUG] Player " + player.getUsername() + " triggered game over");
            System.out.println("[GAME OVER DEBUG] Player has " + player.getAssignedTreasureCards().size() + " assigned treasures");
            for (int i = 0; i < player.getAssignedTreasureCards().size(); i++) {
                TreasureCard tc = player.getAssignedTreasureCards().get(i);
                System.out.println("[GAME OVER DEBUG]   Treasure " + i + ": " + tc.getTreasureName() + " collected=" + tc.isCollected());
            }
            gameOver();
            gameOver = true;
            gameLogger.log(GameLogType.GAME_OVER, "Game Over. Winner: " + player.getUsername(), player, null);
        }

        var treasureCollected = currentTreasureCardAfterMove != currentTreasureCardBeforeMove;

        if (treasureCollected) {
            gameLogger.log(GameLogType.COLLECT_TREASURE, "Player collected treasure", player, null);
        }

        var runnerAchieved = achievementService.checkRunnerAchievement(player).isPresent();

        nextPlayer();
        return new MovePlayerToTileResult(true, distanceMoved, treasureCollected, gameOver, runnerAchieved);
    }

    private void gameOver() {
        this.roomState = RoomState.FINISHED;
    }

    /**
     * Resets the game back to lobby state after game completion.
     * Clears the board and prepares for a new game to be started.
     */
    public void returnToLobby() {
        System.out.println("[RETURN TO LOBBY] Current state: " + this.roomState);

        if (this.roomState != RoomState.FINISHED) {
            System.err.println("[RETURN TO LOBBY ERROR] Cannot return to lobby from state: " + this.roomState);
            throw new IllegalStateException("Cannot return to lobby from state: " + this.roomState);
        }

        System.out.println("[RETURN TO LOBBY] Resetting game to lobby state");
        this.roomState = RoomState.LOBBY;
        this.board = null;
        this.gameStartTime = null;
        turnController.reset();

        // Reset player stats and treasures for new game
        for (Player player : players) {
            player.resetForNewGame();
        }

        System.out.println("[RETURN TO LOBBY] Reset complete. Players remaining: " + players.size());
    }

    private synchronized void nextPlayer() {
        turnController.advanceToNextPlayer(
                            players,
                            roomState,
                            gameConfig,
                      player -> aiStrategy.performTurn(this, player));
    }

    private void guardFor(MoveState moveState) {
        turnController.guardForMoveState(board, moveState);
    }

    private void guardFor(RoomState roomState) {
        turnController.guardForRoomState(this.roomState, roomState);
    }

    private void guardFor(Player playerToMove) {
        turnController.guardForPlayer(board, players, playerToMove);
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

    /**
     * Handles bonus state transitions after a successful board shift.
     * Consumes the PUSH_FIXED bonus if it was active, or allows another push
     * if PUSH_TWICE bonus is active.
     */
    private void handleBonusAfterShift() {
        boolean isPushTwice = activeBonusState.isOfType(BonusTypes.PUSH_TWICE);

        activeBonusState = activeBonusState.consume();

        if (isPushTwice) {
            turnController.setMoveState(MoveState.PLACE_TILE);
        } else {
            turnController.setMoveState(MoveState.MOVE);
        }
    }

    /**
     * Logs the board shift action with metadata including the inserted tile.
     * Determines which tile was inserted based on the shift direction.
     *
     * @param index     the row or column index that was shifted
     * @param direction the direction of the shift
     * @param player    the player who performed the shift
     */
    private void logShiftAction(int index, Direction direction, Player player) {
        java.util.Map<String, String> meta = new java.util.HashMap<>();
        meta.put("index", String.valueOf(index));
        meta.put("direction", direction.toString());

        Tile insertedTile = getInsertedTile(index, direction);
        if (insertedTile != null) {
            meta.put("insertedTile", gameLogger.serializeTile(insertedTile));
        }

        gameLogger.log(GameLogType.SHIFT_BOARD, "Player shifted board " + direction + " at index " + index, player,
                meta);
    }

    /**
     * Determines which tile was inserted into the board after a shift operation.
     *
     * @param index     the row or column index that was shifted
     * @param direction the direction of the shift
     * @return the tile that was inserted, or null if direction is invalid
     */
    private Tile getInsertedTile(int index, Direction direction) {
        return switch (direction) {
            case DOWN -> board.getTileAt(0, index);
            case UP -> board.getTileAt(board.getHeight() - 1, index);
            case RIGHT -> board.getTileAt(index, 0);
            case LEFT -> board.getTileAt(index, board.getWidth() - 1);
        };
    }

    /**
     * Distributes treasure cards to players in round-robin fashion and places them on the board.
     *
     * @param treasureCards the list of treasure cards to distribute
     * @param board         the board to place treasures on
     */
    private void distributeTreasureCards(List<TreasureCard> treasureCards, Board board) {
        System.out.println("[TREASURE DEBUG] Distributing " + treasureCards.size() + " treasures to " + players.size() + " players");

        var playerToAssignCardsToIndex = 0;
        while (!treasureCards.isEmpty()) {
            TreasureCard card = treasureCards.getFirst();
            board.placeRandomTreasure(card);

            Player player = players.get(playerToAssignCardsToIndex);
            player.getAssignedTreasureCards().add(card);
            System.out.println("[TREASURE DEBUG] Assigned '" + card.getTreasureName() + "' to " + player.getUsername());

            playerToAssignCardsToIndex++;
            if (playerToAssignCardsToIndex >= players.size()) {
                playerToAssignCardsToIndex = 0;
            }

            treasureCards.removeFirst();
        }

        // Verify distribution
        for (Player p : players) {
            System.out.println("[TREASURE DEBUG] " + p.getUsername() + " has " + p.getAssignedTreasureCards().size() + " treasures");
        }
    }

    /**
     * Initializes player positions on the board based on the game configuration.
     * Sets both the home tile and current tile for each player.
     *
     * @param board      the board to place players on
     * @param gameConfig the game configuration containing start positions
     */
    private void initializePlayerPositions(Board board, GameConfig gameConfig) {
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            var position = gameConfig.getStartPosition(i);

            Tile startingTile = board.getTileAt(position);
            player.setHomeTile(startingTile);
            player.setCurrentTile(startingTile);
        }
    }

    /**
     * Logs the game start event with board state and player positions.
     *
     * @param board      the initialized board
     * @param gameConfig the game configuration
     */
    private void logGameStart(Board board, GameConfig gameConfig) {
        java.util.Map<String, String> startMeta = new java.util.HashMap<>();
        startMeta.put("boardState", gameLogger.serializeBoard(board));

        for (Player p : players) {
            startMeta.put("player_" + p.getId(),
                    p.getUsername() + "@" + gameConfig.getStartPosition(players.indexOf(p)));
        }

        gameLogger.log(GameLogType.START_GAME, "Game started in GameLobby with " + players.size() + " players.", null,
                startMeta);
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

    /**
     * Sets the active bonus state. Used by bonus effect implementations.
     * Provided for backward compatibility with existing bonus effects.
     *
     * @param bonusType the type of bonus to activate
     */
    public void setActiveBonus(BonusTypes bonusType) {
        this.activeBonusState = new ActiveBonus(bonusType);
    }

    /**
     * Gets the currently active bonus type, if any.
     * Provided for backward compatibility with tests.
     *
     * @return the active bonus type, or null if no bonus is active
     */
    public BonusTypes getActiveBonus() {
        return activeBonusState.getBonusType().orElse(null);
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
