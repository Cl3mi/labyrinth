package labyrinth.server.game.models;

import labyrinth.server.game.abstractions.*;
import labyrinth.server.game.ai.AiStrategy;
import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.*;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import labyrinth.server.game.results.LeaveResult;
import labyrinth.server.game.results.MovePlayerToTileResult;
import labyrinth.server.game.results.ShiftResult;
import labyrinth.server.game.services.AchievementService;
import labyrinth.server.game.services.GameInitializerService;
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
 *
 * <p>This class now uses interface-based dependencies to enable
 * dependency injection, testing with mocks, and flexibility in
 * swapping implementations.</p>
 */
@Getter
@Setter
public class Game {

    private final int MAX_PLAYERS = 4;

    private IGameTimer nextTurnTimer;

    private final ITurnController turnController;
    private final IPlayerRegistry playerRegistry;
    private final IMovementManager movementManager;
    private final IAchievementService achievementService;
    private final IBonusManager bonusManager;

    private final GameInitializerService gameInitializer;

    @Setter(lombok.AccessLevel.NONE)
    private Board board;

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

    public java.util.List<labyrinth.server.game.models.records.GameLogEntry> getExecutionLogs() {
        return gameLogger.getExecutionLogs();
    }

    /**
     * Constructor with interface-based dependencies for improved testability.
     *
     * @param playerRegistry player management service
     * @param turnController turn state management service
     * @param movementManager movement and tile interaction service
     * @param achievementService achievement awarding service
     * @param bonusManager bonus management service
     * @param nextTurnTimer timer for turn management
     * @param aiStrategy AI strategy for automated players
     * @param gameLogger game event logger
     * @param gameInitializer game initialization service
     */
    public Game(
            IPlayerRegistry playerRegistry,
            ITurnController turnController,
            IMovementManager movementManager,
            IAchievementService achievementService,
            IBonusManager bonusManager,
            IGameTimer nextTurnTimer,
            AiStrategy aiStrategy,
            GameLogger gameLogger,
            GameInitializerService gameInitializer
    ) {
        this.playerRegistry = playerRegistry;
        this.turnController = turnController;
        this.movementManager = movementManager;
        this.achievementService = achievementService;
        this.bonusManager = bonusManager;
        this.nextTurnTimer = nextTurnTimer;
        this.aiStrategy = aiStrategy;
        this.gameLogger = gameLogger;
        this.gameInitializer = gameInitializer;

        this.roomState = RoomState.LOBBY;
        this.board = null;
        this.gameConfig = GameConfig.getDefault();
        this.activeBonusState = NoBonusActive.getInstance();
    }

    /**
     * Uses a bonus for the current player.
     * Delegates to the BonusManager for validation and application.
     *
     * @param type the type of bonus to use
     * @param args additional arguments required by the specific bonus
     * @return true if the bonus was successfully applied
     */
    public boolean useBonus(BonusTypes type, Object... args) {
        return bonusManager.useBonus(type, this, getCurrentPlayer(), args);
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

        return playerRegistry.addPlayer(username);
    }

    public LeaveResult leave(Player player) {
        return playerRegistry.removePlayer(player);
    }

    /**
     * Resets the game to LOBBY state, clearing all players and game data.
     * This allows starting a new game after the previous one has finished.
     */
    public void resetForNewGame() {
        if (nextTurnTimer != null) {
            nextTurnTimer.stop();
        }

        playerRegistry.clear();
        this.board = null;
        this.gameConfig = GameConfig.getDefault();
        this.activeBonusState = NoBonusActive.getInstance();
        turnController.reset();
        this.gameStartTime = null;
        this.roomState = RoomState.LOBBY;

        gameLogger.log(GameLogType.GAME, "Game reset to LOBBY state for new game");
    }

    public Player getPlayer(UUID playerId) {
        return playerRegistry.getPlayer(playerId);
    }

    /**
     * Starts the game. This method initializes players, distributes treasure cards,
     * sets up the board, and transitions the game to IN_GAME state.
     */
    public void startGame(GameConfig gameConfig, List<TreasureCard> treasureCards, Board board) {
        if (roomState != RoomState.LOBBY) {
            throw new IllegalStateException("Cannot start a game that is in progress or finished!");
        }

        if (playerRegistry.getPlayers().isEmpty()) {
            throw new IllegalStateException("At least 1 player is required to start the game");
        }

        playerRegistry.fillWithAiPlayers();

        this.gameConfig = Objects.requireNonNullElseGet(gameConfig, GameConfig::getDefault);

        List<Player> players = playerRegistry.getPlayersInternal();
        gameInitializer.distributeTreasuresAndBonuses(treasureCards, board, players, this.gameConfig.totalBonusCount());
        gameInitializer.initializePlayerPositions(board, this.gameConfig, players);
        gameInitializer.logGameStart(board, this.gameConfig, players, gameLogger);

        this.board = board;
        this.board.setPlayers(players);

        if (getCurrentPlayer().shouldMoveBePerformedByAi()) {
            this.aiStrategy.performTurn(this, getCurrentPlayer());
        }

        gameStartTime = OffsetDateTime.now();
        this.roomState = RoomState.IN_GAME;
    }

    public Player getCurrentPlayer() {
        return turnController.getCurrentPlayer(playerRegistry.getPlayersInternal());
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

        logShiftAction(index, direction, player);

        return new ShiftResult(true, false);
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
            return new MovePlayerToTileResult(false, distanceMoved, false, false, false);
        }

        logPlayerMove(row, col, distanceMoved, player);
        updatePlayerStatisticsAfterMove(player, distanceMoved);

        var currentTreasureCardAfterMove = player.getCurrentTreasureCard();
        boolean treasureCollected = currentTreasureCardAfterMove != currentTreasureCardBeforeMove;
        boolean gameOver = checkAndHandleGameOver(player, currentTreasureCardAfterMove);

        if (treasureCollected) {
            gameLogger.log(GameLogType.COLLECT_TREASURE, "Player collected treasure", player, null);
        }

        if (!gameOver) {
            nextPlayer();
        }

        return new MovePlayerToTileResult(true, distanceMoved, treasureCollected, gameOver, false);
    }

    /**
     * Logs a player's move to the game logger.
     */
    private void logPlayerMove(int row, int col, int distanceMoved, Player player) {
        java.util.Map<String, String> meta = new java.util.HashMap<>();
        meta.put("toRow", String.valueOf(row));
        meta.put("toCol", String.valueOf(col));
        meta.put("distance", String.valueOf(distanceMoved));
        gameLogger.log(GameLogType.MOVE_PLAYER,
                "Player moved to " + row + "/" + col + " (" + distanceMoved + " steps)",
                player, meta);
    }

    /**
     * Updates player statistics after a successful move.
     */
    private void updatePlayerStatisticsAfterMove(Player player, int distanceMoved) {
        player.getStatistics().increaseScore(PointRewards.REWARD_MOVE * distanceMoved);
        player.getStatistics().increaseStepsTaken(distanceMoved);

        if (player.getCurrentTreasureCard() == null) {
            player.getStatistics().increaseScore(PointRewards.REWARD_ALL_TREASURES_COLLECTED);
        }
    }

    /**
     * Checks if the player has won (all treasures collected and reached home tile).
     * If so, triggers game over sequence.
     *
     * @return true if game is over, false otherwise
     */
    private boolean checkAndHandleGameOver(Player player, TreasureCard currentTreasureCard) {
        if (currentTreasureCard == null && player.getCurrentTile() == player.getHomeTile()) {
            gameLogger.log(GameLogType.GAME_OVER, "Player " + player.getUsername() + " triggered game over", player, null);
            gameLogger.log(GameLogType.GAME_OVER, "Player has " + player.getAssignedTreasureCards().size() + " assigned treasures", player, null);

            for (int i = 0; i < player.getAssignedTreasureCards().size(); i++) {
                TreasureCard tc = player.getAssignedTreasureCards().get(i);
                gameLogger.log(GameLogType.GAME_OVER, "Treasure " + i + ": " + tc.getTreasureName() + " collected=" + tc.isCollected(), player, null);
            }

            gameLogger.log(GameLogType.GAME_OVER, "Player reached home tile - game over!", player, null);

            awardEndGameAchievements();
            gameOver();
            gameLogger.log(GameLogType.GAME_OVER, "Game Over. Winner: " + player.getUsername(), player, null);
            return true;
        }
        return false;
    }

    private List<AchievementService.AchievementAward> endGameAchievements = new ArrayList<>();

    /**
     * Awards end-game achievements to players with the best statistics.
     * Called before game over to ensure achievements are awarded before final scoring.
     */
    private void awardEndGameAchievements() {
        endGameAchievements = achievementService.awardEndGameAchievements(playerRegistry.getPlayersInternal());

        // Increase scores for awarded achievements
        for (var award : endGameAchievements) {
            award.player().getStatistics().increaseScore(50); // Achievement bonus points
            gameLogger.log(GameLogType.ACHIEVEMENT, award.player().getUsername() + " earned " + award.achievement(), award.player(), null);
        }
    }

    /**
     * Gets the achievements that were awarded at the end of the game.
     * @return list of achievement awards
     */
    public List<labyrinth.server.game.services.AchievementService.AchievementAward> getEndGameAchievements() {
        return new ArrayList<>(endGameAchievements);
    }

    private void gameOver() {
        this.roomState = RoomState.FINISHED;
    }

    /**
     * Resets the game back to lobby state after game completion.
     * Clears the board and prepares for a new game to be started.
     */
    public void returnToLobby() {
        gameLogger.log(GameLogType.RETURN_TO_LOBBY, "Current state: " + this.roomState);

        if (this.roomState != RoomState.FINISHED && this.roomState != RoomState.IN_GAME) {
            gameLogger.error(GameLogType.RETURN_TO_LOBBY, "Cannot return to lobby from state: " + this.roomState);
            throw new IllegalStateException("Cannot return to lobby from state: " + this.roomState);
        }
        gameLogger.log(GameLogType.RETURN_TO_LOBBY," Resetting game to lobby state");
        this.roomState = RoomState.LOBBY;
        this.board = null;
        this.gameStartTime = null;
        turnController.reset();

        // Reset player stats and treasures for new game
        for (Player player : playerRegistry.getPlayersInternal()) {
            player.resetForNewGame();
        }

        gameLogger.log(GameLogType.RETURN_TO_LOBBY, "Reset complete. Players remaining: " + playerRegistry.getPlayers().size());
    }

    private synchronized void nextPlayer() {
        turnController.advanceToNextPlayer(
                            playerRegistry.getPlayersInternal(),
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
        turnController.guardForPlayer(board, playerRegistry.getPlayersInternal(), playerToMove);
    }

    public List<Player> getPlayers() {
        return playerRegistry.getPlayers();
    }

    public int getMAX_PLAYERS() {
        return MAX_PLAYERS;
    }

    @Override
    public String toString() {
        return "Room{" +
                ", players=" + playerRegistry.getPlayers() +
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

    /**
     * Sets the current move state.
     *
     * @param moveState the new move state
     */
    public void setMoveState(MoveState moveState) {
        turnController.setMoveState(moveState);
    }

    /**
     * Processes a player stepping onto a tile, handling treasure and bonus collection.
     * Used by bonus effects that move players (BEAM, SWAP).
     *
     * @param player the player stepping onto the tile
     * @param tile   the tile being stepped on
     */
    public void processPlayerStepOnTile(Player player, Tile tile) {
        movementManager.processPlayerStepOnTile(player, tile);
    }

    /**
     * Checks if a bonus has already been used this turn.
     *
     * @return true if a bonus was used this turn, false otherwise
     */
    public boolean isBonusUsedThisTurn() {
        return turnController.isBonusUsedThisTurn();
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
