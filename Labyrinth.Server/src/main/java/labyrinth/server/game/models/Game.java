package labyrinth.server.game.models;

import labyrinth.contracts.models.PlayerColor;
import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.*;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.Position;
import labyrinth.server.game.results.movePlayerToTileResult;
import labyrinth.server.game.results.shiftResult;
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

    private final labyrinth.server.game.ai.AiStrategy aiStrategy;

    private final labyrinth.server.game.services.GameLogger gameLogger;

    public java.util.List<labyrinth.server.game.models.records.GameLogEntry> getExecutionLogs() {
        return gameLogger.getExecutionLogs();
    }

    private final java.util.Map<BonusTypes, labyrinth.server.game.bonuses.IBonusEffect> bonusEffects = new java.util.EnumMap<>(
            BonusTypes.class);

    public Game(IGameTimer nextTurnTimer, labyrinth.server.game.ai.AiStrategy aiStrategy,
            labyrinth.server.game.services.GameLogger gameLogger) {
        this.nextTurnTimer = nextTurnTimer;
        this.aiStrategy = aiStrategy;
        this.gameLogger = gameLogger;
        this.players = new ArrayList<>();
        this.roomState = RoomState.LOBBY;
        this.board = null;
        this.gameConfig = GameConfig.getDefault();
        this.currentPlayerIndex = 0;

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
            // Could add args to metadata if needed
            gameLogger.log(GameLogType.USE_BONUS, "Player used bonus " + type, getCurrentPlayer(), meta);
        }
        return result;
    }

    // Kept for backward compatibility / API contract compliance, but delegates to
    // strategy
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

        if (players.isEmpty()) {
            throw new IllegalStateException("At least 1 player is required to start the game");
        }

        while (players.size() < MAX_PLAYERS) {
            addAiPlayer();
        }

        this.gameConfig = Objects.requireNonNullElseGet(gameConfig, GameConfig::getDefault);
        // Treasure cards created - logged via gameLogger if needed

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

        labyrinth.server.game.factories.BonusFactory bonusFactory = new labyrinth.server.game.factories.BonusFactory();
        var bonuses = bonusFactory.createBonuses(gameConfig.totalBonusCount());
        board.placeRandomBonuses(bonuses);

        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            var position = gameConfig.getStartPosition(i);

            Tile startingTile = board.getTileAt(position);
            // Player start positions logged in the startMeta below
            player.setHomeTile(startingTile);
            player.setCurrentTile(startingTile);
        }

        this.board = board;
        this.board.setPlayers(players);

        java.util.Map<String, String> startMeta = new java.util.HashMap<>();
        startMeta.put("boardState", gameLogger.serializeBoard(board));
        // Also log initial player positions maybe? They are effectively on home tiles
        // which are in board, but explicit is nice.
        for (Player p : players) {
            startMeta.put("player_" + p.getId(),
                    p.getUsername() + "@" + gameConfig.getStartPosition(players.indexOf(p)));
        }

        gameLogger.log(GameLogType.START_GAME, "Game started in GameLobby with " + players.size() + " players.", null,
                startMeta);

        if (getCurrentPlayer().isAiActive()) {
            this.aiStrategy.performTurn(this, getCurrentPlayer());
        }

        gameStartTime = OffsetDateTime.now();
        this.roomState = RoomState.IN_GAME;
    }

    public Player getCurrentPlayer() {
        return players.get(currentPlayerIndex);
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

    public shiftResult shift(int index, Direction direction, Player player) {
        guardFor(MoveState.PLACE_TILE);
        guardFor(player);
        guardFor(RoomState.IN_GAME);

        var fixedBonusActive = activeBonus == BonusTypes.PUSH_FIXED;

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
            return new shiftResult(false, false);
        }

        if (fixedBonusActive) {
            activeBonus = null;
        }

        currentMoveState = MoveState.MOVE;

        if (activeBonus == BonusTypes.PUSH_TWICE) {
            currentMoveState = MoveState.PLACE_TILE;
            activeBonus = null;
        }

        player.getStatistics().increaseScore(PointRewards.REWARD_SHIFT_TILE);
        player.getStatistics().increaseTilesPushed(1);

        var statistics = player.getStatistics();
        var pusherAchieved = false;
        if (!statistics.getCollectedAchievements().contains(Achievement.PUSHER) && statistics.getTilesPushed() >= 20) {
            pusherAchieved = true;
            statistics.collectAchievement(Achievement.PUSHER);
        }

        java.util.Map<String, String> meta = new java.util.HashMap<>();
        meta.put("index", String.valueOf(index));
        meta.put("direction", direction.toString());
        // Capture extra tile BEFORE shift (which effectively becomes the one pushed in)
        // Wait, logic says: shift methods use 'extraTile' to put into board.
        // So we should log the CURRENT extraTile before the shift happens?
        // The shift method swaps extraTile with the one pushed out.
        // BUT the tool call logic here happens AFTER the shift method returns!
        // See: boolean res = switch... then if(!res) return... then log.
        // So at this point 'extraTile' is the one that got pushed OUT.
        // The one that got pushed IN was the 'extraTile' before this function call.
        // However, we want to know what tile WAS pushed in.
        // Since we can't easily go back in time, we should probably log the board state
        // or just trust
        // that if we have initial state + all operations, we can reconstruct.
        // USER REQUEST said: "Also how the tile that gets pushed in is shaped"
        // Since we are logging AFTER the action, we missed the state of the tile that
        // was pushed in (it is now on the board).
        // Actually, we can just find it on the board?
        // If we shift column down, the new tile is at (0, col).
        // If we shift column up, the new tile is at (height-1, col).
        // If we shift row right, new is at (row, 0).
        // If we shift row left, new is at (row, width-1).

        // Let's grab the tile that was just inserted.
        Tile insertedTile = null;
        if (direction == Direction.DOWN)
            insertedTile = board.getTileAt(0, index);
        else if (direction == Direction.UP)
            insertedTile = board.getTileAt(board.getHeight() - 1, index);
        else if (direction == Direction.RIGHT)
            insertedTile = board.getTileAt(index, 0);
        else if (direction == Direction.LEFT)
            insertedTile = board.getTileAt(index, board.getWidth() - 1);

        if (insertedTile != null) {
            meta.put("insertedTile", gameLogger.serializeTile(insertedTile));
        }

        gameLogger.log(GameLogType.SHIFT_BOARD, "Player shifted board " + direction + " at index " + index, player,
                meta);

        return new shiftResult(true, pusherAchieved);
    }

    public void toggleAiForPlayer(Player player) {
        player.setAiActive(!player.isAiActive());
    }

    public movePlayerToTileResult movePlayerToTile(int row, int col, Player player) {
        guardFor(RoomState.IN_GAME);
        guardFor(MoveState.MOVE);
        guardFor(player);

        var currentTreasureCardBeforeMove = player.getCurrentTreasureCard();
        var distanceMoved = board.movePlayerToTile(player, row, col);

        // removed LOGGER.info
        if (distanceMoved == -1) {
            // Move failed - no logging needed for failed attempt
            return new movePlayerToTileResult(false, distanceMoved, false, false, false);
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
            gameOver();
            gameOver = true;
            gameLogger.log(GameLogType.GAME_OVER, "Game Over. Winner: " + player.getUsername(), player, null);
        }

        var treasureCollected = currentTreasureCardAfterMove != currentTreasureCardBeforeMove;

        if (treasureCollected) {
            gameLogger.log(GameLogType.COLLECT_TREASURE, "Player collected treasure", player, null);
        }

        var statistics = player.getStatistics();
        var runnerAchieved = false;
        if (!statistics.getCollectedAchievements().contains(Achievement.RUNNER) && statistics.getStepsTaken() >= 200) {
            runnerAchieved = true;
            statistics.collectAchievement(Achievement.RUNNER);
        }

        nextPlayer();
        return new movePlayerToTileResult(true, distanceMoved, treasureCollected, gameOver, runnerAchieved);
    }

    private void gameOver() {
        this.roomState = RoomState.FINISHED;
    }

    private synchronized void nextPlayer() {
        guardFor(RoomState.IN_GAME);
        nextTurnTimer.stop();

        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        // LOGGER handled by logGameAction
        gameLogger.log(GameLogType.NEXT_TURN, "New Player to move: " + getCurrentPlayer().getUsername(),
                getCurrentPlayer(), null);
        currentMoveState = MoveState.PLACE_TILE;

        if (getCurrentPlayer().isAiActive()) {
            this.aiStrategy.performTurn(this, getCurrentPlayer());
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
