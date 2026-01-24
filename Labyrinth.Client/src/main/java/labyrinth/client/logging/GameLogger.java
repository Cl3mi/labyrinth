package labyrinth.client.logging;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.PushActionInfo;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.TurnState;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GameLogger {

    private static final Logger log = LoggerFactory.getLogger(GameLogger.class);

    private int moveNumber = 0;
    private String lastPushPlayerId;
    private TurnState lastTurnState;
    private Map<String, Position> lastPlayerPositions = new HashMap<>();
    private Map<String, Integer> lastTreasureCounts = new HashMap<>();
    private Map<String, Integer> playerNumbers = new HashMap<>();
    private PushActionInfo lastLoggedPush;

    private static final DateTimeFormatter DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static final DateTimeFormatter FILE_DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss");

    private static final String PROFILE = System.getProperty("labyrinth.profile", "default");
    private static final Path LOG_DIR = Paths.get("logs");

    private BufferedWriter gameFileWriter;
    private Path currentLogFile;

    // Box-drawing characters for tile visualization with orientation
    // L-shapes (corners)
    private static final char L_UP_RIGHT = '\u2514';    // └
    private static final char L_UP_LEFT = '\u2518';     // ┘
    private static final char L_DOWN_RIGHT = '\u250C';  // ┌
    private static final char L_DOWN_LEFT = '\u2510';   // ┐

    // I-shapes (straight)
    private static final char I_VERTICAL = '\u2502';    // │
    private static final char I_HORIZONTAL = '\u2500';  // ─

    // T-shapes
    private static final char T_UP = '\u2534';          // ┴ (open up, left, right)
    private static final char T_DOWN = '\u252C';        // ┬ (open down, left, right)
    private static final char T_LEFT = '\u2524';        // ┤ (open up, down, left)
    private static final char T_RIGHT = '\u251C';       // ├ (open up, down, right)

    // Cross
    private static final char CROSS = '\u253C';         // ┼

    /**
     * Logs the initial game state when a new game starts.
     * Creates a new log file for this game.
     */
    public void logGameStart(Board board, List<Player> players) {
        moveNumber = 0;
        lastPlayerPositions.clear();
        lastTreasureCounts.clear();
        playerNumbers.clear();
        lastLoggedPush = null;
        lastPushPlayerId = null;
        lastTurnState = null;

        // Create new log file for this game
        createNewGameFile();

        // Store initial player state and assign player numbers
        int pNum = 1;
        for (Player player : players) {
            playerNumbers.put(player.getId(), pNum++);
            if (player.getCurrentPosition() != null) {
                lastPlayerPositions.put(player.getId(),
                    new Position(player.getCurrentPosition().getRow(), player.getCurrentPosition().getColumn()));
            }
            lastTreasureCounts.put(player.getId(), player.getTreasuresFound().size());
        }

        StringBuilder sb = new StringBuilder();
        sb.append("================================================================================\n");
        sb.append("LABYRINTH GAME STARTED - ").append(LocalDateTime.now().format(DATE_FORMAT)).append("\n");
        sb.append("================================================================================\n\n");

        // Log board dimensions and initial state
        sb.append(String.format("INITIAL BOARD (%dx%d):\n", board.getWidth(), board.getHeight()));
        sb.append(formatBoard(board, players));
        sb.append("\n");

        // Add textual board description
        sb.append("TILE DETAILS:\n");
        sb.append(formatBoardTextual(board, players));
        sb.append("\n");

        // Log player states
        sb.append("PLAYERS:\n");
        for (Player player : players) {
            sb.append(formatPlayerInfo(player, playerNumbers.get(player.getId())));
        }
        sb.append("\n");

        // Log extra tile
        if (board.getExtraTile() != null) {
            sb.append("EXTRA TILE: ").append(getTileShape(board.getExtraTile()))
              .append(" [").append(formatExits(board.getExtraTile())).append("]\n");
        }

        sb.append("\n================================================================================\n");
        sb.append("GAME MOVES\n");
        sb.append("================================================================================\n");

        writeToFile(sb.toString());
    }

    private void createNewGameFile() {
        closeCurrentFile();

        try {
            // Create logs directory if it doesn't exist
            Files.createDirectories(LOG_DIR);

            // Create new file with timestamp
            String timestamp = LocalDateTime.now().format(FILE_DATE_FORMAT);
            String filename = String.format("game_%s_%s.log", PROFILE, timestamp);
            currentLogFile = LOG_DIR.resolve(filename);

            gameFileWriter = Files.newBufferedWriter(currentLogFile,
                StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);

            log.info("Created new game log: {}", currentLogFile);
        } catch (IOException e) {
            log.error("Failed to create game log file: {}", e.getMessage());
        }
    }

    private void closeCurrentFile() {
        if (gameFileWriter != null) {
            try {
                gameFileWriter.close();
            } catch (IOException e) {
                log.error("Failed to close game log file: {}", e.getMessage());
            }
            gameFileWriter = null;
        }
    }

    private void writeToFile(String content) {
        if (gameFileWriter != null) {
            try {
                gameFileWriter.write(content);
                gameFileWriter.flush();
            } catch (IOException e) {
                log.error("Failed to write to game log: {}", e.getMessage());
            }
        }
    }

    /**
     * Logs a game state update by detecting and logging changes.
     * This is called after each state update from the server.
     */
    public void logStateUpdate(Board newBoard, List<Player> players,
                               String currentPlayerId, TurnState currentTurnState) {

        PushActionInfo currentPush = newBoard.getLastPush();

        // Case 1: Turn state changed from PUSH to MOVE
        // This means the current player just completed their push
        if (lastTurnState == TurnState.WAITING_FOR_PUSH && currentTurnState == TurnState.WAITING_FOR_MOVE) {
            // Log the push that just happened
            if (currentPush != null && !isSamePush(currentPush, lastLoggedPush)) {
                Player pusher = findPlayerById(players, currentPlayerId);
                if (pusher != null) {
                    logPushAction(pusher, currentPush);
                }
                lastLoggedPush = currentPush;
            }

            // Store current positions AFTER push (tiles may have shifted players)
            updatePlayerPositions(players);
        }

        // Case 2: Turn state changed from MOVE to PUSH
        // This means a player just completed their move, now it's next player's turn
        if (lastTurnState == TurnState.WAITING_FOR_MOVE && currentTurnState == TurnState.WAITING_FOR_PUSH) {
            // Check for position changes and treasures
            for (Player player : players) {
                Position oldPos = lastPlayerPositions.get(player.getId());
                Position newPos = player.getCurrentPosition();

                if (oldPos != null && newPos != null && !oldPos.equals(newPos)) {
                    // Player moved
                    int oldTreasures = lastTreasureCounts.getOrDefault(player.getId(), 0);
                    int newTreasures = player.getTreasuresFound().size();
                    boolean collectedTreasure = newTreasures > oldTreasures;

                    logMoveAction(player, oldPos, newPos, collectedTreasure);
                }
            }

            // Update stored positions and treasure counts
            updatePlayerPositions(players);
        }

        lastTurnState = currentTurnState;
    }

    private void updatePlayerPositions(List<Player> players) {
        for (Player player : players) {
            Position newPos = player.getCurrentPosition();
            if (newPos != null) {
                lastPlayerPositions.put(player.getId(),
                    new Position(newPos.getRow(), newPos.getColumn()));
            }
            lastTreasureCounts.put(player.getId(), player.getTreasuresFound().size());
        }
    }

    private void logPushAction(Player player, PushActionInfo pushInfo) {
        if (pushInfo == null || pushInfo.getDirection() == null) return;

        moveNumber++;

        int index = pushInfo.getRowOrColIndex();
        Direction direction = pushInfo.getDirection();

        String pushNotation = formatPushNotation(index, direction);
        int pNum = playerNumbers.getOrDefault(player.getId(), 0);

        writeToFile(String.format("\n%3d. P%d (%s)  PUSH %s\n",
            moveNumber,
            pNum,
            String.format("%-10s", player.getName()),
            pushNotation));
    }

    private void logMoveAction(Player player, Position from, Position to, boolean collectedTreasure) {
        int pNum = playerNumbers.getOrDefault(player.getId(), 0);

        StringBuilder sb = new StringBuilder();
        sb.append(String.format("     P%d (%s)  MOVE %s -> %s",
            pNum,
            String.format("%-10s", player.getName()),
            formatPosition(from),
            formatPosition(to)));

        if (collectedTreasure) {
            sb.append(" * TREASURE!");
        }
        sb.append("\n");

        writeToFile(sb.toString());
    }

    private boolean isSamePush(PushActionInfo a, PushActionInfo b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.getRowOrColIndex() == b.getRowOrColIndex() &&
               a.getDirection() == b.getDirection();
    }

    private Player findPlayerById(List<Player> players, String id) {
        if (players == null || id == null) return null;
        for (Player p : players) {
            if (id.equals(p.getId())) return p;
        }
        return null;
    }

    /**
     * Logs game over with winner and final scores.
     */
    public void logGameOver(String winnerId, String winnerName, List<Player> players) {
        StringBuilder sb = new StringBuilder();
        sb.append("\n================================================================================\n");
        sb.append("GAME OVER - ").append(LocalDateTime.now().format(DATE_FORMAT)).append("\n");
        sb.append("================================================================================\n\n");

        sb.append("WINNER: ").append(winnerName).append("\n\n");

        sb.append("FINAL STANDINGS:\n");
        // Sort by treasures found (descending)
        List<Player> sorted = players.stream()
            .sorted((a, b) -> b.getTreasuresFound().size() - a.getTreasuresFound().size())
            .toList();

        int rank = 1;
        for (Player p : sorted) {
            int pNum = playerNumbers.getOrDefault(p.getId(), rank);
            sb.append(String.format("  %d. P%d %s - %d treasures collected\n",
                rank++,
                pNum,
                p.getName(),
                p.getTreasuresFound().size()));
        }

        sb.append("\nTotal moves: ").append(moveNumber).append("\n");
        sb.append("================================================================================\n");

        writeToFile(sb.toString());

        // Close the file after game over
        closeCurrentFile();
        log.info("Game log saved to: {}", currentLogFile);
    }

    /**
     * Resets the logger for a new game.
     */
    public void reset() {
        closeCurrentFile();
        moveNumber = 0;
        lastPushPlayerId = null;
        lastTurnState = null;
        lastPlayerPositions.clear();
        lastTreasureCounts.clear();
        playerNumbers.clear();
        lastLoggedPush = null;
    }

    // ======================== HELPER METHODS ========================

    private String formatBoard(Board board, List<Player> players) {
        StringBuilder sb = new StringBuilder();
        int width = board.getWidth();
        int height = board.getHeight();
        Tile[][] tiles = board.getTiles();

        // Top border
        sb.append("┌");
        for (int col = 0; col < width; col++) {
            sb.append("───");
            if (col < width - 1) sb.append("┬");
        }
        sb.append("┐\n");

        // Rows
        for (int row = 0; row < height; row++) {
            sb.append("│");
            for (int col = 0; col < width; col++) {
                Tile tile = tiles[row][col];
                String tileStr = formatTileWithPlayers(tile, players, row, col);
                sb.append(" ").append(tileStr).append(" ");
                if (col < width - 1) sb.append("│");
            }
            sb.append("│\n");

            // Row separator
            if (row < height - 1) {
                sb.append("├");
                for (int col = 0; col < width; col++) {
                    sb.append("───");
                    if (col < width - 1) sb.append("┼");
                }
                sb.append("┤\n");
            }
        }

        // Bottom border
        sb.append("└");
        for (int col = 0; col < width; col++) {
            sb.append("───");
            if (col < width - 1) sb.append("┴");
        }
        sb.append("┘\n");

        return sb.toString();
    }

    private String formatBoardTextual(Board board, List<Player> players) {
        StringBuilder sb = new StringBuilder();
        int width = board.getWidth();
        int height = board.getHeight();
        Tile[][] tiles = board.getTiles();

        for (int row = 0; row < height; row++) {
            sb.append(String.format("  Row %d: ", row));
            for (int col = 0; col < width; col++) {
                Tile tile = tiles[row][col];

                // Check for player on this tile
                String playerMarker = "";
                if (players != null) {
                    for (Player p : players) {
                        if (p.getCurrentPosition() != null &&
                            p.getCurrentPosition().getRow() == row &&
                            p.getCurrentPosition().getColumn() == col) {
                            int pNum = playerNumbers.getOrDefault(p.getId(), 1);
                            playerMarker = "P" + pNum + ":";
                            break;
                        }
                    }
                }

                String exits = formatExits(tile);
                sb.append(String.format("%s[%s]", playerMarker, exits));

                if (col < width - 1) sb.append(" ");
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    private String formatExits(Tile tile) {
        if (tile == null || tile.getEntrances() == null) return "?";

        Direction[] entrances = tile.getEntrances();
        StringBuilder exits = new StringBuilder();

        // Use direction names: UP, DOWN, LEFT, RIGHT
        boolean first = true;
        if (hasDirection(entrances, Direction.UP)) {
            exits.append("UP");
            first = false;
        }
        if (hasDirection(entrances, Direction.DOWN)) {
            if (!first) exits.append(",");
            exits.append("DOWN");
            first = false;
        }
        if (hasDirection(entrances, Direction.LEFT)) {
            if (!first) exits.append(",");
            exits.append("LEFT");
            first = false;
        }
        if (hasDirection(entrances, Direction.RIGHT)) {
            if (!first) exits.append(",");
            exits.append("RIGHT");
        }

        return exits.toString();
    }

    private String formatTileWithPlayers(Tile tile, List<Player> players, int row, int col) {
        // Check if any player is on this tile
        if (players != null) {
            for (Player p : players) {
                if (p.getCurrentPosition() != null &&
                    p.getCurrentPosition().getRow() == row &&
                    p.getCurrentPosition().getColumn() == col) {
                    int pNum = playerNumbers.getOrDefault(p.getId(), 1);
                    return String.valueOf(pNum); // Show player number
                }
            }
        }
        return String.valueOf(getTileShape(tile));
    }

    private char getTileShape(Tile tile) {
        if (tile == null || tile.getEntrances() == null) return '?';

        Direction[] entrances = tile.getEntrances();
        boolean hasUp = hasDirection(entrances, Direction.UP);
        boolean hasDown = hasDirection(entrances, Direction.DOWN);
        boolean hasLeft = hasDirection(entrances, Direction.LEFT);
        boolean hasRight = hasDirection(entrances, Direction.RIGHT);

        int count = entrances.length;

        if (count == 4) {
            return CROSS; // ┼
        } else if (count == 3) {
            // T-shape - determine orientation by which direction is missing
            if (!hasUp) return T_DOWN;      // ┬ (missing up = open down, left, right)
            if (!hasDown) return T_UP;      // ┴ (missing down = open up, left, right)
            if (!hasLeft) return T_RIGHT;   // ├ (missing left = open up, down, right)
            if (!hasRight) return T_LEFT;   // ┤ (missing right = open up, down, left)
        } else if (count == 2) {
            // I-shape (straight) or L-shape (corner)
            if (hasUp && hasDown) return I_VERTICAL;      // │
            if (hasLeft && hasRight) return I_HORIZONTAL; // ─

            // L-shapes
            if (hasUp && hasRight) return L_UP_RIGHT;     // └
            if (hasUp && hasLeft) return L_UP_LEFT;       // ┘
            if (hasDown && hasRight) return L_DOWN_RIGHT; // ┌
            if (hasDown && hasLeft) return L_DOWN_LEFT;   // ┐
        }
        return '?';
    }

    private boolean hasDirection(Direction[] entrances, Direction dir) {
        for (Direction d : entrances) {
            if (d == dir) return true;
        }
        return false;
    }

    private String formatPlayerInfo(Player player, int playerNum) {
        String colorStr = player.getColor() != null ? player.getColor().toString() : "?";
        String posStr = player.getCurrentPosition() != null ?
            formatPosition(player.getCurrentPosition()) : "(?:?)";
        String targetStr = player.getCurrentTargetTreasure() != null ?
            String.valueOf(player.getCurrentTargetTreasure().getId()) : "?";

        return String.format("  P%d (%s) [%s] at %s - Target: %s - Remaining: %d\n",
            playerNum,
            player.getName(),
            colorStr,
            posStr,
            targetStr,
            player.getRemainingTreasureCount());
    }

    private String formatPushNotation(int rowOrColIndex, Direction direction) {
        // Format: R3→ (Row 3 Right) or C5↓ (Col 5 Down)
        String arrow = switch (direction) {
            case UP -> "↑";
            case DOWN -> "↓";
            case LEFT -> "←";
            case RIGHT -> "→";
        };

        boolean isRow = (direction == Direction.LEFT || direction == Direction.RIGHT);
        String prefix = isRow ? "R" : "C";
        String description = isRow ? "Row" : "Col";
        String dirName = direction.toString().charAt(0) + direction.toString().substring(1).toLowerCase();

        return String.format("%s%d%s (%s %d %s)", prefix, rowOrColIndex, arrow, description, rowOrColIndex, dirName);
    }

    private String formatPosition(Position pos) {
        if (pos == null) return "(?:?)";
        return String.format("(%d:%d)", pos.getRow(), pos.getColumn());
    }
}
