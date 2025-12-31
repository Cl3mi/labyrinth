package labyrinth.server.game.services;

import labyrinth.server.game.enums.GameLogType;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.records.GameLogEntry;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Handles game action logging and state serialization.
 * Provides an immutable log of all game actions for replay and debugging.
 */
public class GameLogger {

    private static final Logger LOGGER = Logger.getLogger(GameLogger.class.getName());
    private final List<GameLogEntry> executionLogs = new ArrayList<>();

    /**
     * Logs a game action with metadata.
     *
     * @param type     the type of game action
     * @param message  human-readable description
     * @param player   the player involved (may be null for game-level events)
     * @param metadata additional key-value data for replay
     */
    public void log(GameLogType type, String message, Player player, Map<String, String> metadata) {
        var entry = new GameLogEntry(
                OffsetDateTime.now(),
                type,
                player != null ? player.getId().toString() : null,
                message,
                metadata);
        executionLogs.add(entry);
        LOGGER.info(message);
    }

    /**
     * Returns an immutable view of all logged game actions.
     *
     * @return unmodifiable list of log entries
     */
    public List<GameLogEntry> getExecutionLogs() {
        return Collections.unmodifiableList(executionLogs);
    }

    /**
     * Serializes a tile to a string representation for logging.
     *
     * @param tile the tile to serialize
     * @return string representation of the tile
     */
    public String serializeTile(Tile tile) {
        if (tile == null) {
            return "null";
        }
        StringBuilder sb = new StringBuilder();
        sb.append("entrances=").append(tile.getEntrances());
        if (tile.getTreasureCard() != null) {
            sb.append(",treasure=").append(tile.getTreasureCard().getTreasureName());
        }
        if (tile.getBonus() != null) {
            sb.append(",bonus=").append(tile.getBonus());
        }
        if (tile.isFixed()) {
            sb.append(",fixed=true");
        }
        return sb.toString();
    }

    /**
     * Serializes the entire board state for logging.
     *
     * @param board the board to serialize
     * @return string representation of the board
     */
    public String serializeBoard(Board board) {
        StringBuilder sb = new StringBuilder();
        sb.append("width=").append(board.getWidth()).append(";");
        sb.append("height=").append(board.getHeight()).append(";");
        sb.append("extraTile=").append(serializeTile(board.getExtraTile())).append(";");

        for (int r = 0; r < board.getHeight(); r++) {
            for (int c = 0; c < board.getWidth(); c++) {
                Tile t = board.getTileAt(r, c);
                sb.append("tile_").append(r).append("_").append(c).append("=").append(serializeTile(t)).append(";");
            }
        }
        return sb.toString();
    }
}
