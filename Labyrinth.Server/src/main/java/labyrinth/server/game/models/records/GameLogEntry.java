package labyrinth.server.game.models.records;

import labyrinth.server.game.enums.GameLogType;

import java.time.OffsetDateTime;
import java.util.Map;

public record GameLogEntry(
        OffsetDateTime timestamp,
        GameLogType type,
        String playerId,
        String message,
        Map<String, String> metadata) {
}
