package labyrinth.server.persistence;

import java.util.Map;
import java.util.UUID;

public interface SessionTokenSource {
    Map<UUID, UUID> identifierTokensByPlayerId();
}
