package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.PlayerGameStats;
import labyrinth.contracts.models.RankingEntry;
import labyrinth.server.game.models.Player;
import org.springframework.stereotype.Component;

@Component
public class RankingMapper {
    public RankingEntry toDto(Player player, int rank) {
        var entry = new RankingEntry();

        var sourceStats = player.getStatistics();

        entry.setPlayerId(player.getId().toString());
        // playerName is not in standard Contracts - use additionalProperties
        var additionalProps = new java.util.HashMap<String, Object>();
        additionalProps.put("playerName", player.getUsername());
        entry.setAdditionalProperties(additionalProps);
        entry.setScore(sourceStats.getScore());
        entry.setRank(rank);

        var targetStats = new PlayerGameStats();
        targetStats.setStepsTaken(sourceStats.getStepsTaken());
        targetStats.setTilesPushed(sourceStats.getTilesPushed());
        targetStats.setTreasuresCollected(sourceStats.getTreasuresCollected());
        entry.setStats(targetStats);

        return entry;

    }
}
