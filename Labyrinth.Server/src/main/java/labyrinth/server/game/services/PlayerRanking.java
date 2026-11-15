package labyrinth.server.game.services;

import labyrinth.server.game.models.Player;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Final ranking of a game. Used for the GAME_OVER message and for the stored results,
 * so clients and database always agree on the placement.
 *
 * <p>Order: higher score first, then more treasures, then fewer steps.
 * Players with equal score and equal treasures share a placement.</p>
 */
public final class PlayerRanking {

    public record RankedPlayer(Player player, int placement) {
    }

    private static final Comparator<Player> ORDER = Comparator
            .comparingInt((Player p) -> p.getStatistics().getScore()).reversed()
            .thenComparing(Comparator.comparingInt((Player p) -> p.getStatistics().getTreasuresCollected()).reversed())
            .thenComparingInt(p -> p.getStatistics().getStepsTaken());

    private PlayerRanking() {
    }

    public static List<RankedPlayer> rank(List<Player> players) {
        var sorted = players.stream().sorted(ORDER).toList();
        var result = new ArrayList<RankedPlayer>(sorted.size());

        int placement = 1;
        int previousScore = -1;
        int previousTreasures = -1;

        for (int position = 1; position <= sorted.size(); position++) {
            var player = sorted.get(position - 1);
            int score = player.getStatistics().getScore();
            int treasures = player.getStatistics().getTreasuresCollected();

            if (position == 1 || score != previousScore || treasures != previousTreasures) {
                placement = position;
            }
            result.add(new RankedPlayer(player, placement));

            previousScore = score;
            previousTreasures = treasures;
        }
        return result;
    }
}
