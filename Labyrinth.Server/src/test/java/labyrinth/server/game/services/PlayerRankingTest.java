package labyrinth.server.game.services;

import labyrinth.server.game.models.Player;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PlayerRankingTest {

    private static Player player(String name, int score, int treasures, int steps) {
        var p = new Player(UUID.randomUUID(), name);
        p.getStatistics().increaseScore(score);
        p.getStatistics().increaseTreasuresCollected(treasures);
        p.getStatistics().increaseStepsTaken(steps);
        return p;
    }

    @Test
    void ordersByScoreThenTreasuresThenFewerSteps() {
        var a = player("a", 100, 2, 10);
        var b = player("b", 200, 1, 10);
        var c = player("c", 100, 3, 10);
        var d = player("d", 100, 3, 5);

        var ranked = PlayerRanking.rank(List.of(a, b, c, d));

        assertEquals(List.of("b", "d", "c", "a"), ranked.stream().map(r -> r.player().getUsername()).toList());
        assertEquals(1, ranked.get(0).placement());
        assertEquals(2, ranked.get(1).placement());
        // c has the same score and treasures as d, so it shares placement 2 despite more steps
        assertEquals(2, ranked.get(2).placement());
        assertEquals(4, ranked.get(3).placement());
    }

    @Test
    void sharedFirstPlace() {
        var ranked = PlayerRanking.rank(List.of(player("a", 50, 1, 1), player("b", 50, 1, 9)));
        assertEquals(1, ranked.get(0).placement());
        assertEquals(1, ranked.get(1).placement());
    }
}
