package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.RankingEntry;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.events.records.GameOverEvent;
import labyrinth.server.messaging.mapper.RankingMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Component
@RequiredArgsConstructor
public class GameOverEventListener {
    private static final Logger log = LoggerFactory.getLogger(GameOverEventListener.class);

    private final MessageService messageService;
    private final RankingMapper rankingMapper;


    @EventListener
    public void onEvent(GameOverEvent event) {
        try {

            var ranking = new ArrayList<RankingEntry>();

            List<Player> sortedPlayers = event.players().stream()
                    .sorted(Comparator.comparingInt((Player p) -> p.getStatistics().getScore())
                            .reversed())
                    .toList();

            var payload = new GameOverEventPayload();
            var rank = 1;
            for (Player p : sortedPlayers) {

                if(rank == 1) {
                    payload.setWinnerId(p.getId().toString());
                }

                var entry = rankingMapper.toDto(p, rank);

                ranking.add(entry);
                rank++;
            }

            payload.setType(EventType.GAME_OVER);
            payload.setRanking(ranking.toArray(RankingEntry[]::new));

            messageService.broadcastToPlayers(payload);
        } catch (Exception ex) {
            log.error("Error while handling GameOverEvent", ex);
        }
    }


}
