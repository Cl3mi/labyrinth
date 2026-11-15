package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.RankingEntry;
import labyrinth.server.game.events.GameOverEvent;
import labyrinth.server.game.services.PlayerRanking;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.RankingMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.util.ArrayList;

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
            var payload = new GameOverEventPayload();

            for (var ranked : PlayerRanking.rank(event.players())) {
                if (ranked.placement() == 1) {
                    payload.setWinnerId(ranked.player().getId().toString());
                }
                ranking.add(rankingMapper.toDto(ranked.player(), ranked.placement()));
            }

            payload.setType(EventType.GAME_OVER);
            payload.setRanking(ranking.toArray(RankingEntry[]::new));
            messageService.broadcastToPlayers(payload);
        } catch (Exception ex) {
            log.error("Error while handling GameOverEvent", ex);
        }
    }
}
