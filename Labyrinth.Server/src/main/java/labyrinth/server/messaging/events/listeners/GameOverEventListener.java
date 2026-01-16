package labyrinth.server.messaging.events.listeners;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.RankingEntry;
import labyrinth.server.game.GameService;
import labyrinth.server.game.events.GameOverEvent;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.MessageService;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import labyrinth.server.messaging.mapper.RankingMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class GameOverEventListener {
    private static final Logger log = LoggerFactory.getLogger(GameOverEventListener.class);

    private final MessageService messageService;
    private final RankingMapper rankingMapper;
    private final PlayerInfoMapper playerInfoMapper;
    private final GameService gameService;


    @EventListener
    public void onEvent(GameOverEvent event) {
        try {

            var ranking = new ArrayList<RankingEntry>();

            List<Player> sortedPlayers = event.players().stream()
                    .sorted((p1, p2) -> {

                        int scoreCompare = Integer.compare(p2.getStatistics().getScore(), p1.getStatistics().getScore());
                        if (scoreCompare != 0) return scoreCompare;

                        int treasuresCompare = Integer.compare(p2.getStatistics().getTreasuresCollected(), p1.getStatistics().getTreasuresCollected());
                        if (treasuresCompare != 0) return treasuresCompare;

                        return Integer.compare(p1.getStatistics().getStepsTaken(), p2.getStatistics().getStepsTaken());
                    })
                    .toList();

            var payload = new GameOverEventPayload();
            var rank = 1;
            int previousScore = -1;
            int previousTreasures = -1;
            int actualPosition = 1;

            for (Player p : sortedPlayers) {
                int currentScore = p.getStatistics().getScore();
                int currentTreasures = p.getStatistics().getTreasuresCollected();

                if (actualPosition <= 1 ||
                        currentScore != previousScore ||
                        currentTreasures != previousTreasures) {
                    rank = actualPosition;
                }

                if (rank == 1) {
                    payload.setWinnerId(p.getId().toString());
                }

                var entry = rankingMapper.toDto(p, rank);
                ranking.add(entry);

                previousScore = currentScore;
                previousTreasures = currentTreasures;
                actualPosition++;
            }

            payload.setType(EventType.GAME_OVER);
            payload.setRanking(ranking.toArray(RankingEntry[]::new));
            messageService.broadcastToPlayers(payload);

            var lobbyStateUpdated = new LobbyStateEventPayload();
            lobbyStateUpdated.setType(EventType.LOBBY_STATE);
            lobbyStateUpdated.setPlayers(gameService.getPlayers()
                    .stream()
                    .map(playerInfoMapper::toDto)
                    .toArray(labyrinth.contracts.models.PlayerInfo[]::new));

            messageService.broadcastToPlayers(lobbyStateUpdated);
        } catch (Exception ex) {
            log.error("Error while handling GameOverEvent", ex);
        }
    }
}
