package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.GameStateUpdateEventPayload;
import labyrinth.contracts.models.PlayerState;
import labyrinth.server.game.abstractions.IGame;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.ArrayList;

@Component
@RequiredArgsConstructor
public class GameMapper {
    private final PlayerInfoMapper playerMapper;
    private final GameBoardMapper gameBoardMapper;

    public GameStateUpdateEventPayload toGameStateDto(IGame game) {
        var gameState = new GameStateUpdateEventPayload();
        gameState.setType(EventType.GAME_STATE_UPDATE);

        var players = game.getPlayers()
                .stream()
                .map(playerMapper::toDto)
                .toList();

        var playerStates = new ArrayList<PlayerState>();
        for(var player : players) {
            var playerState = new PlayerState();
            playerState.setPlayerInfo(player);
            // TODO: playerState.setAchievements();
            // TODO: playerState.setAvailableBonuses();
            // TODO: playerState.setCurrentPosition();
            // TODO: playerState.setCurrentTreasure();
            // TODO: playerState.setHomePosition();
            // TODO: playerState.setRemainingTreasureCount();
            // TODO: playerState.setAvailableBonuses();

            playerStates.add(playerState);
        }

        gameState.setPlayers(playerStates.toArray(PlayerState[]::new));
        gameState.setBoard(gameBoardMapper.toDto(game.getBoard()));
        //TODO gameState.setCurrentTurnState();
        //TODO gameState.setCurrentPlayerId();

        return gameState;
    }
}
