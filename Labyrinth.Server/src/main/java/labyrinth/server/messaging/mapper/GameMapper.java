package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.GameStateUpdateEventPayload;
import labyrinth.contracts.models.PlayerState;
import labyrinth.server.game.models.Game;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.ArrayList;

@Component
@RequiredArgsConstructor
public class GameMapper {

    private final PlayerInfoMapper playerMapper;
    private final GameBoardMapper gameBoardMapper;
    private final TurnStateMapper turnStateMapper;
    private final CoordinatesMapper coordinatesMapper;

    public GameStateUpdateEventPayload toGameStateDto(Game game) {
        var gameState = new GameStateUpdateEventPayload();
        gameState.setType(EventType.GAME_STATE_UPDATE);

        var gameBoard = game.getBoard();
        var players = game.getPlayers();

        var playerStates = new ArrayList<PlayerState>();
        for (var player : players) {
            var playerState = new PlayerState();
            playerState.setPlayerInfo(playerMapper.toDto(player));

            var playersPosition = game.getCurrentPositionOfPlayer(player);
            playerState.setCurrentPosition(coordinatesMapper.toDto(playersPosition));

            // TODO: playerState.setAchievements();
            // TODO: playerState.setAvailableBonuses();
            // TODO: playerState.setCurrentTreasure();
            // TODO: playerState.setHomePosition();
            // TODO: playerState.setRemainingTreasureCount();
            // TODO: playerState.setAvailableBonuses();

            playerStates.add(playerState);
        }

        gameState.setPlayers(playerStates.toArray(PlayerState[]::new));
        gameState.setCurrentTurnState(turnStateMapper.toDto(game.getCurrentMoveState()));
        gameState.setCurrentPlayerId(game.getCurrentPlayer().getId().toString());
        gameState.setBoard(gameBoardMapper.toDto(gameBoard));

        return gameState;
    }
}
