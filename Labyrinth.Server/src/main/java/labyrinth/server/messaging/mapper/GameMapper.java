package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.*;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.TreasureCard;
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
    private final TreasureMapper treasureMapper;
    private final BonusMapper bonusMapper;
    private final DirectionMapper directionMapper;

    public GameStateEventPayload toGameStateDto(Game game) {


        var gameBoard = game.getBoard();
        var players = game.getPlayers();

        var playerStates = new ArrayList<PlayerState>();
        for (var player : players) {
            var playersPosition = coordinatesMapper.toDto(game.getCurrentPositionOfPlayer(player));

            var remainingTreasureCards = (int) player.getAssignedTreasureCards()
                    .stream()
                    .filter(x -> !x.isCollected())
                    .count();

            var foundTreasures = player.getAssignedTreasureCards()
                    .stream()
                    .filter(TreasureCard::isCollected)
                    .map(treasureMapper::toDto)
                    .toArray(Treasure[]::new);

            var homeTile = player.getHomeTile();
            var homeTilePosition = coordinatesMapper.toDto(gameBoard.getPositionOfTile(homeTile));
            var bonus = player.getBonuses()
                    .stream()
                    .map(bonusMapper::toDto)
                    .toArray(BonusType[]::new);

            var playerState = new PlayerState();
            playerState.setPlayerInfo(playerMapper.toDto(player));
            playerState.setCurrentPosition(playersPosition);
            playerState.setRemainingTreasureCount(remainingTreasureCards);
            playerState.setTreasuresFound(foundTreasures);
            playerState.setHomePosition(homeTilePosition);
            playerState.setAvailableBonuses(bonus);

            // TODO: playerState.setAchievements();

            playerStates.add(playerState);
        }

        var currentTurnInfo = new CurrentTurnInfo();
        currentTurnInfo.setCurrentPlayerId(game.getCurrentPlayer().getId().toString());
        currentTurnInfo.setState(turnStateMapper.toDto(game.getCurrentMoveState()));
        currentTurnInfo.setTurnEndTime(game.getTurnEndTime());

        var gameState = new GameStateEventPayload();
        gameState.setType(EventType.GAME_STATE_UPDATE);
        gameState.setPlayers(playerStates.toArray(PlayerState[]::new));
        gameState.setCurrentTurnInfo(currentTurnInfo);

        var boardDto = gameBoardMapper.toDto(gameBoard);

        // Set lastPush for client-side AI to avoid forbidden reverse push
        game.getLastShift().ifPresent(lastShift -> {
            var pushInfo = new PushActionInfo();
            pushInfo.setRowOrColIndex(lastShift.index());
            pushInfo.setDirection(directionMapper.toDto(lastShift.direction()));
            boardDto.setLastPush(pushInfo);
        });

        gameState.setBoard(boardDto);
        gameState.setGameEndTime(game.getGameEndTime());

        return gameState;
    }
}
