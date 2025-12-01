package labyrinth.server.game;


import labyrinth.contracts.models.*;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.mapper.GameBoardMapper;
import labyrinth.server.messaging.mapper.PlayerInfoMapper;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
public class GameService {

    private final Game game;
    private final PlayerInfoMapper playerMapper;
    private final GameBoardMapper gameBoardMapper;
    private final List<Player> players = new ArrayList<>();

    public GameService(Game lobby,
                       PlayerInfoMapper playerMapper,
                       GameBoardMapper gameBoardMapper) {
        this.game = lobby;
        this.playerMapper = playerMapper;
        this.gameBoardMapper = gameBoardMapper;
    }


    public Player connectPlayer(String username) {
        Player player = new Player(UUID.randomUUID(), username);
        player.setJoinDate(OffsetDateTime.now());
        player.setColor(getNextColor());
        players.add(player);

        return game.join(player);
    }

    public void disconnectPlayer(UUID playerId) throws Exception{
        var player = getPlayer(playerId);

        if(player == null) {
            throw new Exception("Player with ID " + playerId + " not found");
        }
        game.leave(player);
    }

    public List<Player> getPlayersInLobby() {
        return game.getPlayers();
    }

    public void removePlayer(UUID playerId) {
        game.removePlayer(playerId);
    }

    public Player getPlayer(UUID playerId) {
        return players.stream()
                .filter(p -> p.getId().equals(playerId))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Player not found"));
    }

    private PlayerColor getNextColor() {
        for (PlayerColor color : PlayerColor.values()) {
            boolean used = players.stream()
                    .anyMatch(p -> p.getColor() == color);
            if (!used) {
                return color;
            }
        }
        throw new IllegalStateException("No available colors left");
    }

    //TODO: is this the correct place for this method?
    public GameStateUpdateEventPayload getGameState() {
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
