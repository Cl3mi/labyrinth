package labyrinth.server.game.services;

import labyrinth.server.game.enums.GameLogType;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Handles game initialization logic including treasure distribution,
 * bonus placement, and player positioning.
 *
 * Responsibilities:
 * - Treasure card distribution (round-robin to players)
 * - Delegating treasure and bonus placement to TreasureBonusDistributionService
 * - Player position initialization
 * - Game start logging
 */
public class GameInitializerService {

    private final TreasureBonusDistributionService distributionService;
    private static final org.slf4j.Logger log = LoggerFactory.getLogger(GameInitializerService.class);

    public GameInitializerService(TreasureBonusDistributionService distributionService) {
        this.distributionService = distributionService;
    }

    /**
     * Distributes treasure cards to players in round-robin fashion and places them on the board.
     * Also places bonuses on the board using the distribution service.
     *
     * @param treasureCards the list of treasure cards to distribute
     * @param board         the board to place treasures and bonuses on
     * @param players       the players to assign cards to
     * @param bonusCount    number of bonuses to create and place
     */
    public void distributeTreasuresAndBonuses(List<TreasureCard> treasureCards, Board board, List<Player> players, int bonusCount) {
        log.debug("[TREASURE DEBUG] Distributing {} treasures and {} bonuses", treasureCards.size(), bonusCount);

        distributionService.distributeAll(board, treasureCards, bonusCount);

        var playerToAssignCardsToIndex = 0;
        for (TreasureCard card : treasureCards) {
            Player player = players.get(playerToAssignCardsToIndex);
            player.getAssignedTreasureCards().add(card);
            log.debug("[TREASURE DEBUG] Assigned '{}' to {}", card.getTreasureName(), player.getUsername());

            playerToAssignCardsToIndex++;
            if (playerToAssignCardsToIndex >= players.size()) {
                playerToAssignCardsToIndex = 0;
            }
        }

        for (Player p : players) {
            log.debug("[TREASURE DEBUG] {} has {} treasures", p.getUsername(), p.getAssignedTreasureCards().size());
        }
    }

    /**
     * Initializes player positions on the board based on the game configuration.
     * Sets both the home tile and current tile for each player.
     *
     * @param board      the board to place players on
     * @param gameConfig the game configuration containing start positions
     * @param players    the players to position
     */
    public void initializePlayerPositions(Board board, GameConfig gameConfig, List<Player> players) {
        for (int i = 0; i < players.size(); i++) {
            Player player = players.get(i);
            var position = gameConfig.getStartPosition(i);

            Tile startingTile = board.getTileAt(position);
            player.setHomeTile(startingTile);
            player.setCurrentTile(startingTile);
        }
    }

    /**
     * Logs the game start event with board state and player positions.
     *
     * @param board      the initialized board
     * @param gameConfig the game configuration
     * @param players    the players in the game
     * @param gameLogger the logger to use
     */
    public void logGameStart(Board board, GameConfig gameConfig, List<Player> players, GameLogger gameLogger) {
        Map<String, String> startMeta = new HashMap<>();
        startMeta.put("boardState", gameLogger.serializeBoard(board));

        for (Player p : players) {
            startMeta.put("player_" + p.getId(),
                    p.getUsername() + "@" + gameConfig.getStartPosition(players.indexOf(p)));
        }

        gameLogger.log(GameLogType.START_GAME, "Game started in GameLobby with " + players.size() + " players.", null,
                startMeta);
    }
}
