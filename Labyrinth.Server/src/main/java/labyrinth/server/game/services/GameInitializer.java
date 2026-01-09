package labyrinth.server.game.services;

import labyrinth.server.game.enums.GameLogType;
import labyrinth.server.game.factories.BonusFactory;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Handles game initialization logic including treasure distribution,
 * bonus placement, and player positioning.
 *
 * Responsibilities:
 * - Treasure card distribution (round-robin)
 * - Bonus placement on board
 * - Player position initialization
 * - Game start logging
 */
public class GameInitializer {

    /**
     * Distributes treasure cards to players in round-robin fashion and places them on the board.
     *
     * @param treasureCards the list of treasure cards to distribute
     * @param board         the board to place treasures on
     * @param players       the players to assign cards to
     */
    public void distributeTreasureCards(List<TreasureCard> treasureCards, Board board, List<Player> players) {
        System.out.println("[TREASURE DEBUG] Distributing " + treasureCards.size() + " treasures to " + players.size() + " players");

        var playerToAssignCardsToIndex = 0;
        while (!treasureCards.isEmpty()) {
            TreasureCard card = treasureCards.getFirst();
            board.placeRandomTreasure(card);

            Player player = players.get(playerToAssignCardsToIndex);
            player.addTreasureCard(card);  // Use package-private method
            System.out.println("[TREASURE DEBUG] Assigned '" + card.getTreasureName() + "' to " + player.getUsername());

            playerToAssignCardsToIndex++;
            if (playerToAssignCardsToIndex >= players.size()) {
                playerToAssignCardsToIndex = 0;
            }

            treasureCards.removeFirst();
        }

        // Verify distribution
        for (Player p : players) {
            System.out.println("[TREASURE DEBUG] " + p.getUsername() + " has " + p.getAssignedTreasureCards().size() + " treasures");
        }
    }

    /**
     * Places bonuses randomly on the board.
     *
     * @param board      the board to place bonuses on
     * @param bonusCount number of bonuses to create
     */
    public void placeBonuses(Board board, int bonusCount) {
        BonusFactory bonusFactory = new BonusFactory();
        var bonuses = bonusFactory.createBonuses(bonusCount);
        board.placeRandomBonuses(bonuses);
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
