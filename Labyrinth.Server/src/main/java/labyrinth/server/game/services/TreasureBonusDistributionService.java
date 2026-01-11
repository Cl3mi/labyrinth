package labyrinth.server.game.services;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.factories.BonusFactory;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.Position;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * Service responsible for distributing treasures and bonuses on the game board.
 * Ensures that:
 * - Neither treasures nor bonuses are placed on corner tiles (home positions)
 * - A tile can have either a treasure OR a bonus, but not both
 * - Distribution only proceeds if there are enough viable positions
 */
@Service
public class TreasureBonusDistributionService {

    private static final Random RANDOM = new Random();
    private static final java.util.logging.Logger LOGGER =
            java.util.logging.Logger.getLogger(TreasureBonusDistributionService.class.getName());

    private final BonusFactory bonusFactory;

    public TreasureBonusDistributionService(BonusFactory bonusFactory) {
        this.bonusFactory = bonusFactory;
    }

    /**
     * Distributes treasures and bonuses on the board.
     * Treasures are distributed first, then bonuses in the remaining free spaces.
     *
     * @param board the board to place items on
     * @param treasureCards the treasure cards to place
     * @param bonusCount the number of bonuses to create and place
     * @throws IllegalStateException if there are not enough viable positions
     */
    public void distributeAll(Board board, List<TreasureCard> treasureCards, int bonusCount) {
        List<Position> viablePositions = getViablePositions(board);

        if (treasureCards.size() > viablePositions.size()) {
            throw new IllegalStateException(
                "Not enough viable positions for treasures. Need " + treasureCards.size() +
                " but only have " + viablePositions.size() + " positions available"
            );
        }

        Collections.shuffle(viablePositions, RANDOM);

        distributeTreasures(board, treasureCards, viablePositions);

        List<Position> remainingPositions = getFreePositions(board);

        if (bonusCount > remainingPositions.size()) {
            throw new IllegalStateException(
                "Not enough free positions for bonuses. Need " + bonusCount +
                " but only have " + remainingPositions.size() + " positions available"
            );
        }

        distributeBonuses(board, bonusCount, remainingPositions);
    }

    /**
     * Gets all viable positions on the board (excluding corners).
     */
    private List<Position> getViablePositions(Board board) {
        List<Position> positions = new ArrayList<>();

        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                if (!isCornerPosition(row, col, board)) {
                    positions.add(new Position(row, col));
                }
            }
        }

        return positions;
    }

    /**
     * Gets all free positions on the board (no treasure, no bonus, not a corner).
     */
    private List<Position> getFreePositions(Board board) {
        List<Position> positions = new ArrayList<>();

        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                if (isCornerPosition(row, col, board)) {
                    continue;
                }

                Tile tile = board.getTileAt(row, col);
                if (tile != null && tile.getTreasureCard() == null && tile.getBonus() == null) {
                    positions.add(new Position(row, col));
                }
            }
        }

        return positions;
    }

    /**
     * Distributes treasure cards on the board at the given positions.
     */
    private void distributeTreasures(Board board, List<TreasureCard> treasureCards, List<Position> positions) {
        for (int i = 0; i < treasureCards.size(); i++) {
            Position pos = positions.get(i);
            TreasureCard card = treasureCards.get(i);

            Tile tile = board.getTileAt(pos);
            if (tile != null) {
                tile.setTreasureCard(card);
                LOGGER.info("Placed treasure '" + card.getTreasureName() + "' at " + pos.row() + "/" + pos.column());
            }
        }
    }

    /**
     * Distributes bonuses on the board at the given free positions.
     */
    private void distributeBonuses(Board board, int bonusCount, List<Position> freePositions) {
        List<BonusTypes> bonuses = bonusFactory.createBonuses(bonusCount);
        Collections.shuffle(freePositions, RANDOM);

        for (int i = 0; i < bonuses.size(); i++) {
            Position pos = freePositions.get(i);
            BonusTypes bonus = bonuses.get(i);

            Tile tile = board.getTileAt(pos);
            if (tile != null) {
                tile.setBonus(bonus);
                LOGGER.info("Placed bonus " + bonus + " at " + pos.row() + "/" + pos.column());
            }
        }
    }

    /**
     * Checks if a position is a corner position (home tile).
     */
    private boolean isCornerPosition(int row, int col, Board board) {
        return (row == 0 && col == 0) ||
               (row == 0 && col == board.getWidth() - 1) ||
               (row == board.getHeight() - 1 && col == 0) ||
               (row == board.getHeight() - 1 && col == board.getWidth() - 1);
    }
}
