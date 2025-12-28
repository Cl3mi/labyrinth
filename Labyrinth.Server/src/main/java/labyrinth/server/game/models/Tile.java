package labyrinth.server.game.models;

import labyrinth.server.game.constants.PointRewards;
import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import lombok.Getter;
import lombok.Setter;

import java.awt.*;
import java.util.EnumSet;
import java.util.Set;

/**
 * Represents a single tile in the Labyrinth board.
 * Each tile has entrances in specific directions and may contain a treasure.
 */
public class Tile {

    private final EnumSet<Direction> entrances;

    @Getter
    private boolean isFixed;
    @Setter
    @Getter
    private TreasureCard treasureCard;

    @Setter
    @Getter
    private BonusTypes bonus;

    /**
     * Creates a tile with specified entrances and optional treasure.
     *
     * @param entrances directions where the tile has openings (at least 2)
     */
    public Tile(Set<Direction> entrances) {
        if (entrances.size() < 2) {
            throw new IllegalArgumentException("A tile must have at least 2 entrances.");
        }
        this.entrances = EnumSet.copyOf(entrances);
    }

    public Set<Direction> getEntrances() {
        return EnumSet.copyOf(entrances);
    }

    public void setIsFixed(boolean isFixed) {
        this.isFixed = isFixed;
    }

    public Tile copy() {
        Tile newTile = new Tile(this.entrances);
        newTile.setIsFixed(this.isFixed);
        newTile.setTreasureCard(this.treasureCard); // Reference copy is fine for treasure card as it's not mutated
                                                    // during simulation
        return newTile;
    }

    /**
     * Rotates the tile 90 degrees clockwise.
     * Updates the entrances accordingly.
     */
    public void rotate() {
        EnumSet<Direction> rotated = EnumSet.noneOf(Direction.class);
        for (Direction dir : entrances) {
            rotated.add(switch (dir) {
                case UP -> Direction.RIGHT;
                case RIGHT -> Direction.DOWN;
                case DOWN -> Direction.LEFT;
                case LEFT -> Direction.UP;
            });
        }
        entrances.clear();
        entrances.addAll(rotated);
    }

    /**
     * Checks if this tile is connected to another tile.
     * Two tiles are connected if they have matching entrances in opposite
     * directions.
     *
     * @param neighbor  the neighboring tile to check connection with
     * @param direction the direction from this tile to the neighbor
     * @return true if connected, false otherwise
     */
    public boolean isConnectedTo(Tile neighbor, Direction direction) {
        return entrances.contains(direction) && neighbor.getEntrances().contains(direction.opposite());
    }

    /**
     * @deprecated Use MovementManager#processPlayerStepOnTile instead.
     */
    @Deprecated(forRemoval = true)
    public void getSteppedOnBy(Player player) {
        if (treasureCard != null) {
            if (player.getCurrentTreasureCard() == treasureCard) {
                System.out.println("Card: " + treasureCard.getTreasureName());
                treasureCard.collect();
                player.getStatistics().increaseScore(PointRewards.REWARD_TREASURE);
                player.getStatistics().increaseTreasuresCollected(1);

                this.treasureCard = null;
            }
        }

        if (bonus != null) {
            System.out.println("Bonus collected: " + bonus);
            player.getBonuses().add(bonus);
            this.bonus = null;
        }

        player.setCurrentTile(this);
    }

    @Override
    public String toString() {
        return "Tile{" +
                "entrances=" + entrances +
                '}';
    }
}