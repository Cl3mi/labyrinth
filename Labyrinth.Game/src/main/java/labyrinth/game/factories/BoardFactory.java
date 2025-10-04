package labyrinth.game.factories;

import labyrinth.game.models.*;
import labyrinth.game.enums.*;

import java.util.*;

/**
 * Factory class to generate random labyrinth boards.
 */
public class BoardFactory {

    private static final Random RANDOM = new Random();

    /**
     * Generates a random board with given dimensions.
     * Each tile is randomly selected from corner, straight, or T-junction shapes.
     *
     * @param width  number of columns
     * @param height number of rows
     * @return generated Board
     */
    public static Board createRandomBoard(int width, int height) {
        Tile[][] tiles = new Tile[height][width];

        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile = createRandomTile();
                tiles[row][col] = tile;
            }
        }

        return new Board(width, height, tiles);
    }

    /**
     * Creates a random tile (corner, straight, or T-junction) with random rotation.
     */
    private static Tile createRandomTile() {
        int type = RANDOM.nextInt(3); // 0=corner, 1=straight, 2=t-junction
        Set<Direction> entrances = switch (type) {
            case 0 -> EnumSet.of(Direction.UP, Direction.RIGHT); // corner
            case 1 -> EnumSet.of(Direction.UP, Direction.DOWN);  // straight
            case 2 -> EnumSet.of(Direction.UP, Direction.LEFT, Direction.RIGHT); // T-junction
            default -> throw new IllegalStateException("Unexpected tile type");
        };

        Tile tile = new Tile(entrances, RANDOM.nextDouble() < 0.2); // ~20% chance treasure

        // Rotate randomly 0–3 times
        int rotations = RANDOM.nextInt(4);
        for (int i = 0; i < rotations; i++) {
            tile.rotate();
        }

        return tile;
    }
}
