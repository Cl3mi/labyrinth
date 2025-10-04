package labyrinth.game.factories;

import labyrinth.game.models.*;
import labyrinth.game.enums.*;

import java.security.DigestException;
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

                if ((row == 0 || row == height - 1) && (col == 0 || col == width - 1)) {
                    tile.setIsFixed(true);
                }
                else if (row == 0 || row == height - 1) {
                    if (width % 2 == 0) {
                        if (col == width / 2 - 1 || col == width / 2) {
                            tile.setIsFixed(true);
                        }
                    } else if (col % 2 == 0) {
                        tile.setIsFixed(true);
                    }
                }
                else if (col == 0 || col == width - 1) {
                    if (height % 2 == 0) {
                        if (row == height / 2 - 1 || row == height / 2) {
                            tile.setIsFixed(true);
                        }
                    } else if (row % 2 == 0) {
                        tile.setIsFixed(true);
                    }
                }

                tiles[row][col] = tile;
            }
        }

        // Replace corner tiles, maybe we can do the in the loop already
        tiles[0][0] = new Tile(EnumSet.of(Direction.DOWN, Direction.RIGHT), false);
        tiles[0][0].setIsFixed(true);
        tiles[0][width-1] = new Tile(EnumSet.of(Direction.DOWN, Direction.LEFT), false);
        tiles[0][width-1].setIsFixed(true);
        tiles[height-1][0] = new Tile(EnumSet.of(Direction.UP, Direction.RIGHT), false);
        tiles[height-1][0].setIsFixed(true);
        tiles[height-1][width-1] = new Tile(EnumSet.of(Direction.UP, Direction.LEFT), false);
        tiles[height-1][width-1].setIsFixed(true);




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
