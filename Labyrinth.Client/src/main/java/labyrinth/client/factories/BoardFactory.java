package labyrinth.client.factories;

import labyrinth.client.abstractions.IBoardFactory;
import labyrinth.client.models.*;
import labyrinth.client.enums.*;

import java.security.DigestException;
import java.util.*;

/**
 * Factory class to generate random labyrinth boards.
 */
public class BoardFactory implements IBoardFactory {

    private static final Random RANDOM = new Random();


    @Override
    public Board createBoardForGame(Game game) {
        var width = game.getBoardWidth();
        var height = game.getBoardHeight();
        Tile[][] tiles = new Tile[height][width];

        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile;

                // Since the board is not always uneven we need a more sophisticated logic
                // than % 2. If one direction is even, keep the middle two fixed and every
                // second from there on also
                boolean rowFixed = shouldBeFixed(row, height);
                boolean colFixed = shouldBeFixed(col, width);

                if (rowFixed && colFixed) {
                    // Fixed tiles should always have 3 entrances
                    // Fixed tiles on the outside should always look to the inside
                    if(row == 0) {
                        tile = new Tile(EnumSet.of(Direction.DOWN, Direction.RIGHT, Direction.LEFT));
                    } else if(col == 0) {
                        tile = new Tile(EnumSet.of(Direction.DOWN, Direction.UP, Direction.RIGHT));
                    } else if(row == height - 1) {
                        tile = new Tile(EnumSet.of(Direction.UP, Direction.LEFT, Direction.RIGHT));
                    }  else if(col == width - 1) {
                        tile = new Tile(EnumSet.of(Direction.DOWN, Direction.UP, Direction.LEFT));
                    } else {
                        tile = new Tile(EnumSet.of(Direction.DOWN, Direction.RIGHT, Direction.LEFT));
                        int rotations = RANDOM.nextInt(4);
                        for (int i = 0; i < rotations; i++) {
                            tile.rotate();
                        }
                    }
                    tile.setFixed(true);
                } else {
                    tile = createRandomTile();
                }

                tiles[row][col] = tile;
            }
        }

        // Replace corner tiles, maybe we can do the in the loop already, but rn im too lazy
        tiles[0][0] = new Tile(EnumSet.of(Direction.DOWN, Direction.RIGHT));
        tiles[0][0].setFixed(true);
        tiles[0][width-1] = new Tile(EnumSet.of(Direction.DOWN, Direction.LEFT));
        tiles[0][width-1].setFixed(true);
        tiles[height-1][0] = new Tile(EnumSet.of(Direction.UP, Direction.RIGHT));
        tiles[height-1][0].setFixed(true);
        tiles[height-1][width-1] = new Tile(EnumSet.of(Direction.UP, Direction.LEFT));
        tiles[height-1][width-1].setFixed(true);

        return new Board(width, height, tiles,createRandomTile());
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

        Tile tile = new Tile(entrances);

        // Rotate randomly 0–3 times
        int rotations = RANDOM.nextInt(4);
        for (int i = 0; i < rotations; i++) {
            tile.rotate();
        }

        return tile;
    }

    private static boolean shouldBeFixed(int index, int dimension) {
        if (dimension % 2 == 0) {
            int mid1 = dimension / 2 - 1;
            int mid2 = dimension / 2;

            if (index == mid1 || index == mid2) {
                return true;
            }

            if (index < mid1) {
                return index % 2 == 0;
            }

            if (index > mid2) {
                return (index - mid2) % 2 == 0;
            }
        } else {
            return index % 2 == 0;
        }

        return false;
    }
}
