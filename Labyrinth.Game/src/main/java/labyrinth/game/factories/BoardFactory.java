package labyrinth.game.factories;

import labyrinth.game.abstractions.IBoardFactory;
import labyrinth.game.models.*;
import labyrinth.game.enums.*;
import labyrinth.game.models.BiMap;
import labyrinth.game.models.Position;

import java.security.DigestException;
import java.util.*;

/**
 * Factory class to generate random labyrinth boards.
 */
public class BoardFactory implements IBoardFactory {

    private static final Random RANDOM = new Random();

    @Override
    public Board createBoardForGame(Game game) {
        var gameConfig = game.getGameConfig();
        var width = gameConfig.boardWidth();
        var height = gameConfig.boardHeight();
        BiMap<Position, Tile> tileMap = new BiMap<>();

        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile;

                boolean rowFixed = shouldBeFixed(row, height);
                boolean colFixed = shouldBeFixed(col, width);

                if (rowFixed && colFixed) {
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
                    tile.setIsFixed(true);
                } else {
                    tile = createRandomTile();
                }

                tileMap.put(new Position(row, col), tile);
            }
        }

        tileMap.put(new Position(0, 0), new Tile(EnumSet.of(Direction.DOWN, Direction.RIGHT)) {{ setIsFixed(true); }});
        tileMap.put(new Position(0, width-1), new Tile(EnumSet.of(Direction.DOWN, Direction.LEFT)) {{ setIsFixed(true); }});
        tileMap.put(new Position(height-1, 0), new Tile(EnumSet.of(Direction.UP, Direction.RIGHT)) {{ setIsFixed(true); }});
        tileMap.put(new Position(height-1, width-1), new Tile(EnumSet.of(Direction.UP, Direction.LEFT)) {{ setIsFixed(true); }});

        return new Board(width, height, tileMap, createRandomTile());
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
