package labyrinth.game.factories;

import labyrinth.game.abstractions.IBoardFactory;
import labyrinth.game.models.*;
import labyrinth.game.enums.*;
import labyrinth.game.models.BiMap;
import labyrinth.game.models.Position;
import labyrinth.game.util.TileShapes;

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

        var tileMap = createRandomTileMap(width, height);
        replaceCornerTiles(tileMap, width, height);

        return new Board(width, height, tileMap, createRandomTile());
    }

    private BiMap<Position, Tile> createRandomTileMap(int width, int height){
        BiMap<Position, Tile> tileMap = new BiMap<>();

        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile;

                boolean rowFixed = shouldBeFixed(row, height);
                boolean colFixed = shouldBeFixed(col, width);

                if (rowFixed && colFixed) {
                    if(row == 0) {
                        tile = new Tile(TileShapes.SHAPE_T_NO_UP);
                    } else if(col == 0) {
                        tile = new Tile(TileShapes.SHAPE_T_NO_LEFT);
                    } else if(row == height - 1) {
                        tile = new Tile(TileShapes.SHAPE_T_NO_DOWN);
                    }  else if(col == width - 1) {
                        tile = new Tile(TileShapes.SHAPE_T_NO_RIGHT);
                    } else {
                        tile = new Tile(TileShapes.SHAPE_T_NO_UP);
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

        return tileMap;
    }

    private void replaceCornerTiles(BiMap<Position, Tile> tileMap, int width, int height){
        tileMap.put(new Position(0, 0), new Tile(TileShapes.SHAPE_CORNER_DOWN_RIGHT) {{ setIsFixed(true); }});
        tileMap.put(new Position(0, width-1), new Tile(TileShapes.SHAPE_CORNER_DOWN_LEFT) {{ setIsFixed(true); }});
        tileMap.put(new Position(height-1, 0), new Tile(TileShapes.SHAPE_CORNER_UP_RIGHT) {{ setIsFixed(true); }});
        tileMap.put(new Position(height-1, width-1), new Tile(TileShapes.SHAPE_CORNER_UP_LEFT) {{ setIsFixed(true); }});

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
