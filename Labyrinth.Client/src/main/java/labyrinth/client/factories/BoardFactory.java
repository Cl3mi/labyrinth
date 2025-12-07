package labyrinth.client.factories;

import labyrinth.client.abstractions.IBoardFactory;
import labyrinth.client.models.*;
import labyrinth.client.enums.*;
import labyrinth.contracts.models.Coordinates;
import labyrinth.contracts.models.PlayerState;

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

    public static Board fromContracts(labyrinth.contracts.models.GameBoard gameBoard) {
        int rows = gameBoard.getRows();
        int cols = gameBoard.getCols();

        Tile[][] clientTiles = new Tile[rows][cols];

        labyrinth.contracts.models.Tile[][] contractTiles = gameBoard.getTiles();

        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                labyrinth.contracts.models.Tile ct = contractTiles[row][col];
                clientTiles[row][col] = convertContractTile(ct);
            }
        }

        // Extra-Tile:
        // Im GameBoard-Contract ist sie nicht direkt drin.
        // Bis du weißt, wo sie im Event kommt, nehmen wir eine Dummy-Tile.
        Tile extraTile = createRandomTile();

        // Achtung: Board-Konstruktor: (width, height, tiles, extraTile)
        return new Board(cols, rows, clientTiles, extraTile);
    }

    /**
     * Hilfsmethode: wandelt ein Contract-Tile in ein Client-Tile um.
     */
    private static Tile convertContractTile(labyrinth.contracts.models.Tile ct) {
        if (ct == null) {
            // Fallback: einfach eine gerade Tile
            return new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
        }

        // Entrances: contracts.Direction[] -> EnumSet<client.Direction>
        EnumSet<Direction> entrances = EnumSet.noneOf(Direction.class);
        if (ct.getEntrances() != null) {
            for (labyrinth.contracts.models.Direction d : ct.getEntrances()) {
                entrances.add(Direction.valueOf(d.name()));
            }
        }

        Tile clientTile = new Tile(entrances);

        // Fixed-Flag
        if (ct.getIsFixed() != null && ct.getIsFixed()) {
            clientTile.setFixed(true);
        }

        // TODO: Treasure / Bonus aus Contracts übernehmen,
        // wenn du dafür schon Client-Modelle hast.
        // z.B. clientTile.setTreasureCard(...);

        return clientTile;
    }

    public static List<labyrinth.client.models.Player> convertPlayerStates(PlayerState[] states) {
        List<labyrinth.client.models.Player> list = new ArrayList<>();
        if (states == null) return list;

        for (PlayerState s : states) {
            if (s == null) continue;

            // Client-Player mit ID & Name
            labyrinth.client.models.Player p =
                    new labyrinth.client.models.Player(s.getId(), s.getName());

            // Position (falls vorhanden)
            if (s.getCurrentPosition() != null) {
                Coordinates pos = s.getCurrentPosition();
                // x = row, y = column
                p.setCurrentPosition(
                        new labyrinth.client.models.Position(pos.getX(), pos.getY())
                );
            }

            // Optional: Farbe, Admin-Status, etc., nur falls du passende Setter im Client hast
            // if (s.getColor() != null) {
            //     p.setColor( mapColor(s.getColor()) );
            // }
            // p.setAdmin(Boolean.TRUE.equals(s.getIsAdmin()));

            list.add(p);
        }

        return list;
    }


}
