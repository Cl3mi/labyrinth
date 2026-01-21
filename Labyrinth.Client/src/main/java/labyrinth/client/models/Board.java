package labyrinth.client.models;

import labyrinth.client.enums.MoveState;
import labyrinth.client.models.extensions.TreasureUtils;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.PushActionInfo;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * Repräsentiert das Spielfeld für das Labyrinth-Spiel.
 * Nutzt contracts.Tile und contracts.Direction.
 */

@Getter
@Setter
public class Board {

    private final int width;
    private final int height;
    private final Tile[][] tiles;
    private final Graph graph;

    private List<Player> players;
    private int currentPlayerIndex;
    private MoveState currentMoveState = MoveState.PLACE_TILE;
    private Tile extraTile;
    private boolean freeRoam = false;
    private PushActionInfo lastPush;

    public Board(int width, int height, Tile[][] tiles, Tile extraTile) {
        if (tiles.length != height || tiles[0].length != width) {
            throw new IllegalArgumentException("Tile array dimensions must match width and height");
        }
        this.width = width;
        this.height = height;
        this.tiles = tiles;
        this.graph = new Graph();
        this.extraTile = extraTile;

        initializeGraph();
    }

    private void initializeGraph() {
        graph.clear();
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                Tile tile = tiles[row][col];
                graph.addTile(tile);

                if (row > 0) {
                    Tile upNeighbor = tiles[row - 1][col];
                    graph.connect(tile, upNeighbor, Direction.UP);
                }
                if (row < height - 1) {
                    Tile downNeighbor = tiles[row + 1][col];
                    graph.connect(tile, downNeighbor, Direction.DOWN);
                }
                if (col > 0) {
                    Tile leftNeighbor = tiles[row][col - 1];
                    graph.connect(tile, leftNeighbor, Direction.LEFT);
                }
                if (col < width - 1) {
                    Tile rightNeighbor = tiles[row][col + 1];
                    graph.connect(tile, rightNeighbor, Direction.RIGHT);
                }
            }
        }
    }






}
