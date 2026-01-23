package labyrinth.client.unit.models;

import labyrinth.client.enums.MoveState;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("Board")
class BoardTest {

    private static final int DEFAULT_WIDTH = 7;
    private static final int DEFAULT_HEIGHT = 7;

    /**
     * Helper method to create a tile with specified entrances.
     */
    private Tile createTile(Direction... entrances) {
        Tile tile = new Tile();
        tile.setEntrances(entrances);
        return tile;
    }

    /**
     * Helper method to create a standard 7x7 board with connected tiles.
     */
    private Tile[][] createStandardTileGrid(int width, int height) {
        Tile[][] tiles = new Tile[height][width];
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                // Create tiles with all four entrances for connectivity
                tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            }
        }
        return tiles;
    }

    /**
     * Helper method to create a simple 3x3 board for testing.
     */
    private Tile[][] createSmallTileGrid() {
        return createStandardTileGrid(3, 3);
    }

    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {

        @Test
        @DisplayName("constructor_validParameters_createsBoard")
        void constructor_validParameters_createsBoard() {
            // Given
            Tile[][] tiles = createStandardTileGrid(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN);

            // When
            Board board = new Board(DEFAULT_WIDTH, DEFAULT_HEIGHT, tiles, extraTile);

            // Then
            assertThat(board.getWidth()).isEqualTo(DEFAULT_WIDTH);
            assertThat(board.getHeight()).isEqualTo(DEFAULT_HEIGHT);
            assertThat(board.getTiles()).isEqualTo(tiles);
            assertThat(board.getExtraTile()).isEqualTo(extraTile);
        }

        @Test
        @DisplayName("constructor_smallBoard_createsBoard")
        void constructor_smallBoard_createsBoard() {
            // Given
            int width = 3;
            int height = 3;
            Tile[][] tiles = createStandardTileGrid(width, height);
            Tile extraTile = createTile(Direction.LEFT, Direction.RIGHT);

            // When
            Board board = new Board(width, height, tiles, extraTile);

            // Then
            assertThat(board.getWidth()).isEqualTo(width);
            assertThat(board.getHeight()).isEqualTo(height);
        }

        @Test
        @DisplayName("constructor_mismatchedDimensions_throwsException")
        void constructor_mismatchedDimensions_throwsException() {
            // Given
            int declaredWidth = 7;
            int declaredHeight = 7;
            Tile[][] tiles = createStandardTileGrid(5, 5); // Wrong dimensions
            Tile extraTile = createTile(Direction.UP);

            // When/Then
            assertThatThrownBy(() -> new Board(declaredWidth, declaredHeight, tiles, extraTile))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("dimensions");
        }

        @Test
        @DisplayName("constructor_heightMismatch_throwsException")
        void constructor_heightMismatch_throwsException() {
            // Given
            int declaredWidth = 7;
            int declaredHeight = 7;
            Tile[][] tiles = createStandardTileGrid(7, 5); // Height mismatch
            Tile extraTile = createTile(Direction.UP);

            // When/Then
            assertThatThrownBy(() -> new Board(declaredWidth, declaredHeight, tiles, extraTile))
                    .isInstanceOf(IllegalArgumentException.class);
        }

        @Test
        @DisplayName("constructor_initializesGraph")
        void constructor_initializesGraph() {
            // Given
            Tile[][] tiles = createStandardTileGrid(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN);

            // When
            Board board = new Board(DEFAULT_WIDTH, DEFAULT_HEIGHT, tiles, extraTile);

            // Then
            assertThat(board.getGraph()).isNotNull();
        }

        @Test
        @DisplayName("constructor_initializesDefaultMoveState")
        void constructor_initializesDefaultMoveState() {
            // Given
            Tile[][] tiles = createStandardTileGrid(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN);

            // When
            Board board = new Board(DEFAULT_WIDTH, DEFAULT_HEIGHT, tiles, extraTile);

            // Then
            assertThat(board.getCurrentMoveState()).isEqualTo(MoveState.PLACE_TILE);
        }
    }

    @Nested
    @DisplayName("Getters and Setters")
    class GetterSetterTests {

        private Board board;
        private Tile[][] tiles;
        private Tile extraTile;

        @BeforeEach
        void setUp() {
            tiles = createStandardTileGrid(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            extraTile = createTile(Direction.UP, Direction.DOWN);
            board = new Board(DEFAULT_WIDTH, DEFAULT_HEIGHT, tiles, extraTile);
        }

        @Test
        @DisplayName("getWidth_afterCreation_returnsCorrectWidth")
        void getWidth_afterCreation_returnsCorrectWidth() {
            // When/Then
            assertThat(board.getWidth()).isEqualTo(DEFAULT_WIDTH);
        }

        @Test
        @DisplayName("getHeight_afterCreation_returnsCorrectHeight")
        void getHeight_afterCreation_returnsCorrectHeight() {
            // When/Then
            assertThat(board.getHeight()).isEqualTo(DEFAULT_HEIGHT);
        }

        @Test
        @DisplayName("getTiles_afterCreation_returnsTileArray")
        void getTiles_afterCreation_returnsTileArray() {
            // When/Then
            assertThat(board.getTiles()).isEqualTo(tiles);
            assertThat(board.getTiles().length).isEqualTo(DEFAULT_HEIGHT);
            assertThat(board.getTiles()[0].length).isEqualTo(DEFAULT_WIDTH);
        }

        @Test
        @DisplayName("getExtraTile_afterCreation_returnsExtraTile")
        void getExtraTile_afterCreation_returnsExtraTile() {
            // When/Then
            assertThat(board.getExtraTile()).isEqualTo(extraTile);
        }

        @Test
        @DisplayName("setExtraTile_newTile_updatesExtraTile")
        void setExtraTile_newTile_updatesExtraTile() {
            // Given
            Tile newExtraTile = createTile(Direction.LEFT, Direction.RIGHT);

            // When
            board.setExtraTile(newExtraTile);

            // Then
            assertThat(board.getExtraTile()).isEqualTo(newExtraTile);
        }

        @Test
        @DisplayName("getPlayers_initially_returnsNull")
        void getPlayers_initially_returnsNull() {
            // When/Then
            assertThat(board.getPlayers()).isNull();
        }

        @Test
        @DisplayName("setPlayers_validList_updatesPlayers")
        void setPlayers_validList_updatesPlayers() {
            // Given
            List<Player> players = new ArrayList<>();
            players.add(new Player("1", "Alice"));
            players.add(new Player("2", "Bob"));

            // When
            board.setPlayers(players);

            // Then
            assertThat(board.getPlayers()).hasSize(2);
            assertThat(board.getPlayers().get(0).getName()).isEqualTo("Alice");
        }

        @Test
        @DisplayName("getCurrentPlayerIndex_initially_returnsZero")
        void getCurrentPlayerIndex_initially_returnsZero() {
            // When/Then
            assertThat(board.getCurrentPlayerIndex()).isZero();
        }

        @Test
        @DisplayName("setCurrentPlayerIndex_validIndex_updatesIndex")
        void setCurrentPlayerIndex_validIndex_updatesIndex() {
            // Given
            int newIndex = 2;

            // When
            board.setCurrentPlayerIndex(newIndex);

            // Then
            assertThat(board.getCurrentPlayerIndex()).isEqualTo(newIndex);
        }

        @Test
        @DisplayName("isFreeRoam_initially_returnsFalse")
        void isFreeRoam_initially_returnsFalse() {
            // When/Then
            assertThat(board.isFreeRoam()).isFalse();
        }

        @Test
        @DisplayName("setFreeRoam_true_updatesFreeRoam")
        void setFreeRoam_true_updatesFreeRoam() {
            // When
            board.setFreeRoam(true);

            // Then
            assertThat(board.isFreeRoam()).isTrue();
        }

        @Test
        @DisplayName("getLastPush_initially_returnsNull")
        void getLastPush_initially_returnsNull() {
            // When/Then
            assertThat(board.getLastPush()).isNull();
        }
    }

    @Nested
    @DisplayName("MoveState")
    class MoveStateTests {

        private Board board;

        @BeforeEach
        void setUp() {
            Tile[][] tiles = createStandardTileGrid(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN);
            board = new Board(DEFAULT_WIDTH, DEFAULT_HEIGHT, tiles, extraTile);
        }

        @Test
        @DisplayName("getCurrentMoveState_initially_returnsPlaceTile")
        void getCurrentMoveState_initially_returnsPlaceTile() {
            // When/Then
            assertThat(board.getCurrentMoveState()).isEqualTo(MoveState.PLACE_TILE);
        }

        @Test
        @DisplayName("setCurrentMoveState_move_updatesState")
        void setCurrentMoveState_move_updatesState() {
            // When
            board.setCurrentMoveState(MoveState.MOVE);

            // Then
            assertThat(board.getCurrentMoveState()).isEqualTo(MoveState.MOVE);
        }

        @Test
        @DisplayName("setCurrentMoveState_placeTile_updatesState")
        void setCurrentMoveState_placeTile_updatesState() {
            // Given
            board.setCurrentMoveState(MoveState.MOVE);

            // When
            board.setCurrentMoveState(MoveState.PLACE_TILE);

            // Then
            assertThat(board.getCurrentMoveState()).isEqualTo(MoveState.PLACE_TILE);
        }
    }

    @Nested
    @DisplayName("Graph Initialization")
    class GraphInitializationTests {

        @Test
        @DisplayName("constructor_createsGraphWithConnections")
        void constructor_createsGraphWithConnections() {
            // Given
            Tile[][] tiles = createStandardTileGrid(3, 3);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN);

            // When
            Board board = new Board(3, 3, tiles, extraTile);

            // Then
            assertThat(board.getGraph()).isNotNull();
        }

        @Test
        @DisplayName("constructor_largeBoard_initializesGraph")
        void constructor_largeBoard_initializesGraph() {
            // Given
            Tile[][] tiles = createStandardTileGrid(11, 11);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);

            // When
            Board board = new Board(11, 11, tiles, extraTile);

            // Then
            assertThat(board.getGraph()).isNotNull();
            assertThat(board.getWidth()).isEqualTo(11);
            assertThat(board.getHeight()).isEqualTo(11);
        }
    }

    @Nested
    @DisplayName("Tile Access")
    class TileAccessTests {

        private Board board;
        private Tile[][] tiles;

        @BeforeEach
        void setUp() {
            tiles = createStandardTileGrid(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN);
            board = new Board(DEFAULT_WIDTH, DEFAULT_HEIGHT, tiles, extraTile);
        }

        @Test
        @DisplayName("getTiles_atCorner_returnsTile")
        void getTiles_atCorner_returnsTile() {
            // When/Then
            assertThat(board.getTiles()[0][0]).isNotNull();
            assertThat(board.getTiles()[0][DEFAULT_WIDTH - 1]).isNotNull();
            assertThat(board.getTiles()[DEFAULT_HEIGHT - 1][0]).isNotNull();
            assertThat(board.getTiles()[DEFAULT_HEIGHT - 1][DEFAULT_WIDTH - 1]).isNotNull();
        }

        @Test
        @DisplayName("getTiles_atCenter_returnsTile")
        void getTiles_atCenter_returnsTile() {
            // Given
            int centerRow = DEFAULT_HEIGHT / 2;
            int centerCol = DEFAULT_WIDTH / 2;

            // When/Then
            assertThat(board.getTiles()[centerRow][centerCol]).isNotNull();
        }

        @Test
        @DisplayName("getTiles_directArrayAccess_returnsSameTile")
        void getTiles_directArrayAccess_returnsSameTile() {
            // Given
            Tile expectedTile = tiles[3][4];

            // When
            Tile actualTile = board.getTiles()[3][4];

            // Then
            assertThat(actualTile).isSameAs(expectedTile);
        }
    }

    @Nested
    @DisplayName("EdgeCases")
    class EdgeCaseTests {

        @Test
        @DisplayName("constructor_minimumSize_createsBoard")
        void constructor_minimumSize_createsBoard() {
            // Given
            Tile[][] tiles = createStandardTileGrid(1, 1);
            Tile extraTile = createTile(Direction.UP);

            // When
            Board board = new Board(1, 1, tiles, extraTile);

            // Then
            assertThat(board.getWidth()).isEqualTo(1);
            assertThat(board.getHeight()).isEqualTo(1);
        }

        @Test
        @DisplayName("constructor_rectangularBoard_createsBoard")
        void constructor_rectangularBoard_createsBoard() {
            // Given
            int width = 5;
            int height = 3;
            Tile[][] tiles = createStandardTileGrid(width, height);
            Tile extraTile = createTile(Direction.UP);

            // When
            Board board = new Board(width, height, tiles, extraTile);

            // Then
            assertThat(board.getWidth()).isEqualTo(width);
            assertThat(board.getHeight()).isEqualTo(height);
        }

        @Test
        @DisplayName("constructor_tilesWithMixedEntrances_createsBoard")
        void constructor_tilesWithMixedEntrances_createsBoard() {
            // Given
            Tile[][] tiles = new Tile[3][3];
            tiles[0][0] = createTile(Direction.DOWN, Direction.RIGHT);
            tiles[0][1] = createTile(Direction.LEFT, Direction.DOWN, Direction.RIGHT);
            tiles[0][2] = createTile(Direction.DOWN, Direction.LEFT);
            tiles[1][0] = createTile(Direction.UP, Direction.DOWN, Direction.RIGHT);
            tiles[1][1] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            tiles[1][2] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT);
            tiles[2][0] = createTile(Direction.UP, Direction.RIGHT);
            tiles[2][1] = createTile(Direction.UP, Direction.LEFT, Direction.RIGHT);
            tiles[2][2] = createTile(Direction.UP, Direction.LEFT);
            Tile extraTile = createTile(Direction.UP, Direction.DOWN);

            // When
            Board board = new Board(3, 3, tiles, extraTile);

            // Then
            assertThat(board.getGraph()).isNotNull();
            assertThat(board.getWidth()).isEqualTo(3);
        }
    }
}
