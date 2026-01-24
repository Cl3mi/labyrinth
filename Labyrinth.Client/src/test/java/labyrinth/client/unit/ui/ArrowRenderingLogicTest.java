package labyrinth.client.unit.ui;

import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.*;

/**
 * Tests for the arrow rendering logic that determines which rows/columns
 * should have push arrows based on fixed tile positions.
 *
 * This tests the logic extracted from BoardPanel.createAndDrawArrowButtons()
 */
@DisplayName("Arrow Rendering Logic")
class ArrowRenderingLogicTest {

    /**
     * Helper method to create a tile with specified entrances.
     */
    private Tile createTile(Direction... entrances) {
        Tile tile = new Tile();
        tile.setEntrances(entrances);
        tile.setIsFixed(false);
        return tile;
    }

    /**
     * Helper method to create a fixed tile.
     */
    private Tile createFixedTile(Direction... entrances) {
        Tile tile = new Tile();
        tile.setEntrances(entrances);
        tile.setIsFixed(true);
        return tile;
    }

    /**
     * Checks if a row contains any fixed tiles.
     * This is the same logic used in BoardPanel.
     */
    private boolean rowContainsFixedTile(Tile[][] tiles, int row, int cols) {
        for (int col = 0; col < cols; col++) {
            if (tiles[row][col] != null && Boolean.TRUE.equals(tiles[row][col].getIsFixed())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if a column contains any fixed tiles.
     * This is the same logic used in BoardPanel.
     */
    private boolean colContainsFixedTile(Tile[][] tiles, int col, int rows) {
        for (int row = 0; row < rows; row++) {
            if (tiles[row][col] != null && Boolean.TRUE.equals(tiles[row][col].getIsFixed())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Determines if a row should have arrows rendered.
     * Logic: Not outer edge, and either no fixed tiles OR pushFixedActive
     */
    private boolean shouldRenderRowArrows(Tile[][] tiles, int row, int rows, int cols, boolean pushFixedActive) {
        // Skip outer edges
        if (row == 0 || row == rows - 1) return false;

        boolean hasFixed = rowContainsFixedTile(tiles, row, cols);
        boolean isNormalPushable = !hasFixed;
        boolean isPushFixedPushable = pushFixedActive && hasFixed;

        return isNormalPushable || isPushFixedPushable;
    }

    /**
     * Determines if a column should have arrows rendered.
     * Logic: Not outer edge, and either no fixed tiles OR pushFixedActive
     */
    private boolean shouldRenderColArrows(Tile[][] tiles, int col, int rows, int cols, boolean pushFixedActive) {
        // Skip outer edges
        if (col == 0 || col == cols - 1) return false;

        boolean hasFixed = colContainsFixedTile(tiles, col, rows);
        boolean isNormalPushable = !hasFixed;
        boolean isPushFixedPushable = pushFixedActive && hasFixed;

        return isNormalPushable || isPushFixedPushable;
    }

    @Nested
    @DisplayName("Row Contains Fixed Tile")
    class RowContainsFixedTileTests {

        @Test
        @DisplayName("rowWithNoFixedTiles_returnsFalse")
        void rowWithNoFixedTiles_returnsFalse() {
            Tile[][] tiles = new Tile[3][3];
            for (int col = 0; col < 3; col++) {
                tiles[1][col] = createTile(Direction.UP, Direction.DOWN);
            }

            assertThat(rowContainsFixedTile(tiles, 1, 3)).isFalse();
        }

        @Test
        @DisplayName("rowWithOneFixedTile_returnsTrue")
        void rowWithOneFixedTile_returnsTrue() {
            Tile[][] tiles = new Tile[3][3];
            tiles[1][0] = createTile(Direction.UP, Direction.DOWN);
            tiles[1][1] = createFixedTile(Direction.UP, Direction.DOWN);
            tiles[1][2] = createTile(Direction.UP, Direction.DOWN);

            assertThat(rowContainsFixedTile(tiles, 1, 3)).isTrue();
        }

        @Test
        @DisplayName("rowWithAllFixedTiles_returnsTrue")
        void rowWithAllFixedTiles_returnsTrue() {
            Tile[][] tiles = new Tile[3][3];
            for (int col = 0; col < 3; col++) {
                tiles[1][col] = createFixedTile(Direction.UP, Direction.DOWN);
            }

            assertThat(rowContainsFixedTile(tiles, 1, 3)).isTrue();
        }

        @Test
        @DisplayName("rowWithFixedTileAtStart_returnsTrue")
        void rowWithFixedTileAtStart_returnsTrue() {
            Tile[][] tiles = new Tile[3][5];
            tiles[1][0] = createFixedTile(Direction.UP, Direction.DOWN);
            for (int col = 1; col < 5; col++) {
                tiles[1][col] = createTile(Direction.UP, Direction.DOWN);
            }

            assertThat(rowContainsFixedTile(tiles, 1, 5)).isTrue();
        }

        @Test
        @DisplayName("rowWithFixedTileAtEnd_returnsTrue")
        void rowWithFixedTileAtEnd_returnsTrue() {
            Tile[][] tiles = new Tile[3][5];
            for (int col = 0; col < 4; col++) {
                tiles[1][col] = createTile(Direction.UP, Direction.DOWN);
            }
            tiles[1][4] = createFixedTile(Direction.UP, Direction.DOWN);

            assertThat(rowContainsFixedTile(tiles, 1, 5)).isTrue();
        }
    }

    @Nested
    @DisplayName("Column Contains Fixed Tile")
    class ColContainsFixedTileTests {

        @Test
        @DisplayName("colWithNoFixedTiles_returnsFalse")
        void colWithNoFixedTiles_returnsFalse() {
            Tile[][] tiles = new Tile[3][3];
            for (int row = 0; row < 3; row++) {
                tiles[row][1] = createTile(Direction.LEFT, Direction.RIGHT);
            }

            assertThat(colContainsFixedTile(tiles, 1, 3)).isFalse();
        }

        @Test
        @DisplayName("colWithOneFixedTile_returnsTrue")
        void colWithOneFixedTile_returnsTrue() {
            Tile[][] tiles = new Tile[3][3];
            tiles[0][1] = createTile(Direction.LEFT, Direction.RIGHT);
            tiles[1][1] = createFixedTile(Direction.LEFT, Direction.RIGHT);
            tiles[2][1] = createTile(Direction.LEFT, Direction.RIGHT);

            assertThat(colContainsFixedTile(tiles, 1, 3)).isTrue();
        }

        @Test
        @DisplayName("colWithAllFixedTiles_returnsTrue")
        void colWithAllFixedTiles_returnsTrue() {
            Tile[][] tiles = new Tile[3][3];
            for (int row = 0; row < 3; row++) {
                tiles[row][1] = createFixedTile(Direction.LEFT, Direction.RIGHT);
            }

            assertThat(colContainsFixedTile(tiles, 1, 3)).isTrue();
        }
    }

    @Nested
    @DisplayName("Should Render Row Arrows")
    class ShouldRenderRowArrowsTests {

        private Tile[][] tiles;
        private static final int ROWS = 7;
        private static final int COLS = 7;

        @BeforeEach
        void setUp() {
            tiles = new Tile[ROWS][COLS];
            // Create standard board with fixed tiles on even rows/cols
            for (int row = 0; row < ROWS; row++) {
                for (int col = 0; col < COLS; col++) {
                    boolean rowFixed = row % 2 == 0;
                    boolean colFixed = col % 2 == 0;
                    if (rowFixed && colFixed) {
                        tiles[row][col] = createFixedTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    } else {
                        tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    }
                }
            }
        }

        @Test
        @DisplayName("outerEdgeRow0_neverHasArrows")
        void outerEdgeRow0_neverHasArrows() {
            assertThat(shouldRenderRowArrows(tiles, 0, ROWS, COLS, false)).isFalse();
            assertThat(shouldRenderRowArrows(tiles, 0, ROWS, COLS, true)).isFalse();
        }

        @Test
        @DisplayName("outerEdgeRowMax_neverHasArrows")
        void outerEdgeRowMax_neverHasArrows() {
            assertThat(shouldRenderRowArrows(tiles, ROWS - 1, ROWS, COLS, false)).isFalse();
            assertThat(shouldRenderRowArrows(tiles, ROWS - 1, ROWS, COLS, true)).isFalse();
        }

        @Test
        @DisplayName("oddRowWithoutFixedTiles_hasArrows")
        void oddRowWithoutFixedTiles_hasArrows() {
            // Row 1, 3, 5 are odd and should have arrows (no fixed tiles in row)
            // But in standard setup, odd rows have fixed tiles at even columns
            // Let's create a board without fixed tiles in row 1
            for (int col = 0; col < COLS; col++) {
                tiles[1][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            }

            assertThat(shouldRenderRowArrows(tiles, 1, ROWS, COLS, false)).isTrue();
        }

        @Test
        @DisplayName("rowWithFixedTiles_noArrowsWithoutBonus")
        void rowWithFixedTiles_noArrowsWithoutBonus() {
            // Row 2 (even) has fixed tiles
            assertThat(shouldRenderRowArrows(tiles, 2, ROWS, COLS, false)).isFalse();
        }

        @Test
        @DisplayName("rowWithFixedTiles_hasArrowsWithPushFixedBonus")
        void rowWithFixedTiles_hasArrowsWithPushFixedBonus() {
            // Row 2 (even) has fixed tiles, but with PUSH_FIXED bonus it should have arrows
            assertThat(shouldRenderRowArrows(tiles, 2, ROWS, COLS, true)).isTrue();
        }
    }

    @Nested
    @DisplayName("Should Render Column Arrows")
    class ShouldRenderColArrowsTests {

        private Tile[][] tiles;
        private static final int ROWS = 7;
        private static final int COLS = 7;

        @BeforeEach
        void setUp() {
            tiles = new Tile[ROWS][COLS];
            // Create standard board with fixed tiles on even rows/cols
            for (int row = 0; row < ROWS; row++) {
                for (int col = 0; col < COLS; col++) {
                    boolean rowFixed = row % 2 == 0;
                    boolean colFixed = col % 2 == 0;
                    if (rowFixed && colFixed) {
                        tiles[row][col] = createFixedTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    } else {
                        tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    }
                }
            }
        }

        @Test
        @DisplayName("outerEdgeCol0_neverHasArrows")
        void outerEdgeCol0_neverHasArrows() {
            assertThat(shouldRenderColArrows(tiles, 0, ROWS, COLS, false)).isFalse();
            assertThat(shouldRenderColArrows(tiles, 0, ROWS, COLS, true)).isFalse();
        }

        @Test
        @DisplayName("outerEdgeColMax_neverHasArrows")
        void outerEdgeColMax_neverHasArrows() {
            assertThat(shouldRenderColArrows(tiles, COLS - 1, ROWS, COLS, false)).isFalse();
            assertThat(shouldRenderColArrows(tiles, COLS - 1, ROWS, COLS, true)).isFalse();
        }

        @Test
        @DisplayName("oddColWithoutFixedTiles_hasArrows")
        void oddColWithoutFixedTiles_hasArrows() {
            // Create a column without fixed tiles
            for (int row = 0; row < ROWS; row++) {
                tiles[row][1] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            }

            assertThat(shouldRenderColArrows(tiles, 1, ROWS, COLS, false)).isTrue();
        }

        @Test
        @DisplayName("colWithFixedTiles_noArrowsWithoutBonus")
        void colWithFixedTiles_noArrowsWithoutBonus() {
            // Col 2 (even) has fixed tiles
            assertThat(shouldRenderColArrows(tiles, 2, ROWS, COLS, false)).isFalse();
        }

        @Test
        @DisplayName("colWithFixedTiles_hasArrowsWithPushFixedBonus")
        void colWithFixedTiles_hasArrowsWithPushFixedBonus() {
            // Col 2 (even) has fixed tiles, but with PUSH_FIXED bonus it should have arrows
            assertThat(shouldRenderColArrows(tiles, 2, ROWS, COLS, true)).isTrue();
        }
    }

    @Nested
    @DisplayName("Rectangular Board")
    class RectangularBoardTests {

        @Test
        @DisplayName("wideBoard_correctlyDetectsFixedRows")
        void wideBoard_correctlyDetectsFixedRows() {
            int rows = 5;
            int cols = 9;
            Tile[][] tiles = new Tile[rows][cols];

            // Create board with fixed tiles on even positions
            for (int row = 0; row < rows; row++) {
                for (int col = 0; col < cols; col++) {
                    boolean rowFixed = row % 2 == 0;
                    boolean colFixed = col % 2 == 0;
                    if (rowFixed && colFixed) {
                        tiles[row][col] = createFixedTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    } else {
                        tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    }
                }
            }

            // Row 0 and 4 are outer edges - no arrows
            assertThat(shouldRenderRowArrows(tiles, 0, rows, cols, false)).isFalse();
            assertThat(shouldRenderRowArrows(tiles, 4, rows, cols, false)).isFalse();

            // Row 2 (even, has fixed) - no arrows without bonus
            assertThat(shouldRenderRowArrows(tiles, 2, rows, cols, false)).isFalse();

            // Row 2 with PUSH_FIXED - arrows
            assertThat(shouldRenderRowArrows(tiles, 2, rows, cols, true)).isTrue();
        }

        @Test
        @DisplayName("tallBoard_correctlyDetectsFixedCols")
        void tallBoard_correctlyDetectsFixedCols() {
            int rows = 9;
            int cols = 5;
            Tile[][] tiles = new Tile[rows][cols];

            // Create board with fixed tiles on even positions
            for (int row = 0; row < rows; row++) {
                for (int col = 0; col < cols; col++) {
                    boolean rowFixed = row % 2 == 0;
                    boolean colFixed = col % 2 == 0;
                    if (rowFixed && colFixed) {
                        tiles[row][col] = createFixedTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    } else {
                        tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    }
                }
            }

            // Col 0 and 4 are outer edges - no arrows
            assertThat(shouldRenderColArrows(tiles, 0, rows, cols, false)).isFalse();
            assertThat(shouldRenderColArrows(tiles, 4, rows, cols, false)).isFalse();

            // Col 2 (even, has fixed) - no arrows without bonus
            assertThat(shouldRenderColArrows(tiles, 2, rows, cols, false)).isFalse();

            // Col 2 with PUSH_FIXED - arrows
            assertThat(shouldRenderColArrows(tiles, 2, rows, cols, true)).isTrue();
        }
    }

    @Nested
    @DisplayName("All Non-Fixed Board")
    class AllNonFixedBoardTests {

        @Test
        @DisplayName("boardWithNoFixedTiles_allInnerRowsHaveArrows")
        void boardWithNoFixedTiles_allInnerRowsHaveArrows() {
            int rows = 5;
            int cols = 5;
            Tile[][] tiles = new Tile[rows][cols];

            // Create board with no fixed tiles
            for (int row = 0; row < rows; row++) {
                for (int col = 0; col < cols; col++) {
                    tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                }
            }

            // Outer edges (0 and 4) - no arrows
            assertThat(shouldRenderRowArrows(tiles, 0, rows, cols, false)).isFalse();
            assertThat(shouldRenderRowArrows(tiles, 4, rows, cols, false)).isFalse();

            // Inner rows (1, 2, 3) - all have arrows
            assertThat(shouldRenderRowArrows(tiles, 1, rows, cols, false)).isTrue();
            assertThat(shouldRenderRowArrows(tiles, 2, rows, cols, false)).isTrue();
            assertThat(shouldRenderRowArrows(tiles, 3, rows, cols, false)).isTrue();
        }

        @Test
        @DisplayName("boardWithNoFixedTiles_allInnerColsHaveArrows")
        void boardWithNoFixedTiles_allInnerColsHaveArrows() {
            int rows = 5;
            int cols = 5;
            Tile[][] tiles = new Tile[rows][cols];

            // Create board with no fixed tiles
            for (int row = 0; row < rows; row++) {
                for (int col = 0; col < cols; col++) {
                    tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                }
            }

            // Outer edges (0 and 4) - no arrows
            assertThat(shouldRenderColArrows(tiles, 0, rows, cols, false)).isFalse();
            assertThat(shouldRenderColArrows(tiles, 4, rows, cols, false)).isFalse();

            // Inner cols (1, 2, 3) - all have arrows
            assertThat(shouldRenderColArrows(tiles, 1, rows, cols, false)).isTrue();
            assertThat(shouldRenderColArrows(tiles, 2, rows, cols, false)).isTrue();
            assertThat(shouldRenderColArrows(tiles, 3, rows, cols, false)).isTrue();
        }
    }
}
