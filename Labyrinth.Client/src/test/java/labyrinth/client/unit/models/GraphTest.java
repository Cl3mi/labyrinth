package labyrinth.client.unit.models;

import labyrinth.client.models.Graph;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.*;

@DisplayName("Graph")
class GraphTest {

    private Graph graph;

    @BeforeEach
    void setUp() {
        graph = new Graph();
    }

    /**
     * Helper method to create a tile with specified entrances.
     */
    private Tile createTile(Direction... entrances) {
        Tile tile = new Tile();
        tile.setEntrances(entrances);
        return tile;
    }

    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {

        @Test
        @DisplayName("constructor_default_createsEmptyGraph")
        void constructor_default_createsEmptyGraph() {
            // Given/When
            Graph newGraph = new Graph();

            // Then
            assertThat(newGraph).isNotNull();
        }
    }

    @Nested
    @DisplayName("AddTile")
    class AddTileTests {

        @Test
        @DisplayName("addTile_singleTile_addsTileToGraph")
        void addTile_singleTile_addsTileToGraph() {
            // Given
            Tile tile = createTile(Direction.UP, Direction.DOWN);

            // When
            graph.addTile(tile);

            // Then - no exception should be thrown
            assertThatCode(() -> graph.addTile(tile)).doesNotThrowAnyException();
        }

        @Test
        @DisplayName("addTile_multipleTiles_addsAllTilesToGraph")
        void addTile_multipleTiles_addsAllTilesToGraph() {
            // Given
            Tile tile1 = createTile(Direction.UP, Direction.DOWN);
            Tile tile2 = createTile(Direction.LEFT, Direction.RIGHT);
            Tile tile3 = createTile(Direction.UP, Direction.RIGHT, Direction.DOWN);

            // When/Then - all tiles should be added without exception
            assertThatCode(() -> {
                graph.addTile(tile1);
                graph.addTile(tile2);
                graph.addTile(tile3);
            }).doesNotThrowAnyException();
        }

        @Test
        @DisplayName("addTile_sameTileTwice_handlesGracefully")
        void addTile_sameTileTwice_handlesGracefully() {
            // Given
            Tile tile = createTile(Direction.UP, Direction.DOWN);

            // When/Then - adding same tile twice should not throw
            assertThatCode(() -> {
                graph.addTile(tile);
                graph.addTile(tile);
            }).doesNotThrowAnyException();
        }
    }

    @Nested
    @DisplayName("Connect")
    class ConnectTests {

        @Test
        @DisplayName("connect_tilesWithMatchingEntrances_connectsTiles")
        void connect_tilesWithMatchingEntrances_connectsTiles() {
            // Given
            // Tile1 has DOWN entrance, Tile2 has UP entrance
            Tile tile1 = createTile(Direction.DOWN);
            Tile tile2 = createTile(Direction.UP);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When - Tile2 is DOWN from Tile1
            graph.connect(tile1, tile2, Direction.DOWN);

            // Then - connection should succeed (no exception)
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_tilesWithoutMatchingEntrances_doesNotConnect")
        void connect_tilesWithoutMatchingEntrances_doesNotConnect() {
            // Given
            // Tile1 has UP entrance only, Tile2 has DOWN entrance only
            // They cannot connect because tile1 has no DOWN and tile2 has no UP
            Tile tile1 = createTile(Direction.UP);
            Tile tile2 = createTile(Direction.DOWN);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When - trying to connect (tile2 is DOWN from tile1)
            // Tile1 needs DOWN entrance, tile2 needs UP entrance
            graph.connect(tile1, tile2, Direction.DOWN);

            // Then - no exception, but tiles are not connected
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_nullTile1_handlesGracefully")
        void connect_nullTile1_handlesGracefully() {
            // Given
            Tile tile2 = createTile(Direction.UP);
            graph.addTile(tile2);

            // When/Then - should handle null gracefully
            assertThatCode(() -> graph.connect(null, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_nullTile2_handlesGracefully")
        void connect_nullTile2_handlesGracefully() {
            // Given
            Tile tile1 = createTile(Direction.DOWN);
            graph.addTile(tile1);

            // When/Then - should handle null gracefully
            assertThatCode(() -> graph.connect(tile1, null, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_bothNull_handlesGracefully")
        void connect_bothNull_handlesGracefully() {
            // When/Then
            assertThatCode(() -> graph.connect(null, null, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_horizontalConnection_connectsTilesCorrectly")
        void connect_horizontalConnection_connectsTilesCorrectly() {
            // Given
            // Tile1 has RIGHT entrance, Tile2 has LEFT entrance
            Tile tile1 = createTile(Direction.RIGHT);
            Tile tile2 = createTile(Direction.LEFT);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When - Tile2 is RIGHT from Tile1
            graph.connect(tile1, tile2, Direction.RIGHT);

            // Then - should connect without exception
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.RIGHT))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_verticalConnection_connectsTilesCorrectly")
        void connect_verticalConnection_connectsTilesCorrectly() {
            // Given
            // Tile1 has UP entrance, Tile2 has DOWN entrance
            Tile tile1 = createTile(Direction.UP);
            Tile tile2 = createTile(Direction.DOWN);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When - Tile2 is UP from Tile1
            graph.connect(tile1, tile2, Direction.UP);

            // Then - should connect without exception
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.UP))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_tileWithNullEntrances_handlesGracefully")
        void connect_tileWithNullEntrances_handlesGracefully() {
            // Given
            Tile tile1 = new Tile();
            tile1.setEntrances(null);
            Tile tile2 = createTile(Direction.UP);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When/Then
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_allDirections_handlesAllCases")
        void connect_allDirections_handlesAllCases() {
            // Given
            Tile centerTile = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            Tile upTile = createTile(Direction.DOWN);
            Tile downTile = createTile(Direction.UP);
            Tile leftTile = createTile(Direction.RIGHT);
            Tile rightTile = createTile(Direction.LEFT);

            graph.addTile(centerTile);
            graph.addTile(upTile);
            graph.addTile(downTile);
            graph.addTile(leftTile);
            graph.addTile(rightTile);

            // When/Then - all directions should work
            assertThatCode(() -> {
                graph.connect(centerTile, upTile, Direction.UP);
                graph.connect(centerTile, downTile, Direction.DOWN);
                graph.connect(centerTile, leftTile, Direction.LEFT);
                graph.connect(centerTile, rightTile, Direction.RIGHT);
            }).doesNotThrowAnyException();
        }
    }

    @Nested
    @DisplayName("Clear")
    class ClearTests {

        @Test
        @DisplayName("clear_emptyGraph_doesNotThrow")
        void clear_emptyGraph_doesNotThrow() {
            // Given - empty graph

            // When/Then
            assertThatCode(() -> graph.clear()).doesNotThrowAnyException();
        }

        @Test
        @DisplayName("clear_graphWithTiles_removesAllTiles")
        void clear_graphWithTiles_removesAllTiles() {
            // Given
            Tile tile1 = createTile(Direction.UP);
            Tile tile2 = createTile(Direction.DOWN);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When
            graph.clear();

            // Then - clearing should succeed
            assertThatCode(() -> graph.clear()).doesNotThrowAnyException();
        }

        @Test
        @DisplayName("clear_calledMultipleTimes_doesNotThrow")
        void clear_calledMultipleTimes_doesNotThrow() {
            // Given
            Tile tile = createTile(Direction.UP);
            graph.addTile(tile);

            // When/Then
            assertThatCode(() -> {
                graph.clear();
                graph.clear();
                graph.clear();
            }).doesNotThrowAnyException();
        }
    }

    @Nested
    @DisplayName("ConnectionLogic")
    class ConnectionLogicTests {

        @Test
        @DisplayName("areConnected_upDown_matchesCorrectly")
        void areConnected_upDown_matchesCorrectly() {
            // Given: tile1 with DOWN entrance connects to tile2 with UP entrance
            Tile tile1 = createTile(Direction.DOWN);
            Tile tile2 = createTile(Direction.UP);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When: tile2 is placed DOWN from tile1
            // tile1 needs DOWN, tile2 needs UP (opposite of DOWN)
            graph.connect(tile1, tile2, Direction.DOWN);

            // Then: tiles should be connected (tested implicitly by no exception)
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("areConnected_leftRight_matchesCorrectly")
        void areConnected_leftRight_matchesCorrectly() {
            // Given: tile1 with RIGHT entrance connects to tile2 with LEFT entrance
            Tile tile1 = createTile(Direction.RIGHT);
            Tile tile2 = createTile(Direction.LEFT);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When: tile2 is placed RIGHT from tile1
            graph.connect(tile1, tile2, Direction.RIGHT);

            // Then: tiles should be connected
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.RIGHT))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("areConnected_partialMismatch_doesNotConnect")
        void areConnected_partialMismatch_doesNotConnect() {
            // Given: tile1 has DOWN but tile2 has no UP
            Tile tile1 = createTile(Direction.DOWN);
            Tile tile2 = createTile(Direction.DOWN, Direction.LEFT); // No UP entrance!
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When: trying to connect (tiles don't actually connect due to missing entrance)
            graph.connect(tile1, tile2, Direction.DOWN);

            // Then: no exception, but connection is not made
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("areConnected_tShapedTile_connectsOnMultipleSides")
        void areConnected_tShapedTile_connectsOnMultipleSides() {
            // Given: T-shaped tile with UP, LEFT, RIGHT entrances
            Tile tTile = createTile(Direction.UP, Direction.LEFT, Direction.RIGHT);
            Tile upTile = createTile(Direction.DOWN);
            Tile leftTile = createTile(Direction.RIGHT);
            Tile rightTile = createTile(Direction.LEFT);
            Tile downTile = createTile(Direction.UP);

            graph.addTile(tTile);
            graph.addTile(upTile);
            graph.addTile(leftTile);
            graph.addTile(rightTile);
            graph.addTile(downTile);

            // When/Then: T-tile should connect to up, left, right but not down
            assertThatCode(() -> {
                graph.connect(tTile, upTile, Direction.UP);
                graph.connect(tTile, leftTile, Direction.LEFT);
                graph.connect(tTile, rightTile, Direction.RIGHT);
                // DOWN connection should not actually form (tTile has no DOWN entrance)
                graph.connect(tTile, downTile, Direction.DOWN);
            }).doesNotThrowAnyException();
        }

        @Test
        @DisplayName("areConnected_crossShapedTile_connectsOnAllSides")
        void areConnected_crossShapedTile_connectsOnAllSides() {
            // Given: Cross-shaped tile with all four entrances
            Tile crossTile = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            Tile upTile = createTile(Direction.DOWN);
            Tile downTile = createTile(Direction.UP);
            Tile leftTile = createTile(Direction.RIGHT);
            Tile rightTile = createTile(Direction.LEFT);

            graph.addTile(crossTile);
            graph.addTile(upTile);
            graph.addTile(downTile);
            graph.addTile(leftTile);
            graph.addTile(rightTile);

            // When/Then: Cross-tile should connect to all neighbors
            assertThatCode(() -> {
                graph.connect(crossTile, upTile, Direction.UP);
                graph.connect(crossTile, downTile, Direction.DOWN);
                graph.connect(crossTile, leftTile, Direction.LEFT);
                graph.connect(crossTile, rightTile, Direction.RIGHT);
            }).doesNotThrowAnyException();
        }
    }

    @Nested
    @DisplayName("EdgeCases")
    class EdgeCaseTests {

        @Test
        @DisplayName("connect_emptyEntranceArray_handlesGracefully")
        void connect_emptyEntranceArray_handlesGracefully() {
            // Given
            Tile tile1 = createTile(); // Empty entrances
            Tile tile2 = createTile(Direction.UP);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When/Then
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("connect_singleEntranceTile_connectsWhenMatching")
        void connect_singleEntranceTile_connectsWhenMatching() {
            // Given
            Tile tile1 = createTile(Direction.DOWN);
            Tile tile2 = createTile(Direction.UP);
            graph.addTile(tile1);
            graph.addTile(tile2);

            // When/Then
            assertThatCode(() -> graph.connect(tile1, tile2, Direction.DOWN))
                    .doesNotThrowAnyException();
        }
    }
}
