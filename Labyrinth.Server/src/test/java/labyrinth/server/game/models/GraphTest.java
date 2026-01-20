package labyrinth.server.game.models;

import labyrinth.server.game.enums.BoardEventType;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.events.BoardEvent;
import labyrinth.server.game.models.records.Position;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.EnumSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Tests for the Graph class.
 * Validates BFS reachability, distance calculation, connectivity, and board event handling.
 */
class GraphTest {

    private Board board;
    private Graph graph;
    private BiMap<Position, Tile> tileMap;

    @BeforeEach
    void setUp() {
        // Create a 3x3 board with all tiles connected (cross-shaped tiles)
        tileMap = new BiMap<>();
        for (int row = 0; row < 3; row++) {
            for (int col = 0; col < 3; col++) {
                Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT));
                tileMap.put(new Position(row, col), tile);
            }
        }
        Tile extraTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
        board = new Board(3, 3, tileMap, extraTile);
        graph = board.getGraph();
    }

    @Nested
    class GraphInitialization {

        @Test
        void shouldInitializeGraphWithAllTiles() {
            // Assert
            for (int row = 0; row < 3; row++) {
                for (int col = 0; col < 3; col++) {
                    Tile tile = board.getTileAt(row, col);
                    assertNotNull(graph.getNeighbors(tile), "Should have entry for tile at " + row + "," + col);
                }
            }
        }

        @Test
        void shouldConnectAdjacentTilesWithMatchingEntrances() {
            // Arrange
            Tile center = board.getTileAt(1, 1);

            // Act
            Set<Tile> neighbors = graph.getNeighbors(center);

            // Assert - center should be connected to all 4 adjacent tiles
            assertEquals(4, neighbors.size(), "Center tile should have 4 neighbors");
        }

        @Test
        void shouldConnectCornerTilesWithTwoNeighbors() {
            // Arrange
            Tile topLeft = board.getTileAt(0, 0);

            // Act
            Set<Tile> neighbors = graph.getNeighbors(topLeft);

            // Assert - corner should have 2 neighbors (right and down)
            assertEquals(2, neighbors.size(), "Corner tile should have 2 neighbors");
        }

        @Test
        void shouldConnectEdgeTilesWithThreeNeighbors() {
            // Arrange
            Tile topMiddle = board.getTileAt(0, 1);

            // Act
            Set<Tile> neighbors = graph.getNeighbors(topMiddle);

            // Assert - edge should have 3 neighbors
            assertEquals(3, neighbors.size(), "Edge tile should have 3 neighbors");
        }
    }

    @Nested
    class TileConnection {

        @Test
        void shouldConnectTilesBidirectionally() {
            // Arrange
            BiMap<Position, Tile> customTileMap = new BiMap<>();
            Tile tile1 = new Tile(EnumSet.of(Direction.RIGHT, Direction.DOWN));
            Tile tile2 = new Tile(EnumSet.of(Direction.LEFT, Direction.DOWN));
            customTileMap.put(new Position(0, 0), tile1);
            customTileMap.put(new Position(0, 1), tile2);
            customTileMap.put(new Position(0, 2), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(1, 0), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(1, 1), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(1, 2), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(2, 0), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(2, 1), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(2, 2), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));

            Board customBoard = new Board(3, 3, customTileMap, new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            Graph customGraph = customBoard.getGraph();

            // Assert - tile1 and tile2 should be connected to each other
            assertTrue(customGraph.getNeighbors(tile1).contains(tile2), "tile1 should connect to tile2");
            assertTrue(customGraph.getNeighbors(tile2).contains(tile1), "tile2 should connect to tile1");
        }

        @Test
        void shouldNotConnectTilesWithoutMatchingEntrances() {
            // Arrange - create tiles that don't connect
            BiMap<Position, Tile> customTileMap = new BiMap<>();
            Tile tile1 = new Tile(EnumSet.of(Direction.UP, Direction.LEFT)); // faces up and left
            Tile tile2 = new Tile(EnumSet.of(Direction.UP, Direction.RIGHT)); // faces up and right
            customTileMap.put(new Position(0, 0), tile1);
            customTileMap.put(new Position(0, 1), tile2);
            customTileMap.put(new Position(0, 2), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(1, 0), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(1, 1), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(1, 2), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(2, 0), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(2, 1), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            customTileMap.put(new Position(2, 2), new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));

            Board customBoard = new Board(3, 3, customTileMap, new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            Graph customGraph = customBoard.getGraph();

            // Assert - tile1 has RIGHT but tile2 has RIGHT (not LEFT), so no connection
            assertFalse(customGraph.getNeighbors(tile1).contains(tile2), "tile1 should NOT connect to tile2");
        }
    }

    @Nested
    class FindReachable {

        @Test
        void shouldFindAllReachableTilesInFullyConnectedBoard() {
            // Arrange
            Tile startTile = board.getTileAt(0, 0);

            // Act
            Set<Tile> reachable = graph.findReachable(startTile);

            // Assert - all 9 tiles should be reachable
            assertEquals(9, reachable.size(), "All tiles should be reachable in fully connected board");
        }

        @Test
        void shouldIncludeStartTileInReachable() {
            // Arrange
            Tile startTile = board.getTileAt(1, 1);

            // Act
            Set<Tile> reachable = graph.findReachable(startTile);

            // Assert
            assertTrue(reachable.contains(startTile), "Start tile should be in reachable set");
        }

        @Test
        void shouldFindOnlyStartTileWhenIsolated() {
            // Arrange - create an isolated tile
            BiMap<Position, Tile> customTileMap = new BiMap<>();
            Tile isolatedTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN)); // only vertical
            Tile horizontalTile = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)); // only horizontal

            customTileMap.put(new Position(0, 0), isolatedTile);
            customTileMap.put(new Position(0, 1), horizontalTile);
            customTileMap.put(new Position(0, 2), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(1, 0), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(1, 1), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(1, 2), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(2, 0), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(2, 1), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(2, 2), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));

            Board customBoard = new Board(3, 3, customTileMap, new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            Graph customGraph = customBoard.getGraph();

            // Act
            Set<Tile> reachable = customGraph.findReachable(isolatedTile);

            // Assert - only the isolated tile itself should be reachable
            assertEquals(1, reachable.size(), "Only start tile should be reachable when isolated");
            assertTrue(reachable.contains(isolatedTile));
        }

        @Test
        void shouldFindConnectedComponentOnly() {
            // Arrange - create two disconnected groups
            BiMap<Position, Tile> customTileMap = new BiMap<>();

            // Left column - connected vertically
            Tile leftTop = new Tile(EnumSet.of(Direction.DOWN, Direction.UP));
            Tile leftMid = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            Tile leftBot = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Right column - connected but not to left
            Tile rightTop = new Tile(EnumSet.of(Direction.DOWN, Direction.UP));
            Tile rightMid = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            Tile rightBot = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Middle column - no horizontal connections
            Tile midTop = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            Tile midMid = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
            Tile midBot = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            customTileMap.put(new Position(0, 0), leftTop);
            customTileMap.put(new Position(1, 0), leftMid);
            customTileMap.put(new Position(2, 0), leftBot);
            customTileMap.put(new Position(0, 1), midTop);
            customTileMap.put(new Position(1, 1), midMid);
            customTileMap.put(new Position(2, 1), midBot);
            customTileMap.put(new Position(0, 2), rightTop);
            customTileMap.put(new Position(1, 2), rightMid);
            customTileMap.put(new Position(2, 2), rightBot);

            Board customBoard = new Board(3, 3, customTileMap, new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            Graph customGraph = customBoard.getGraph();

            // Act
            Set<Tile> reachableFromLeft = customGraph.findReachable(leftTop);

            // Assert - only left column (3 tiles) should be reachable
            assertEquals(3, reachableFromLeft.size(), "Only left column should be reachable");
            assertTrue(reachableFromLeft.contains(leftTop));
            assertTrue(reachableFromLeft.contains(leftMid));
            assertTrue(reachableFromLeft.contains(leftBot));
        }
    }

    @Nested
    class GetDistance {

        @Test
        void shouldReturnZeroForSameTile() {
            // Arrange
            Tile tile = board.getTileAt(1, 1);

            // Act
            int distance = graph.getDistance(tile, tile);

            // Assert
            assertEquals(0, distance);
        }

        @Test
        void shouldReturnOneForAdjacentTiles() {
            // Arrange
            Tile tile1 = board.getTileAt(1, 1);
            Tile tile2 = board.getTileAt(1, 2);

            // Act
            int distance = graph.getDistance(tile1, tile2);

            // Assert
            assertEquals(1, distance);
        }

        @Test
        void shouldReturnCorrectDistanceForFartherTiles() {
            // Arrange
            Tile topLeft = board.getTileAt(0, 0);
            Tile bottomRight = board.getTileAt(2, 2);

            // Act
            int distance = graph.getDistance(topLeft, bottomRight);

            // Assert - Manhattan distance in fully connected grid
            assertEquals(4, distance, "Distance from (0,0) to (2,2) should be 4");
        }

        @Test
        void shouldReturnMinusOneForUnreachableTiles() {
            // Arrange - create disconnected tiles
            BiMap<Position, Tile> customTileMap = new BiMap<>();
            Tile tile1 = new Tile(EnumSet.of(Direction.UP, Direction.DOWN)); // vertical only
            Tile tile2 = new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)); // horizontal only - not connected

            customTileMap.put(new Position(0, 0), tile1);
            customTileMap.put(new Position(0, 1), tile2);
            customTileMap.put(new Position(0, 2), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(1, 0), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(1, 1), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(1, 2), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(2, 0), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(2, 1), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));
            customTileMap.put(new Position(2, 2), new Tile(EnumSet.of(Direction.LEFT, Direction.RIGHT)));

            Board customBoard = new Board(3, 3, customTileMap, new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
            Graph customGraph = customBoard.getGraph();

            // Act
            int distance = customGraph.getDistance(tile1, tile2);

            // Assert
            assertEquals(-1, distance, "Should return -1 for unreachable tiles");
        }

        @Test
        void shouldFindShortestPath() {
            // Arrange - in a 3x3 fully connected board
            Tile topLeft = board.getTileAt(0, 0);
            Tile topRight = board.getTileAt(0, 2);

            // Act
            int distance = graph.getDistance(topLeft, topRight);

            // Assert - shortest path is 2 (right, right)
            assertEquals(2, distance);
        }
    }

    @Nested
    class BoardEventHandling {

        @Test
        void shouldUpdateConnectionsAfterRowShift() {
            // Arrange
            Tile centerTile = board.getTileAt(1, 1);
            Set<Tile> neighborsBefore = Set.copyOf(graph.getNeighbors(centerTile));

            // Act - simulate row shift event
            BoardEvent event = new BoardEvent(BoardEventType.ROW_SHIFTED, 1);
            graph.onBoardEvent(event);

            // Assert - connections should be recalculated (same in this case since tiles are all connected)
            assertNotNull(graph.getNeighbors(centerTile));
        }

        @Test
        void shouldUpdateConnectionsAfterColumnShift() {
            // Arrange
            Tile centerTile = board.getTileAt(1, 1);

            // Act - simulate column shift event
            BoardEvent event = new BoardEvent(BoardEventType.COLUMN_SHIFTED, 1);
            graph.onBoardEvent(event);

            // Assert - connections should be recalculated
            assertNotNull(graph.getNeighbors(centerTile));
        }

        @Test
        void shouldHandleRowZeroShiftEvent() {
            // Act - simulate row 0 shift
            BoardEvent event = new BoardEvent(BoardEventType.ROW_SHIFTED, 0);

            // Assert - should not throw
            assertDoesNotThrow(() -> graph.onBoardEvent(event));
        }

        @Test
        void shouldHandleLastRowShiftEvent() {
            // Act - simulate last row shift
            BoardEvent event = new BoardEvent(BoardEventType.ROW_SHIFTED, 2);

            // Assert - should not throw
            assertDoesNotThrow(() -> graph.onBoardEvent(event));
        }

        @Test
        void shouldHandleColumnZeroShiftEvent() {
            // Act - simulate column 0 shift
            BoardEvent event = new BoardEvent(BoardEventType.COLUMN_SHIFTED, 0);

            // Assert - should not throw
            assertDoesNotThrow(() -> graph.onBoardEvent(event));
        }

        @Test
        void shouldHandleLastColumnShiftEvent() {
            // Act - simulate last column shift
            BoardEvent event = new BoardEvent(BoardEventType.COLUMN_SHIFTED, 2);

            // Assert - should not throw
            assertDoesNotThrow(() -> graph.onBoardEvent(event));
        }
    }

    @Nested
    class GraphClear {

        @Test
        void shouldClearAllTiles() {
            // Arrange
            assertFalse(graph.getNeighbors(board.getTileAt(0, 0)).isEmpty());

            // Act
            graph.clear();

            // Assert
            assertTrue(graph.getNeighbors(board.getTileAt(0, 0)).isEmpty());
        }
    }

    @Nested
    class AddTile {

        @Test
        void shouldAddNewTile() {
            // Arrange
            Tile newTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Act
            graph.addTile(newTile);

            // Assert - tile should exist in graph with empty neighbors
            assertNotNull(graph.getNeighbors(newTile));
            assertTrue(graph.getNeighbors(newTile).isEmpty());
        }

        @Test
        void shouldNotDuplicateExistingTile() {
            // Arrange
            Tile existingTile = board.getTileAt(0, 0);
            int neighborCount = graph.getNeighbors(existingTile).size();

            // Act
            graph.addTile(existingTile);

            // Assert - neighbors should remain unchanged
            assertEquals(neighborCount, graph.getNeighbors(existingTile).size());
        }
    }
}
