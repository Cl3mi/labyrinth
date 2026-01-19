package labyrinth.server.game.models;

import labyrinth.server.game.abstractions.IMovementManager;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.records.Position;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Tests for the Board class.
 * Validates board creation, shifting mechanics, player movement, and edge cases.
 */
class BoardTest {

    private Board board;
    private BiMap<Position, Tile> tileMap;
    private Tile extraTile;

    @BeforeEach
    void setUp() {
        // Create a 7x7 board with all tiles having cross entrances
        tileMap = new BiMap<>();
        for (int row = 0; row < 7; row++) {
            for (int col = 0; col < 7; col++) {
                Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT));
                // Mark fixed tiles at even row AND even column
                if (row % 2 == 0 && col % 2 == 0) {
                    tile.setIsFixed(true);
                }
                tileMap.put(new Position(row, col), tile);
            }
        }
        extraTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
        board = new Board(7, 7, tileMap, extraTile);
        board.setPlayers(new ArrayList<>());
    }

    @Nested
    class BoardCreation {

        @Test
        void shouldCreateBoardWithCorrectDimensions() {
            // Assert
            assertEquals(7, board.getWidth());
            assertEquals(7, board.getHeight());
        }

        @Test
        void shouldStoreExtraTile() {
            // Assert
            assertEquals(extraTile, board.getExtraTile());
        }

        @Test
        void shouldThrowExceptionForWidthLessThanThree() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Board(1, 3, new BiMap<>(), extraTile)
            );
        }

        @Test
        void shouldThrowExceptionForWidthGreaterThanEleven() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Board(13, 3, new BiMap<>(), extraTile)
            );
        }

        @Test
        void shouldThrowExceptionForHeightLessThanThree() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Board(3, 1, new BiMap<>(), extraTile)
            );
        }

        @Test
        void shouldThrowExceptionForHeightGreaterThanEleven() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Board(3, 13, new BiMap<>(), extraTile)
            );
        }

        @Test
        void shouldThrowExceptionForEvenWidth() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Board(4, 3, new BiMap<>(), extraTile)
            );
        }

        @Test
        void shouldThrowExceptionForEvenHeight() {
            // Act & Assert
            assertThrows(IllegalArgumentException.class, () ->
                    new Board(3, 4, new BiMap<>(), extraTile)
            );
        }

        @Test
        void shouldCreateMinimumSizedBoard() {
            // Arrange
            BiMap<Position, Tile> smallTileMap = new BiMap<>();
            for (int row = 0; row < 3; row++) {
                for (int col = 0; col < 3; col++) {
                    smallTileMap.put(new Position(row, col),
                            new Tile(EnumSet.of(Direction.UP, Direction.DOWN)));
                }
            }

            // Act
            Board smallBoard = new Board(3, 3, smallTileMap, extraTile);

            // Assert
            assertEquals(3, smallBoard.getWidth());
            assertEquals(3, smallBoard.getHeight());
        }
    }

    @Nested
    class TileAccess {

        @Test
        void shouldGetTileAtPosition() {
            // Arrange
            Tile expectedTile = tileMap.getForward(new Position(1, 1));

            // Act
            Tile tile = board.getTileAt(1, 1);

            // Assert
            assertEquals(expectedTile, tile);
        }

        @Test
        void shouldGetTileAtPositionObject() {
            // Arrange
            Position pos = new Position(2, 3);
            Tile expectedTile = tileMap.getForward(pos);

            // Act
            Tile tile = board.getTileAt(pos);

            // Assert
            assertEquals(expectedTile, tile);
        }

        @Test
        void shouldGetPositionOfTile() {
            // Arrange
            Position expectedPos = new Position(2, 3);
            Tile tile = board.getTileAt(expectedPos);

            // Act
            Position pos = board.getPositionOfTile(tile);

            // Assert
            assertEquals(expectedPos, pos);
        }

        @Test
        void shouldReturnNullForTileNotOnBoard() {
            // Arrange
            Tile unknownTile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));

            // Act
            Position pos = board.getPositionOfTile(unknownTile);

            // Assert
            assertNull(pos);
        }
    }

    @Nested
    class ColumnShiftDown {

        @Test
        void shouldShiftColumnDownSuccessfully() {
            // Arrange
            Tile originalTop = board.getTileAt(0, 1);
            Tile originalBottom = board.getTileAt(6, 1);

            // Act
            boolean result = board.shiftColumnDown(1, false);

            // Assert
            assertTrue(result);
            assertEquals(extraTile, board.getTileAt(0, 1), "Extra tile should be at top");
            assertEquals(originalBottom, board.getExtraTile(), "Bottom tile should become extra");
        }

        @Test
        void shouldNotShiftFirstColumn() {
            // Act
            boolean result = board.shiftColumnDown(0, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotShiftLastColumn() {
            // Act
            boolean result = board.shiftColumnDown(6, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotShiftColumnWithFixedTiles() {
            // Act - column 2 has fixed tiles at rows 0, 2, 4, 6
            boolean result = board.shiftColumnDown(2, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldShiftColumnWithFixedTilesWhenBonusActive() {
            // Act - column 2 has fixed tiles, but bonus is active
            boolean result = board.shiftColumnDown(2, true);

            // Assert
            assertTrue(result);
        }

        @Test
        void shouldMovePlayerOnPushedOutTile() {
            // Arrange
            Player player = new Player(UUID.randomUUID(), "TestPlayer");
            Tile bottomTile = board.getTileAt(6, 1);
            player.setCurrentTile(bottomTile);
            board.setPlayers(List.of(player));

            // The old extra tile before shift
            Tile oldExtraTile = board.getExtraTile();

            // Act
            board.shiftColumnDown(1, false);

            // Assert - player should now be on what was previously the extra tile
            // which is now inserted at position (0, 1)
            assertEquals(oldExtraTile, player.getCurrentTile());
        }
    }

    @Nested
    class ColumnShiftUp {

        @Test
        void shouldShiftColumnUpSuccessfully() {
            // Arrange
            Tile originalTop = board.getTileAt(0, 1);
            Tile originalBottom = board.getTileAt(6, 1);

            // Act
            boolean result = board.shiftColumnUp(1, false);

            // Assert
            assertTrue(result);
            assertEquals(extraTile, board.getTileAt(6, 1), "Extra tile should be at bottom");
            assertEquals(originalTop, board.getExtraTile(), "Top tile should become extra");
        }

        @Test
        void shouldNotShiftFirstColumn() {
            // Act
            boolean result = board.shiftColumnUp(0, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotShiftLastColumn() {
            // Act
            boolean result = board.shiftColumnUp(6, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotShiftColumnWithFixedTiles() {
            // Act
            boolean result = board.shiftColumnUp(2, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldShiftColumnWithFixedTilesWhenBonusActive() {
            // Act
            boolean result = board.shiftColumnUp(2, true);

            // Assert
            assertTrue(result);
        }
    }

    @Nested
    class RowShiftLeft {

        @Test
        void shouldShiftRowLeftSuccessfully() {
            // Arrange
            Tile originalLeft = board.getTileAt(1, 0);
            Tile originalRight = board.getTileAt(1, 6);

            // Act
            boolean result = board.shiftRowLeft(1, false);

            // Assert
            assertTrue(result);
            assertEquals(extraTile, board.getTileAt(1, 6), "Extra tile should be at right");
            assertEquals(originalLeft, board.getExtraTile(), "Left tile should become extra");
        }

        @Test
        void shouldNotShiftFirstRow() {
            // Act
            boolean result = board.shiftRowLeft(0, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotShiftLastRow() {
            // Act
            boolean result = board.shiftRowLeft(6, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotShiftRowWithFixedTiles() {
            // Act - row 2 has fixed tiles
            boolean result = board.shiftRowLeft(2, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldShiftRowWithFixedTilesWhenBonusActive() {
            // Act
            boolean result = board.shiftRowLeft(2, true);

            // Assert
            assertTrue(result);
        }
    }

    @Nested
    class RowShiftRight {

        @Test
        void shouldShiftRowRightSuccessfully() {
            // Arrange
            Tile originalLeft = board.getTileAt(1, 0);
            Tile originalRight = board.getTileAt(1, 6);

            // Act
            boolean result = board.shiftRowRight(1, false);

            // Assert
            assertTrue(result);
            assertEquals(extraTile, board.getTileAt(1, 0), "Extra tile should be at left");
            assertEquals(originalRight, board.getExtraTile(), "Right tile should become extra");
        }

        @Test
        void shouldNotShiftFirstRow() {
            // Act
            boolean result = board.shiftRowRight(0, false);

            // Assert
            assertFalse(result);
        }

        @Test
        void shouldNotShiftLastRow() {
            // Act
            boolean result = board.shiftRowRight(6, false);

            // Assert
            assertFalse(result);
        }
    }

    @Nested
    class FixedTileDetection {

        @Test
        void shouldDetectFixedTilesInColumn() {
            // Assert - column 2 has fixed tiles at even rows
            assertTrue(board.colContainsFixedTile(2));
        }

        @Test
        void shouldNotDetectFixedTilesInOddColumn() {
            // Assert - column 1 has no fixed tiles (odd column)
            assertFalse(board.colContainsFixedTile(1));
        }

        @Test
        void shouldDetectFixedTilesInRow() {
            // Assert - row 2 has fixed tiles at even columns
            assertTrue(board.rowContainsFixedTile(2));
        }

        @Test
        void shouldNotDetectFixedTilesInOddRow() {
            // Assert - row 1 has no fixed tiles (odd row)
            assertFalse(board.rowContainsFixedTile(1));
        }

        @Test
        void shouldIgnoreFixedTilesInFreeRoamMode() {
            // Arrange
            board.setFreeRoam(true);

            // Assert - should return false even for columns with fixed tiles
            assertFalse(board.colContainsFixedTile(2));
            assertFalse(board.rowContainsFixedTile(2));
        }
    }

    @Nested
    class RecalculateFixedTiles {

        @Test
        void shouldRecalculateFixedTilesCorrectly() {
            // Arrange - unset all fixed flags first
            for (int row = 0; row < 7; row++) {
                for (int col = 0; col < 7; col++) {
                    board.getTileAt(row, col).setIsFixed(false);
                }
            }

            // Act
            board.recalculateFixedTiles();

            // Assert - tiles at even row AND even column should be fixed
            assertTrue(board.getTileAt(0, 0).isFixed(), "(0,0) should be fixed");
            assertTrue(board.getTileAt(2, 2).isFixed(), "(2,2) should be fixed");
            assertTrue(board.getTileAt(4, 4).isFixed(), "(4,4) should be fixed");
            assertFalse(board.getTileAt(1, 1).isFixed(), "(1,1) should NOT be fixed");
            assertFalse(board.getTileAt(0, 1).isFixed(), "(0,1) should NOT be fixed");
            assertFalse(board.getTileAt(1, 0).isFixed(), "(1,0) should NOT be fixed");
        }

        @Test
        void shouldEnsureExtraTileIsNotFixed() {
            // Arrange
            board.getExtraTile().setIsFixed(true);

            // Act
            board.recalculateFixedTiles();

            // Assert
            assertFalse(board.getExtraTile().isFixed(), "Extra tile should never be fixed");
        }
    }

    @Nested
    class ReachableTiles {

        @Test
        void shouldReturnReachableTilesForPlayer() {
            // Arrange
            Player player = new Player(UUID.randomUUID(), "TestPlayer");
            Tile startTile = board.getTileAt(1, 1);
            player.setCurrentTile(startTile);

            // Act
            Set<Tile> reachable = board.getReachableTiles(player);

            // Assert - in fully connected board, all tiles should be reachable
            assertEquals(49, reachable.size());
        }

        @Test
        void shouldReturnEmptySetForPlayerWithNoCurrentTile() {
            // Arrange
            Player player = new Player(UUID.randomUUID(), "TestPlayer");
            player.setCurrentTile(null);

            // Act
            Set<Tile> reachable = board.getReachableTiles(player);

            // Assert
            assertTrue(reachable.isEmpty());
        }
    }

    @Nested
    class CornerCoordinate {

        @ParameterizedTest
        @ValueSource(strings = {"0,0", "0,6", "6,0", "6,6"})
        void shouldIdentifyCornerCoordinates(String coords) {
            // Arrange
            String[] parts = coords.split(",");
            int row = Integer.parseInt(parts[0]);
            int col = Integer.parseInt(parts[1]);

            // Act & Assert
            assertTrue(board.isCornerCoordinate(row, col), coords + " should be a corner");
        }

        @Test
        void shouldNotIdentifyNonCornerAsCorner() {
            // Assert
            assertFalse(board.isCornerCoordinate(0, 1));
            assertFalse(board.isCornerCoordinate(1, 0));
            assertFalse(board.isCornerCoordinate(3, 3));
        }
    }

    @Nested
    class PlayerMovement {

        private IMovementManager movementManager;
        private Player player;

        @BeforeEach
        void setUpMovement() {
            movementManager = mock(IMovementManager.class);
            player = new Player(UUID.randomUUID(), "TestPlayer");
            Tile startTile = board.getTileAt(1, 1);
            player.setCurrentTile(startTile);
            board.setPlayers(List.of(player));
            when(movementManager.isTileBlockedByPlayer(any(), any(), any())).thenReturn(false);
        }

        @Test
        void shouldMovePlayerToReachableTile() {
            // Act
            int distance = board.movePlayerToTile(player, 1, 2, movementManager);

            // Assert
            assertTrue(distance > 0);
            verify(movementManager).processPlayerStepOnTile(eq(player), any(Tile.class));
        }

        @Test
        void shouldReturnMinusOneForUnreachableTile() {
            // Arrange - create a board with disconnected tiles
            BiMap<Position, Tile> isolatedTileMap = new BiMap<>();
            for (int row = 0; row < 3; row++) {
                for (int col = 0; col < 3; col++) {
                    // Only vertical connections
                    Tile tile = new Tile(EnumSet.of(Direction.UP, Direction.DOWN));
                    isolatedTileMap.put(new Position(row, col), tile);
                }
            }
            Board isolatedBoard = new Board(3, 3, isolatedTileMap, extraTile);
            isolatedBoard.setPlayers(List.of(player));

            // Position player in column 0
            player.setCurrentTile(isolatedBoard.getTileAt(0, 0));

            // Act - try to move to column 2 (unreachable)
            int distance = isolatedBoard.movePlayerToTile(player, 0, 2, movementManager);

            // Assert
            assertEquals(-1, distance);
        }

        @Test
        void shouldReturnMinusOneWhenTileIsBlocked() {
            // Arrange
            when(movementManager.isTileBlockedByPlayer(any(), any(), any())).thenReturn(true);

            // Act
            int distance = board.movePlayerToTile(player, 1, 2, movementManager);

            // Assert
            assertEquals(-1, distance);
        }
    }

    @Nested
    class BoardCopy {

        @Test
        void shouldCreateIndependentCopy() {
            // Act
            Board copy = board.copy();

            // Assert
            assertNotSame(board, copy);
            assertEquals(board.getWidth(), copy.getWidth());
            assertEquals(board.getHeight(), copy.getHeight());
        }

        @Test
        void shouldCopyExtraTile() {
            // Act
            Board copy = board.copy();

            // Assert
            assertNotSame(board.getExtraTile(), copy.getExtraTile());
        }

        @Test
        void shouldCopyFreeRoamSetting() {
            // Arrange
            board.setFreeRoam(true);

            // Act
            Board copy = board.copy();

            // Assert
            assertTrue(copy.isFreeRoam());
        }

        @Test
        void shouldCopyPlayersWithCorrectTileReferences() {
            // Arrange
            Player player = new Player(UUID.randomUUID(), "TestPlayer");
            Tile currentTile = board.getTileAt(1, 1);
            Tile homeTile = board.getTileAt(0, 0);
            player.setCurrentTile(currentTile);
            player.setHomeTile(homeTile);
            board.setPlayers(List.of(player));

            // Act
            Board copy = board.copy();

            // Assert
            assertNotNull(copy.getPlayers());
            assertEquals(1, copy.getPlayers().size());

            Player copiedPlayer = copy.getPlayers().get(0);
            assertNotNull(copiedPlayer.getCurrentTile());
            assertNotNull(copiedPlayer.getHomeTile());

            // Verify tiles are from the copied board, not the original
            assertNotSame(player.getCurrentTile(), copiedPlayer.getCurrentTile());
        }
    }

    @Nested
    class BoardListeners {

        @Test
        void shouldNotifyListenersOnColumnShift() {
            // Arrange
            var listener = mock(labyrinth.server.game.abstractions.IBoardEventListener.class);
            board.addListener(listener);

            // Act
            board.shiftColumnDown(1, false);

            // Assert
            verify(listener).onBoardEvent(any());
        }

        @Test
        void shouldNotifyListenersOnRowShift() {
            // Arrange
            var listener = mock(labyrinth.server.game.abstractions.IBoardEventListener.class);
            board.addListener(listener);

            // Act
            board.shiftRowLeft(1, false);

            // Assert
            verify(listener).onBoardEvent(any());
        }

        @Test
        void shouldRemoveListener() {
            // Arrange
            var listener = mock(labyrinth.server.game.abstractions.IBoardEventListener.class);
            board.addListener(listener);
            board.removeListener(listener);

            // Act
            board.shiftColumnDown(1, false);

            // Assert - only the graph listener should be called
            verify(listener, never()).onBoardEvent(any());
        }
    }
}
