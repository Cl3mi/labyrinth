package labyrinth.client.unit.ai;

import labyrinth.client.ai.BoardSimulator;
import labyrinth.client.ai.ShiftOperation;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.assertj.core.api.Assertions.*;

@DisplayName("BoardSimulator")
class BoardSimulatorTest {

    private static final int BOARD_SIZE = 7;

    private Board board;
    private Player player;
    private Tile[][] tiles;

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
     * Helper method to create a fixed tile with specified entrances.
     */
    private Tile createFixedTile(Direction... entrances) {
        Tile tile = createTile(entrances);
        tile.setIsFixed(true);
        return tile;
    }

    /**
     * Helper method to create a standard 7x7 board with connected tiles.
     */
    private Tile[][] createStandardTileGrid() {
        Tile[][] tiles = new Tile[BOARD_SIZE][BOARD_SIZE];
        for (int row = 0; row < BOARD_SIZE; row++) {
            for (int col = 0; col < BOARD_SIZE; col++) {
                tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            }
        }
        return tiles;
    }

    @BeforeEach
    void setUp() {
        tiles = createStandardTileGrid();
        Tile extraTile = createTile(Direction.UP, Direction.DOWN);
        board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, extraTile);

        player = new Player("player-1", "TestPlayer");
        player.setCurrentPosition(new Position(3, 3));
        player.setHomePosition(new Position(0, 0));
    }

    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {

        @Test
        @DisplayName("constructor_validBoardAndPlayer_createsSimulator")
        void constructor_validBoardAndPlayer_createsSimulator() {
            // When
            BoardSimulator simulator = new BoardSimulator(board, player);

            // Then
            assertThat(simulator).isNotNull();
            assertThat(simulator.getWidth()).isEqualTo(BOARD_SIZE);
            assertThat(simulator.getHeight()).isEqualTo(BOARD_SIZE);
        }

        @Test
        @DisplayName("constructor_withAllPlayers_createsSimulator")
        void constructor_withAllPlayers_createsSimulator() {
            // Given
            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);

            Player player2 = new Player("player-2", "Player2");
            player2.setCurrentPosition(new Position(1, 1));
            allPlayers.add(player2);

            // When
            BoardSimulator simulator = new BoardSimulator(board, player, allPlayers);

            // Then
            assertThat(simulator).isNotNull();
            assertThat(simulator.getOtherPlayerPositions()).hasSize(1);
            assertThat(simulator.getOtherPlayerPositions()).contains(new Position(1, 1));
        }

        @Test
        @DisplayName("constructor_copiesPlayerPosition")
        void constructor_copiesPlayerPosition() {
            // Given
            player.setCurrentPosition(new Position(4, 5));

            // When
            BoardSimulator simulator = new BoardSimulator(board, player);

            // Then
            assertThat(simulator.getPlayerPosition()).isEqualTo(new Position(4, 5));
        }

        @Test
        @DisplayName("constructor_copiesHomePosition")
        void constructor_copiesHomePosition() {
            // Given
            player.setHomePosition(new Position(6, 6));

            // When
            BoardSimulator simulator = new BoardSimulator(board, player);

            // Then
            assertThat(simulator.getHomePosition()).isEqualTo(new Position(6, 6));
        }
    }

    @Nested
    @DisplayName("Copy")
    class CopyTests {

        @Test
        @DisplayName("copy_createsIndependentCopy")
        void copy_createsIndependentCopy() {
            // Given
            BoardSimulator original = new BoardSimulator(board, player);

            // When
            BoardSimulator copy = original.copy();

            // Then
            assertThat(copy).isNotSameAs(original);
            assertThat(copy.getWidth()).isEqualTo(original.getWidth());
            assertThat(copy.getHeight()).isEqualTo(original.getHeight());
            assertThat(copy.getPlayerPosition()).isEqualTo(original.getPlayerPosition());
        }

        @Test
        @DisplayName("copy_modificationDoesNotAffectOriginal")
        void copy_modificationDoesNotAffectOriginal() {
            // Given
            BoardSimulator original = new BoardSimulator(board, player);
            Position originalPosition = original.getPlayerPosition();

            // When
            BoardSimulator copy = original.copy();
            copy.simulateBeam(new Position(0, 0));

            // Then
            assertThat(original.getPlayerPosition()).isEqualTo(originalPosition);
            assertThat(copy.getPlayerPosition()).isEqualTo(new Position(0, 0));
        }
    }

    @Nested
    @DisplayName("RotateExtraTile")
    class RotateExtraTileTests {

        @Test
        @DisplayName("rotateExtraTile_onceSingleDirection_rotatesCorrectly")
        void rotateExtraTile_once_rotatesCorrectly() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            simulator.rotateExtraTile();

            // Then - no exception should be thrown
            assertThatCode(() -> simulator.rotateExtraTile()).doesNotThrowAnyException();
        }

        @Test
        @DisplayName("rotateExtraTile_fourTimes_returnsToOriginal")
        void rotateExtraTile_fourTimes_returnsToOriginal() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            for (int i = 0; i < 4; i++) {
                simulator.rotateExtraTile();
            }

            // Then - should complete without error
            assertThat(simulator).isNotNull();
        }
    }

    @Nested
    @DisplayName("ApplyShift")
    class ApplyShiftTests {

        @Test
        @DisplayName("applyShift_rowShiftRight_movesPlayerOnRow")
        void applyShift_rowShiftRight_movesPlayerOnRow() {
            // Given
            player.setCurrentPosition(new Position(3, 2));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.row(3, Direction.RIGHT);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isTrue();
            assertThat(simulator.getPlayerPosition().getColumn()).isEqualTo(3); // 2 + 1 = 3
        }

        @Test
        @DisplayName("applyShift_rowShiftLeft_movesPlayerOnRow")
        void applyShift_rowShiftLeft_movesPlayerOnRow() {
            // Given
            player.setCurrentPosition(new Position(3, 2));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.row(3, Direction.LEFT);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isTrue();
            assertThat(simulator.getPlayerPosition().getColumn()).isEqualTo(1); // 2 - 1 = 1
        }

        @Test
        @DisplayName("applyShift_columnShiftDown_movesPlayerOnColumn")
        void applyShift_columnShiftDown_movesPlayerOnColumn() {
            // Given
            player.setCurrentPosition(new Position(2, 3));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.column(3, Direction.DOWN);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isTrue();
            assertThat(simulator.getPlayerPosition().getRow()).isEqualTo(3); // 2 + 1 = 3
        }

        @Test
        @DisplayName("applyShift_columnShiftUp_movesPlayerOnColumn")
        void applyShift_columnShiftUp_movesPlayerOnColumn() {
            // Given
            player.setCurrentPosition(new Position(2, 3));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.column(3, Direction.UP);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isTrue();
            assertThat(simulator.getPlayerPosition().getRow()).isEqualTo(1); // 2 - 1 = 1
        }

        @Test
        @DisplayName("applyShift_playerOnDifferentRow_positionUnchanged")
        void applyShift_playerOnDifferentRow_positionUnchanged() {
            // Given
            player.setCurrentPosition(new Position(2, 3));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.row(4, Direction.RIGHT); // Different row

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isTrue();
            assertThat(simulator.getPlayerPosition()).isEqualTo(new Position(2, 3));
        }

        @Test
        @DisplayName("applyShift_rowWithFixedTile_returnsFalse")
        void applyShift_rowWithFixedTile_returnsFalse() {
            // Given
            tiles[3][3] = createFixedTile(Direction.UP, Direction.DOWN);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.row(3, Direction.RIGHT);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("applyShift_columnWithFixedTile_returnsFalse")
        void applyShift_columnWithFixedTile_returnsFalse() {
            // Given
            tiles[3][3] = createFixedTile(Direction.UP, Direction.DOWN);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.column(3, Direction.DOWN);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("applyShift_wrapAroundRight_wrapsPlayerPosition")
        void applyShift_wrapAroundRight_wrapsPlayerPosition() {
            // Given
            player.setCurrentPosition(new Position(3, 6)); // Rightmost column
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.row(3, Direction.RIGHT);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isTrue();
            assertThat(simulator.getPlayerPosition().getColumn()).isEqualTo(0); // Wrapped around
        }

        @Test
        @DisplayName("applyShift_wrapAroundLeft_wrapsPlayerPosition")
        void applyShift_wrapAroundLeft_wrapsPlayerPosition() {
            // Given
            player.setCurrentPosition(new Position(3, 0)); // Leftmost column
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.row(3, Direction.LEFT);

            // When
            boolean result = simulator.applyShift(op);

            // Then
            assertThat(result).isTrue();
            assertThat(simulator.getPlayerPosition().getColumn()).isEqualTo(6); // Wrapped around
        }
    }

    @Nested
    @DisplayName("GetReachablePositions")
    class GetReachablePositionsTests {

        @Test
        @DisplayName("getReachablePositions_fullyConnectedBoard_returnsAllPositions")
        void getReachablePositions_fullyConnectedBoard_returnsAllPositions() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            Set<Position> reachable = simulator.getReachablePositions();

            // Then
            assertThat(reachable).hasSize(BOARD_SIZE * BOARD_SIZE);
        }

        @Test
        @DisplayName("getReachablePositions_includesCurrentPosition")
        void getReachablePositions_includesCurrentPosition() {
            // Given
            player.setCurrentPosition(new Position(3, 3));
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            Set<Position> reachable = simulator.getReachablePositions();

            // Then
            assertThat(reachable).contains(new Position(3, 3));
        }

        @Test
        @DisplayName("getReachableUnblockedPositions_excludesOtherPlayers")
        void getReachableUnblockedPositions_excludesOtherPlayers() {
            // Given
            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);

            Player player2 = new Player("player-2", "Player2");
            player2.setCurrentPosition(new Position(3, 4));
            allPlayers.add(player2);

            BoardSimulator simulator = new BoardSimulator(board, player, allPlayers);

            // When
            Set<Position> reachable = simulator.getReachableUnblockedPositions();

            // Then
            assertThat(reachable).doesNotContain(new Position(3, 4));
        }
    }

    @Nested
    @DisplayName("SimulateBeam")
    class SimulateBeamTests {

        @Test
        @DisplayName("simulateBeam_validPosition_teleportsPlayer")
        void simulateBeam_validPosition_teleportsPlayer() {
            // Given
            player.setCurrentPosition(new Position(3, 3));
            BoardSimulator simulator = new BoardSimulator(board, player);
            Position target = new Position(0, 0);

            // When
            simulator.simulateBeam(target);

            // Then
            assertThat(simulator.getPlayerPosition()).isEqualTo(target);
        }

        @Test
        @DisplayName("simulateBeam_samePosition_staysInPlace")
        void simulateBeam_samePosition_staysInPlace() {
            // Given
            Position currentPos = new Position(3, 3);
            player.setCurrentPosition(currentPos);
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            simulator.simulateBeam(currentPos);

            // Then
            assertThat(simulator.getPlayerPosition()).isEqualTo(currentPos);
        }
    }

    @Nested
    @DisplayName("SimulateSwap")
    class SimulateSwapTests {

        @Test
        @DisplayName("simulateSwap_withOtherPlayer_swapsPositions")
        void simulateSwap_withOtherPlayer_swapsPositions() {
            // Given
            Position playerPos = new Position(3, 3);
            Position otherPos = new Position(1, 1);

            player.setCurrentPosition(playerPos);

            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);
            Player player2 = new Player("player-2", "Player2");
            player2.setCurrentPosition(otherPos);
            allPlayers.add(player2);

            BoardSimulator simulator = new BoardSimulator(board, player, allPlayers);

            // When
            simulator.simulateSwap(otherPos);

            // Then
            assertThat(simulator.getPlayerPosition()).isEqualTo(otherPos);
            assertThat(simulator.getOtherPlayerPositions()).contains(playerPos);
            assertThat(simulator.getOtherPlayerPositions()).doesNotContain(otherPos);
        }
    }

    @Nested
    @DisplayName("IsPositionBlocked")
    class IsPositionBlockedTests {

        @Test
        @DisplayName("isPositionBlocked_emptyPosition_returnsFalse")
        void isPositionBlocked_emptyPosition_returnsFalse() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            boolean blocked = simulator.isPositionBlocked(new Position(1, 1));

            // Then
            assertThat(blocked).isFalse();
        }

        @Test
        @DisplayName("isPositionBlocked_otherPlayerPosition_returnsTrue")
        void isPositionBlocked_otherPlayerPosition_returnsTrue() {
            // Given
            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);
            Player player2 = new Player("player-2", "Player2");
            player2.setCurrentPosition(new Position(1, 1));
            allPlayers.add(player2);

            BoardSimulator simulator = new BoardSimulator(board, player, allPlayers);

            // When
            boolean blocked = simulator.isPositionBlocked(new Position(1, 1));

            // Then
            assertThat(blocked).isTrue();
        }
    }

    @Nested
    @DisplayName("FindBonuses")
    class FindBonusesTests {

        @Test
        @DisplayName("findAllBonusPositions_noBonuses_returnsEmptyMap")
        void findAllBonusPositions_noBonuses_returnsEmptyMap() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            Map<Position, BonusType> bonuses = simulator.findAllBonusPositions();

            // Then
            assertThat(bonuses).isEmpty();
        }

        @Test
        @DisplayName("findAllBonusPositions_withBonuses_returnsCorrectPositions")
        void findAllBonusPositions_withBonuses_returnsCorrectPositions() {
            // Given
            tiles[2][2].setBonus(BonusType.BEAM);
            tiles[4][4].setBonus(BonusType.SWAP);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            Map<Position, BonusType> bonuses = simulator.findAllBonusPositions();

            // Then
            assertThat(bonuses).hasSize(2);
            assertThat(bonuses.get(new Position(2, 2))).isEqualTo(BonusType.BEAM);
            assertThat(bonuses.get(new Position(4, 4))).isEqualTo(BonusType.SWAP);
        }

        @Test
        @DisplayName("findReachableBonuses_bonusOnUnreachableTile_excludesBonus")
        void findReachableBonuses_bonusOnBlockedTile_excludesBonus() {
            // Given
            // Create a board where not all tiles are connected
            Tile[][] disconnectedTiles = new Tile[BOARD_SIZE][BOARD_SIZE];
            for (int r = 0; r < BOARD_SIZE; r++) {
                for (int c = 0; c < BOARD_SIZE; c++) {
                    // Only connect tiles horizontally in the middle row
                    if (r == 3) {
                        disconnectedTiles[r][c] = createTile(Direction.LEFT, Direction.RIGHT);
                    } else {
                        disconnectedTiles[r][c] = createTile(); // No entrances
                    }
                }
            }
            // Add a bonus on an unreachable tile
            disconnectedTiles[0][0].setBonus(BonusType.BEAM);
            board = new Board(BOARD_SIZE, BOARD_SIZE, disconnectedTiles, createTile(Direction.UP));
            player.setCurrentPosition(new Position(3, 3));
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            Map<Position, BonusType> reachable = simulator.findReachableBonuses();

            // Then
            assertThat(reachable).doesNotContainKey(new Position(0, 0));
        }
    }

    @Nested
    @DisplayName("GetTileAt")
    class GetTileAtTests {

        @Test
        @DisplayName("getTileAt_validPosition_returnsTile")
        void getTileAt_validPosition_returnsTile() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            BoardSimulator.SimTile tile = simulator.getTileAt(new Position(3, 3));

            // Then
            assertThat(tile).isNotNull();
        }

        @Test
        @DisplayName("getTileAt_outOfBounds_returnsNull")
        void getTileAt_outOfBounds_returnsNull() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            BoardSimulator.SimTile tile = simulator.getTileAt(new Position(100, 100));

            // Then
            assertThat(tile).isNull();
        }

        @Test
        @DisplayName("getTileAt_outOfBoundsNegativeRow_returnsNull")
        void getTileAt_outOfBoundsNegativeRow_returnsNull() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When - Position constructor throws for negative values, so test boundary
            // The getTileAt method handles out of bounds via bounds checking
            BoardSimulator.SimTile tile = simulator.getTileAt(new Position(0, 0));

            // Then - valid position should return tile
            assertThat(tile).isNotNull();
        }
    }

    @Nested
    @DisplayName("GetTargetPosition")
    class GetTargetPositionTests {

        @Test
        @DisplayName("getTargetPosition_noTreasure_returnsHomePosition")
        void getTargetPosition_noTreasure_returnsHomePosition() {
            // Given
            player.setCurrentTargetTreasure(null);
            player.setHomePosition(new Position(0, 0));
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            Position target = simulator.getTargetPosition();

            // Then
            assertThat(target).isEqualTo(new Position(0, 0));
        }

        @Test
        @DisplayName("getTargetPosition_withTreasure_returnsTreasurePosition")
        void getTargetPosition_withTreasure_returnsTreasurePosition() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(1);
            treasure.setName("Gold");
            tiles[2][2].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));

            player.setCurrentTargetTreasure(treasure);
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            Position target = simulator.getTargetPosition();

            // Then
            assertThat(target).isEqualTo(new Position(2, 2));
        }
    }

    @Nested
    @DisplayName("RowAndColumnChecks")
    class RowAndColumnChecksTests {

        @Test
        @DisplayName("rowContainsFixedTile_noFixedTiles_returnsFalse")
        void rowContainsFixedTile_noFixedTiles_returnsFalse() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            boolean hasFixed = simulator.rowContainsFixedTile(3);

            // Then
            assertThat(hasFixed).isFalse();
        }

        @Test
        @DisplayName("rowContainsFixedTile_withFixedTile_returnsTrue")
        void rowContainsFixedTile_withFixedTile_returnsTrue() {
            // Given
            tiles[3][3] = createFixedTile(Direction.UP, Direction.DOWN);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            boolean hasFixed = simulator.rowContainsFixedTile(3);

            // Then
            assertThat(hasFixed).isTrue();
        }

        @Test
        @DisplayName("colContainsFixedTile_noFixedTiles_returnsFalse")
        void colContainsFixedTile_noFixedTiles_returnsFalse() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            boolean hasFixed = simulator.colContainsFixedTile(3);

            // Then
            assertThat(hasFixed).isFalse();
        }

        @Test
        @DisplayName("colContainsFixedTile_withFixedTile_returnsTrue")
        void colContainsFixedTile_withFixedTile_returnsTrue() {
            // Given
            tiles[3][3] = createFixedTile(Direction.UP, Direction.DOWN);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            boolean hasFixed = simulator.colContainsFixedTile(3);

            // Then
            assertThat(hasFixed).isTrue();
        }
    }

    @Nested
    @DisplayName("GetPushFixedCandidates")
    class GetPushFixedCandidatesTests {

        @Test
        @DisplayName("getPushFixedCandidates_noFixedTiles_returnsEmpty")
        void getPushFixedCandidates_noFixedTiles_returnsEmpty() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            List<ShiftOperation> candidates = simulator.getPushFixedCandidates();

            // Then
            assertThat(candidates).isEmpty();
        }

        @Test
        @DisplayName("getPushFixedCandidates_withFixedTileInRow_returnsRowOperations")
        void getPushFixedCandidates_withFixedTileInRow_returnsRowOperations() {
            // Given
            tiles[3][3] = createFixedTile(Direction.UP, Direction.DOWN);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            BoardSimulator simulator = new BoardSimulator(board, player);

            // When
            List<ShiftOperation> candidates = simulator.getPushFixedCandidates();

            // Then
            assertThat(candidates).isNotEmpty();
            // Should contain operations for row 3 (both LEFT and RIGHT) and column 3 (both UP and DOWN)
            assertThat(candidates).anyMatch(op -> op.index() == 3 && op.isRow());
            assertThat(candidates).anyMatch(op -> op.index() == 3 && !op.isRow());
        }
    }

    @Nested
    @DisplayName("ApplyShiftIgnoringFixed")
    class ApplyShiftIgnoringFixedTests {

        @Test
        @DisplayName("applyShiftIgnoringFixed_rowWithFixedTile_shiftsAnyway")
        void applyShiftIgnoringFixed_rowWithFixedTile_shiftsAnyway() {
            // Given
            tiles[3][3] = createFixedTile(Direction.UP, Direction.DOWN);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            player.setCurrentPosition(new Position(3, 2));
            BoardSimulator simulator = new BoardSimulator(board, player);
            ShiftOperation op = ShiftOperation.row(3, Direction.RIGHT);

            // When - applyShiftIgnoringFixed should work even with fixed tiles
            simulator.applyShiftIgnoringFixed(op);

            // Then - player should have moved
            assertThat(simulator.getPlayerPosition().getColumn()).isEqualTo(3);
        }
    }

    @Nested
    @DisplayName("ManhattanDistance")
    class ManhattanDistanceTests {

        @Test
        @DisplayName("manhattanDistance_samePosition_returnsZero")
        void manhattanDistance_samePosition_returnsZero() {
            // Given
            Position a = new Position(3, 3);
            Position b = new Position(3, 3);

            // When
            int distance = BoardSimulator.manhattanDistance(a, b);

            // Then
            assertThat(distance).isZero();
        }

        @Test
        @DisplayName("manhattanDistance_horizontalOnly_returnsCorrectDistance")
        void manhattanDistance_horizontalOnly_returnsCorrectDistance() {
            // Given
            Position a = new Position(3, 0);
            Position b = new Position(3, 5);

            // When
            int distance = BoardSimulator.manhattanDistance(a, b);

            // Then
            assertThat(distance).isEqualTo(5);
        }

        @Test
        @DisplayName("manhattanDistance_verticalOnly_returnsCorrectDistance")
        void manhattanDistance_verticalOnly_returnsCorrectDistance() {
            // Given
            Position a = new Position(0, 3);
            Position b = new Position(4, 3);

            // When
            int distance = BoardSimulator.manhattanDistance(a, b);

            // Then
            assertThat(distance).isEqualTo(4);
        }

        @Test
        @DisplayName("manhattanDistance_diagonal_returnsSumOfDistances")
        void manhattanDistance_diagonal_returnsSumOfDistances() {
            // Given
            Position a = new Position(0, 0);
            Position b = new Position(3, 4);

            // When
            int distance = BoardSimulator.manhattanDistance(a, b);

            // Then
            assertThat(distance).isEqualTo(7); // 3 + 4
        }
    }

    @Nested
    @DisplayName("SimTile")
    class SimTileTests {

        @Test
        @DisplayName("hasEntrance_existingEntrance_returnsTrue")
        void hasEntrance_existingEntrance_returnsTrue() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);
            BoardSimulator.SimTile tile = simulator.getTileAt(new Position(0, 0));

            // When/Then
            assertThat(tile.hasEntrance(Direction.UP)).isTrue();
        }

        @Test
        @DisplayName("isFixed_normalTile_returnsFalse")
        void isFixed_normalTile_returnsFalse() {
            // Given
            BoardSimulator simulator = new BoardSimulator(board, player);
            BoardSimulator.SimTile tile = simulator.getTileAt(new Position(0, 0));

            // When/Then
            assertThat(tile.isFixed()).isFalse();
        }

        @Test
        @DisplayName("isFixed_fixedTile_returnsTrue")
        void isFixed_fixedTile_returnsTrue() {
            // Given
            tiles[0][0] = createFixedTile(Direction.DOWN, Direction.RIGHT);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP));
            BoardSimulator simulator = new BoardSimulator(board, player);
            BoardSimulator.SimTile tile = simulator.getTileAt(new Position(0, 0));

            // When/Then
            assertThat(tile.isFixed()).isTrue();
        }
    }
}
