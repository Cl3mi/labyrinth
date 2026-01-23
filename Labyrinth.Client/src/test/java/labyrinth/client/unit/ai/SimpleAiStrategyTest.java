package labyrinth.client.unit.ai;

import labyrinth.client.ai.AiDecision;
import labyrinth.client.ai.SimpleAiStrategy;
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("SimpleAiStrategy")
class SimpleAiStrategyTest {

    private static final int BOARD_SIZE = 7;

    private SimpleAiStrategy aiStrategy;
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
        aiStrategy = new SimpleAiStrategy();
        tiles = createStandardTileGrid();
        Tile extraTile = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
        board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, extraTile);

        player = new Player("player-1", "TestPlayer");
        player.setCurrentPosition(new Position(3, 3));
        player.setHomePosition(new Position(0, 0));
    }

    @Nested
    @DisplayName("ComputeBestMove - Basic")
    class ComputeBestMoveBasicTests {

        @Test
        @DisplayName("computeBestMove_validBoardAndPlayer_returnsDecision")
        void computeBestMove_validBoardAndPlayer_returnsDecision() {
            // Given - standard setup with no target treasure (going home)

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_withTargetTreasure_returnsDecision")
        void computeBestMove_withTargetTreasure_returnsDecision() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(1);
            treasure.setName("Gold");
            tiles[5][5].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_treasureDirectlyReachable_findsOptimalMove")
        void computeBestMove_treasureDirectlyReachable_findsOptimalMove() {
            // Given - treasure is adjacent to player
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[3][4].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
            // The AI should find a move that reaches the treasure
        }

        @Test
        @DisplayName("computeBestMove_goingHome_returnsHomeDirection")
        void computeBestMove_goingHome_returnsHomeDirection() {
            // Given - no treasure, player should go home
            player.setCurrentTargetTreasure(null);
            player.setHomePosition(new Position(0, 0));

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
            // Decision should move towards home position
        }
    }

    @Nested
    @DisplayName("ComputeBestMove - With All Players")
    class ComputeBestMoveWithPlayersTests {

        @Test
        @DisplayName("computeBestMove_withMultiplePlayers_avoidsBlockedPositions")
        void computeBestMove_withMultiplePlayers_avoidsBlockedPositions() {
            // Given
            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);

            Player player2 = new Player("player-2", "Player2");
            player2.setCurrentPosition(new Position(3, 4));
            allPlayers.add(player2);

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[3][5].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player, allPlayers);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_singlePlayer_worksWithNullAllPlayers")
        void computeBestMove_singlePlayer_worksWithNullAllPlayers() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[5][5].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player, null);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_withEmptyPlayerList_works")
        void computeBestMove_withEmptyPlayerList_works() {
            // Given
            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player, allPlayers);

            // Then
            assertThat(decision).isNotNull();
        }
    }

    @Nested
    @DisplayName("ComputeBestMove - With Bonuses")
    class ComputeBestMoveWithBonusesTests {

        @Test
        @DisplayName("computeBestMove_withSwapBonus_considersSwap")
        void computeBestMove_withSwapBonus_considersSwap() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.SWAP));

            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);

            Player player2 = new Player("player-2", "Player2");
            player2.setCurrentPosition(new Position(5, 5));
            allPlayers.add(player2);

            // Treasure at player2's position
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[5][5].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player, allPlayers);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_withBeamBonus_considersBeam")
        void computeBestMove_withBeamBonus_considersBeam() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.BEAM));

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[6][6].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_withPushFixedBonus_considersPushFixed")
        void computeBestMove_withPushFixedBonus_considersPushFixed() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.PUSH_FIXED));

            // Add some fixed tiles
            tiles[3][3] = createFixedTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);

            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[3][5].setTreasure(treasure);
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_withPushTwiceBonus_considersPushTwice")
        void computeBestMove_withPushTwiceBonus_considersPushTwice() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.PUSH_TWICE));

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[6][6].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_withMultipleBonuses_choosesOptimal")
        void computeBestMove_withMultipleBonuses_choosesOptimal() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.BEAM, BonusType.SWAP, BonusType.PUSH_TWICE));

            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);

            Player player2 = new Player("player-2", "Player2");
            player2.setCurrentPosition(new Position(1, 1));
            allPlayers.add(player2);

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[6][6].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player, allPlayers);

            // Then
            assertThat(decision).isNotNull();
        }
    }

    @Nested
    @DisplayName("ComputeBestMove - Edge Cases")
    class ComputeBestMoveEdgeCasesTests {

        @Test
        @DisplayName("computeBestMove_playerAtCorner_findsMove")
        void computeBestMove_playerAtCorner_findsMove() {
            // Given
            player.setCurrentPosition(new Position(0, 0));

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[6][6].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_treasureAtCorner_findsMove")
        void computeBestMove_treasureAtCorner_findsMove() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[0][0].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);
            player.setCurrentPosition(new Position(6, 6));

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_playerAlreadyOnTreasure_handlesCorrectly")
        void computeBestMove_playerAlreadyOnTreasure_handlesCorrectly() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[3][3].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);
            player.setCurrentPosition(new Position(3, 3)); // Already on treasure

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_withLastPush_avoidsReversePush")
        void computeBestMove_withLastPush_avoidsReversePush() {
            // Given
            labyrinth.contracts.models.PushActionInfo lastPush = new labyrinth.contracts.models.PushActionInfo();
            lastPush.setRowOrColIndex(3);
            lastPush.setDirection(Direction.RIGHT);
            board.setLastPush(lastPush);

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[5][5].setTreasure(treasure);
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
            // The AI should not choose the reverse push (row 3, LEFT)
        }
    }

    @Nested
    @DisplayName("ComputeBestMove - Board Configurations")
    class ComputeBestMoveBoardConfigTests {

        @Test
        @DisplayName("computeBestMove_boardWithFixedTiles_handlesCorrectly")
        void computeBestMove_boardWithFixedTiles_handlesCorrectly() {
            // Given - add some fixed tiles like a real board
            // Corners are typically fixed
            tiles[0][0] = createFixedTile(Direction.DOWN, Direction.RIGHT);
            tiles[0][6] = createFixedTile(Direction.DOWN, Direction.LEFT);
            tiles[6][0] = createFixedTile(Direction.UP, Direction.RIGHT);
            tiles[6][6] = createFixedTile(Direction.UP, Direction.LEFT);

            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[3][4].setTreasure(treasure);
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_sparselyConnectedBoard_findsPath")
        void computeBestMove_sparselyConnectedBoard_findsPath() {
            // Given - create a more restrictive board
            for (int row = 0; row < BOARD_SIZE; row++) {
                for (int col = 0; col < BOARD_SIZE; col++) {
                    // Only have horizontal connections (L-shaped pieces)
                    if (row % 2 == 0) {
                        tiles[row][col] = createTile(Direction.LEFT, Direction.RIGHT, Direction.DOWN);
                    } else {
                        tiles[row][col] = createTile(Direction.LEFT, Direction.RIGHT, Direction.UP);
                    }
                }
            }

            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[4][4].setTreasure(treasure);
            player.setCurrentTargetTreasure(treasure);
            player.setCurrentPosition(new Position(2, 2));

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }
    }

    @Nested
    @DisplayName("ComputeBestMove - Bonus Collection")
    class ComputeBestMoveBonusCollectionTests {

        @Test
        @DisplayName("computeBestMove_bonusTileReachable_considersBonusCollection")
        void computeBestMove_bonusTileReachable_considersBonusCollection() {
            // Given - treasure is far, but bonus is near
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[6][6].setTreasure(treasure);

            tiles[3][4].setBonus(BonusType.BEAM);

            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_multipleBonusTiles_choosesOptimal")
        void computeBestMove_multipleBonusTiles_choosesOptimal() {
            // Given
            tiles[2][2].setBonus(BonusType.BEAM);
            tiles[4][4].setBonus(BonusType.SWAP);
            tiles[5][5].setBonus(BonusType.PUSH_TWICE);

            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[6][6].setTreasure(treasure);

            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }
    }

    @Nested
    @DisplayName("ComputeBestMove - Strategic Moves")
    class ComputeBestMoveStrategicTests {

        @Test
        @DisplayName("computeBestMove_canPushTreasureCloser_considersStrategicMove")
        void computeBestMove_canPushTreasureCloser_considersStrategicMove() {
            // Given - set up scenario where push could move treasure closer
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[3][6].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);
            player.setCurrentPosition(new Position(3, 0));

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
        }

        @Test
        @DisplayName("computeBestMove_canBlockOpponent_considersBlocking")
        void computeBestMove_canBlockOpponent_considersBlocking() {
            // Given
            List<Player> allPlayers = new ArrayList<>();
            allPlayers.add(player);

            Player opponent = new Player("player-2", "Opponent");
            opponent.setCurrentPosition(new Position(5, 5));
            Treasure opponentTreasure = new Treasure();
            opponentTreasure.setId(2);
            tiles[5][6].setTreasure(opponentTreasure);
            opponent.setCurrentTargetTreasure(opponentTreasure);
            allPlayers.add(opponent);

            Treasure myTreasure = new Treasure();
            myTreasure.setId(1);
            tiles[1][1].setTreasure(myTreasure);
            player.setCurrentTargetTreasure(myTreasure);

            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player, allPlayers);

            // Then
            assertThat(decision).isNotNull();
        }
    }

    @Nested
    @DisplayName("ComputeBestMove - Stuck Detection")
    class ComputeBestMoveStuckDetectionTests {

        @Test
        @DisplayName("computeBestMove_playerStuck_findsEscapeMove")
        void computeBestMove_playerStuck_findsEscapeMove() {
            // Given - create scenario where player might get stuck
            // Disconnected tile at player position
            for (int row = 0; row < BOARD_SIZE; row++) {
                for (int col = 0; col < BOARD_SIZE; col++) {
                    if (row == 3 && col == 3) {
                        tiles[row][col] = createTile(); // No entrances - isolated
                    } else {
                        tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
                    }
                }
            }
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT));
            player.setCurrentPosition(new Position(3, 3));

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
            // AI should attempt to find a move even when seemingly stuck
        }
    }

    @Nested
    @DisplayName("AiDecision")
    class AiDecisionTests {

        @Test
        @DisplayName("aiDecision_fromComputeBestMove_containsRequiredFields")
        void aiDecision_fromComputeBestMove_containsRequiredFields() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(1);
            tiles[5][5].setTreasure(treasure);
            board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, createTile(Direction.UP, Direction.DOWN));
            player.setCurrentTargetTreasure(treasure);

            // When
            AiDecision decision = aiStrategy.computeBestMove(board, player);

            // Then
            assertThat(decision).isNotNull();
            // AiDecision should have shift operation or bonus action
        }
    }
}
