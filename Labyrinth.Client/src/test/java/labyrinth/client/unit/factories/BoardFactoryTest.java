package labyrinth.client.unit.factories;

import labyrinth.client.factories.BoardFactory;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.contracts.models.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for BoardFactory.
 */
@DisplayName("BoardFactory")
class BoardFactoryTest {

    @Nested
    @DisplayName("fromContracts")
    class FromContractsTests {

        @Test
        @DisplayName("fromContracts_validGameBoard_returnsBoard")
        void fromContracts_validGameBoard_returnsBoard() {
            // Given
            GameBoard gameBoard = createValidGameBoard(7, 7);

            // When
            Board board = BoardFactory.fromContracts(gameBoard);

            // Then
            assertThat(board).isNotNull();
            assertThat(board.getWidth()).isEqualTo(7);
            assertThat(board.getHeight()).isEqualTo(7);
        }

        @Test
        @DisplayName("fromContracts_withSpareTile_setsSpareTile")
        void fromContracts_withSpareTile_setsSpareTile() {
            // Given
            GameBoard gameBoard = createValidGameBoard(7, 7);
            Tile spareTile = new Tile();
            spareTile.setEntrances(new Direction[]{Direction.UP, Direction.RIGHT});
            gameBoard.setSpareTile(spareTile);

            // When
            Board board = BoardFactory.fromContracts(gameBoard);

            // Then
            assertThat(board.getExtraTile()).isNotNull();
            assertThat(board.getExtraTile().getEntrances()).containsExactlyInAnyOrder(Direction.UP, Direction.RIGHT);
        }

        @Test
        @DisplayName("fromContracts_withLastPush_setsLastPush")
        void fromContracts_withLastPush_setsLastPush() {
            // Given
            GameBoard gameBoard = createValidGameBoard(7, 7);
            PushActionInfo lastPush = new PushActionInfo();
            lastPush.setRowOrColIndex(3);
            lastPush.setDirection(Direction.UP);
            gameBoard.setLastPush(lastPush);

            // When
            Board board = BoardFactory.fromContracts(gameBoard);

            // Then
            assertThat(board.getLastPush()).isNotNull();
            assertThat(board.getLastPush().getRowOrColIndex()).isEqualTo(3);
            assertThat(board.getLastPush().getDirection()).isEqualTo(Direction.UP);
        }

        @Test
        @DisplayName("fromContracts_nullGameBoard_throwsException")
        void fromContracts_nullGameBoard_throwsException() {
            // When/Then
            assertThatThrownBy(() -> BoardFactory.fromContracts(null))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("GameBoard is null");
        }

        @Test
        @DisplayName("fromContracts_zeroRows_throwsException")
        void fromContracts_zeroRows_throwsException() {
            // Given
            GameBoard gameBoard = new GameBoard();
            gameBoard.setRows(0);
            gameBoard.setCols(7);

            // When/Then
            assertThatThrownBy(() -> BoardFactory.fromContracts(gameBoard))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("rows/cols must be > 0");
        }

        @Test
        @DisplayName("fromContracts_zeroCols_throwsException")
        void fromContracts_zeroCols_throwsException() {
            // Given
            GameBoard gameBoard = new GameBoard();
            gameBoard.setRows(7);
            gameBoard.setCols(0);

            // When/Then
            assertThatThrownBy(() -> BoardFactory.fromContracts(gameBoard))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("rows/cols must be > 0");
        }

        @Test
        @DisplayName("fromContracts_nullTiles_throwsException")
        void fromContracts_nullTiles_throwsException() {
            // Given
            GameBoard gameBoard = new GameBoard();
            gameBoard.setRows(7);
            gameBoard.setCols(7);
            gameBoard.setTiles(null);

            // When/Then
            assertThatThrownBy(() -> BoardFactory.fromContracts(gameBoard))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("tiles is null");
        }

        @Test
        @DisplayName("fromContracts_rowDimensionMismatch_throwsException")
        void fromContracts_rowDimensionMismatch_throwsException() {
            // Given
            GameBoard gameBoard = new GameBoard();
            gameBoard.setRows(7);
            gameBoard.setCols(7);
            gameBoard.setTiles(new Tile[5][7]); // Only 5 rows instead of 7

            // When/Then
            assertThatThrownBy(() -> BoardFactory.fromContracts(gameBoard))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("row dimension mismatch");
        }

        @Test
        @DisplayName("fromContracts_colDimensionMismatch_throwsException")
        void fromContracts_colDimensionMismatch_throwsException() {
            // Given
            GameBoard gameBoard = new GameBoard();
            gameBoard.setRows(7);
            gameBoard.setCols(7);
            Tile[][] tiles = new Tile[7][5]; // Only 5 cols instead of 7
            for (int r = 0; r < 7; r++) {
                for (int c = 0; c < 5; c++) {
                    tiles[r][c] = createTile();
                }
            }
            gameBoard.setTiles(tiles);

            // When/Then
            assertThatThrownBy(() -> BoardFactory.fromContracts(gameBoard))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("col dimension mismatch");
        }

        @Test
        @DisplayName("fromContracts_nullRowInTiles_throwsException")
        void fromContracts_nullRowInTiles_throwsException() {
            // Given
            GameBoard gameBoard = new GameBoard();
            gameBoard.setRows(7);
            gameBoard.setCols(7);
            Tile[][] tiles = new Tile[7][];
            tiles[0] = null; // First row is null
            gameBoard.setTiles(tiles);

            // When/Then
            assertThatThrownBy(() -> BoardFactory.fromContracts(gameBoard))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("col dimension mismatch");
        }
    }

    @Nested
    @DisplayName("playersFromState")
    class PlayersFromStateTests {

        @Test
        @DisplayName("playersFromState_validStates_returnsPlayers")
        void playersFromState_validStates_returnsPlayers() {
            // Given
            PlayerState[] states = new PlayerState[2];
            states[0] = createPlayerState("player1", "Alice", PlayerColor.RED);
            states[1] = createPlayerState("player2", "Bob", PlayerColor.BLUE);

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players).hasSize(2);
            assertThat(players.get(0).getId()).isEqualTo("player1");
            assertThat(players.get(0).getName()).isEqualTo("Alice");
            assertThat(players.get(1).getId()).isEqualTo("player2");
            assertThat(players.get(1).getName()).isEqualTo("Bob");
        }

        @Test
        @DisplayName("playersFromState_withPositions_setsPositions")
        void playersFromState_withPositions_setsPositions() {
            // Given
            PlayerState state = createPlayerState("player1", "Alice", PlayerColor.RED);
            Coordinates currentPos = new Coordinates();
            currentPos.setRow(2);
            currentPos.setColumn(3);
            state.setCurrentPosition(currentPos);
            
            Coordinates homePos = new Coordinates();
            homePos.setRow(0);
            homePos.setColumn(0);
            state.setHomePosition(homePos);
            PlayerState[] states = {state};

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players.get(0).getCurrentPosition().getRow()).isEqualTo(2);
            assertThat(players.get(0).getCurrentPosition().getColumn()).isEqualTo(3);
            assertThat(players.get(0).getHomePosition().getRow()).isEqualTo(0);
            assertThat(players.get(0).getHomePosition().getColumn()).isEqualTo(0);
        }

        @Test
        @DisplayName("playersFromState_withFlags_setsFlags")
        void playersFromState_withFlags_setsFlags() {
            // Given
            PlayerState state = createPlayerState("player1", "Alice", PlayerColor.RED);
            state.getPlayerInfo().setIsConnected(true);
            state.getPlayerInfo().setIsAdmin(true);
            state.getPlayerInfo().setIsAiControlled(false);
            PlayerState[] states = {state};

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players.get(0).isConnected()).isTrue();
            assertThat(players.get(0).isAdmin()).isTrue();
            assertThat(players.get(0).isAiControlled()).isFalse();
        }

        @Test
        @DisplayName("playersFromState_withBonuses_setsBonuses")
        void playersFromState_withBonuses_setsBonuses() {
            // Given
            PlayerState state = createPlayerState("player1", "Alice", PlayerColor.RED);
            state.setAvailableBonuses(new BonusType[]{BonusType.BEAM, BonusType.SWAP});
            PlayerState[] states = {state};

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players.get(0).getAvailableBonuses()).containsExactly(BonusType.BEAM, BonusType.SWAP);
        }

        @Test
        @DisplayName("playersFromState_nullStates_returnsEmptyList")
        void playersFromState_nullStates_returnsEmptyList() {
            // When
            List<Player> players = BoardFactory.playersFromState(null);

            // Then
            assertThat(players).isEmpty();
        }

        @Test
        @DisplayName("playersFromState_nullStateInArray_skipsNull")
        void playersFromState_nullStateInArray_skipsNull() {
            // Given
            PlayerState[] states = new PlayerState[3];
            states[0] = createPlayerState("player1", "Alice", PlayerColor.RED);
            states[1] = null;
            states[2] = createPlayerState("player3", "Charlie", PlayerColor.GREEN);

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players).hasSize(2);
        }

        @Test
        @DisplayName("playersFromState_nullPlayerInfo_skipsState")
        void playersFromState_nullPlayerInfo_skipsState() {
            // Given
            PlayerState state = new PlayerState();
            state.setPlayerInfo(null);
            PlayerState[] states = {state};

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players).isEmpty();
        }

        @Test
        @DisplayName("playersFromState_withTreasuresFound_setsTreasures")
        void playersFromState_withTreasuresFound_setsTreasures() {
            // Given
            PlayerState state = createPlayerState("player1", "Alice", PlayerColor.RED);
            Treasure t1 = new Treasure();
            t1.setId(1);
            t1.setName("Ghost");
            Treasure t2 = new Treasure();
            t2.setId(2);
            t2.setName("Dragon");
            state.setTreasuresFound(new Treasure[]{t1, t2});
            PlayerState[] states = {state};

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players.get(0).getTreasuresFound()).hasSize(2);
        }

        @Test
        @DisplayName("playersFromState_withRemainingTreasureCount_setsCount")
        void playersFromState_withRemainingTreasureCount_setsCount() {
            // Given
            PlayerState state = createPlayerState("player1", "Alice", PlayerColor.RED);
            state.setRemainingTreasureCount(5);
            PlayerState[] states = {state};

            // When
            List<Player> players = BoardFactory.playersFromState(states);

            // Then
            assertThat(players.get(0).getRemainingTreasureCount()).isEqualTo(5);
        }
    }

    @Nested
    @DisplayName("convertPlayerStates")
    class ConvertPlayerStatesTests {

        @Test
        @DisplayName("convertPlayerStates_delegatesToPlayersFromState")
        void convertPlayerStates_delegatesToPlayersFromState() {
            // Given
            PlayerState[] states = new PlayerState[1];
            states[0] = createPlayerState("player1", "Alice", PlayerColor.RED);

            // When
            List<Player> players = BoardFactory.convertPlayerStates(states);

            // Then
            assertThat(players).hasSize(1);
            assertThat(players.get(0).getId()).isEqualTo("player1");
        }
    }

    @Nested
    @DisplayName("applyTurnInfo")
    class ApplyTurnInfoTests {

        @Test
        @DisplayName("applyTurnInfo_waitingForPush_setsPlaceTileState")
        void applyTurnInfo_waitingForPush_setsPlaceTileState() {
            // Given
            Board board = createSimpleBoard();
            List<Player> players = List.of(new Player("p1", "Alice"));
            CurrentTurnInfo turnInfo = new CurrentTurnInfo();
            turnInfo.setState(TurnState.WAITING_FOR_PUSH);
            turnInfo.setCurrentPlayerId("p1");

            // When
            BoardFactory.applyTurnInfo(board, players, turnInfo);

            // Then
            assertThat(board.getCurrentMoveState()).isEqualTo(labyrinth.client.enums.MoveState.PLACE_TILE);
        }

        @Test
        @DisplayName("applyTurnInfo_waitingForMove_setsMoveState")
        void applyTurnInfo_waitingForMove_setsMoveState() {
            // Given
            Board board = createSimpleBoard();
            List<Player> players = List.of(new Player("p1", "Alice"));
            CurrentTurnInfo turnInfo = new CurrentTurnInfo();
            turnInfo.setState(TurnState.WAITING_FOR_MOVE);
            turnInfo.setCurrentPlayerId("p1");

            // When
            BoardFactory.applyTurnInfo(board, players, turnInfo);

            // Then
            assertThat(board.getCurrentMoveState()).isEqualTo(labyrinth.client.enums.MoveState.MOVE);
        }

        @Test
        @DisplayName("applyTurnInfo_withCurrentPlayer_setsPlayerIndex")
        void applyTurnInfo_withCurrentPlayer_setsPlayerIndex() {
            // Given
            Board board = createSimpleBoard();
            List<Player> players = List.of(
                    new Player("p1", "Alice"),
                    new Player("p2", "Bob"),
                    new Player("p3", "Charlie")
            );
            CurrentTurnInfo turnInfo = new CurrentTurnInfo();
            turnInfo.setState(TurnState.WAITING_FOR_PUSH);
            turnInfo.setCurrentPlayerId("p2");

            // When
            BoardFactory.applyTurnInfo(board, players, turnInfo);

            // Then
            assertThat(board.getCurrentPlayerIndex()).isEqualTo(1);
        }

        @Test
        @DisplayName("applyTurnInfo_nullBoard_doesNothing")
        void applyTurnInfo_nullBoard_doesNothing() {
            // Given
            List<Player> players = List.of(new Player("p1", "Alice"));
            CurrentTurnInfo turnInfo = new CurrentTurnInfo();
            turnInfo.setState(TurnState.WAITING_FOR_PUSH);

            // When/Then - should not throw
            assertThatCode(() -> BoardFactory.applyTurnInfo(null, players, turnInfo))
                    .doesNotThrowAnyException();
        }

        @Test
        @DisplayName("applyTurnInfo_nullTurnInfo_doesNothing")
        void applyTurnInfo_nullTurnInfo_doesNothing() {
            // Given
            Board board = createSimpleBoard();
            List<Player> players = List.of(new Player("p1", "Alice"));

            // When/Then - should not throw
            assertThatCode(() -> BoardFactory.applyTurnInfo(board, players, null))
                    .doesNotThrowAnyException();
        }
    }

    // Helper methods

    private GameBoard createValidGameBoard(int rows, int cols) {
        GameBoard gameBoard = new GameBoard();
        gameBoard.setRows(rows);
        gameBoard.setCols(cols);

        Tile[][] tiles = new Tile[rows][cols];
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                tiles[r][c] = createTile();
            }
        }
        gameBoard.setTiles(tiles);
        return gameBoard;
    }

    private Tile createTile() {
        Tile tile = new Tile();
        tile.setEntrances(new Direction[]{Direction.UP, Direction.DOWN});
        return tile;
    }

    private PlayerState createPlayerState(String id, String name, PlayerColor color) {
        PlayerState state = new PlayerState();
        PlayerInfo info = new PlayerInfo();
        info.setId(id);
        info.setName(name);
        info.setColor(color);
        state.setPlayerInfo(info);
        return state;
    }

    private Board createSimpleBoard() {
        GameBoard gameBoard = createValidGameBoard(7, 7);
        return BoardFactory.fromContracts(gameBoard);
    }
}
