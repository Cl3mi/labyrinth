package labyrinth.client.factories;

import labyrinth.client.abstractions.IBoardFactory;
import labyrinth.client.models.Board;
import labyrinth.client.models.Game;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.Coordinates;
import labyrinth.contracts.models.GameBoard;
import labyrinth.contracts.models.PlayerState;
import labyrinth.contracts.models.Tile;

import java.util.ArrayList;
import java.util.List;

/**
 * Factory / Mapper für Boards und Spielerzustände vom Server.
 *
 * - Board kommt vollständig vom Server (GameBoard)
 * - spareTile (ExtraTile) wird aus GameBoard übernommen
 */
public class BoardFactory implements IBoardFactory {

    @Override
    public Board createBoardForGame(Game game) {
        throw new UnsupportedOperationException(
                "BoardFactory.createBoardForGame(Game) wird nicht mehr verwendet. " +
                        "Das Board kommt vollständig vom Server (GameBoard)."
        );
    }

    // =================================================================================
    // Mapping: GameBoard (Contracts) -> Board (Client)
    // =================================================================================

    public static Board fromContracts(GameBoard gb) {
        if (gb == null) {
            throw new IllegalArgumentException("GameBoard is null");
        }

        int rows = gb.getRows() != null ? gb.getRows() : 0;
        int cols = gb.getCols() != null ? gb.getCols() : 0;

        if (rows <= 0 || cols <= 0) {
            throw new IllegalArgumentException("GameBoard.rows/cols must be > 0");
        }

        Tile[][] tiles = gb.getTiles();
        if (tiles == null) {
            throw new IllegalArgumentException("GameBoard.tiles is null");
        }

        // Robust dimension check
        if (tiles.length != rows) {
            throw new IllegalArgumentException(
                    "GameBoard.tiles row dimension mismatch: rows=" + rows + " tiles.length=" + tiles.length
            );
        }
        for (int r = 0; r < rows; r++) {
            if (tiles[r] == null || tiles[r].length != cols) {
                throw new IllegalArgumentException(
                        "GameBoard.tiles col dimension mismatch at row " + r +
                                ": cols=" + cols + " tiles[r].length=" + (tiles[r] == null ? -1 : tiles[r].length)
                );
            }
        }

        //spareTile direkt übernehmen (Server liefert sie im GameBoard)
        Tile extraTile = gb.getSpareTile();

        // Board-Konstruktor: (width=cols, height=rows, tiles, extraTile)
        return new Board(cols, rows, tiles, extraTile);
    }

    // =================================================================================
    // Mapping: PlayerState[] (Contracts) -> List<Player> (Client)
    // =================================================================================

    public static List<Player> playersFromState(PlayerState[] states) {
        List<Player> list = new ArrayList<>();
        if (states == null) return list;

        for (PlayerState s : states) {
            if (s == null || s.getPlayerInfo() == null) continue;

            String id = s.getPlayerInfo().getId();
            String name = s.getPlayerInfo().getName();

            Player p = new Player(id, name);

            // Position
            Coordinates currentPos = s.getCurrentPosition();
            if (currentPos != null) {
                p.setCurrentPosition(new Position(currentPos.getRow(), currentPos.getColumn()));
            }

            Coordinates homePos = s.getHomePosition();
            if (homePos != null) {
                p.setHomePosition(new Position(homePos.getRow(), homePos.getColumn()));
            }

            // Player info fields
            if (s.getPlayerInfo().getColor() != null) {
                p.setColor(s.getPlayerInfo().getColor());
            }

            Boolean isConnected = s.getPlayerInfo().getIsConnected();
            p.setConnected(isConnected != null && isConnected);

            Boolean isAdmin = s.getPlayerInfo().getIsAdmin();
            p.setAdmin(isAdmin != null && isAdmin);

            Boolean isAiControlled = s.getPlayerInfo().getIsAiControlled();
            p.setAiControlled(isAiControlled != null && isAiControlled);

            // Treasures found
            if (s.getTreasuresFound() != null) {
                p.getTreasuresFound().clear();
                for (labyrinth.contracts.models.Treasure treasure : s.getTreasuresFound()) {
                    if (treasure != null) {
                        p.getTreasuresFound().add(treasure);
                    }
                }
            }

            // Remaining treasure count
            if (s.getRemainingTreasureCount() != null) {
                p.setRemainingTreasureCount(s.getRemainingTreasureCount());
            }

            // Current treasure card (the one they need to find)
            if (s.getCurrentTreasureCard() != null) {
                p.getAssignedTreasureCards().clear();
                p.getAssignedTreasureCards().add(s.getCurrentTreasureCard());
            }

            list.add(p);
        }

        return list;
    }

    public static List<Player> convertPlayerStates(PlayerState[] states) {
        return playersFromState(states);
    }

    // =================================================================================
    // Extra-Tile anwenden (falls du es separat updaten willst)
    // =================================================================================

    public static void applyExtraTile(Board board, Tile extraTile) {
        if (board == null) return;
        board.setExtraTile(extraTile);
    }

    // =================================================================================
    // Turn info mapping: CurrentTurnInfo (Contracts) -> Board fields (Client)
    // =================================================================================

    public static void applyTurnInfo(Board board, List<Player> players,
                                      labyrinth.contracts.models.CurrentTurnInfo turnInfo) {
        if (board == null || turnInfo == null) return;

        // Map TurnState to MoveState
        if (turnInfo.getState() != null) {
            labyrinth.client.enums.MoveState moveState = switch (turnInfo.getState()) {
                case WAITING_FOR_PUSH -> labyrinth.client.enums.MoveState.PLACE_TILE;
                case WAITING_FOR_MOVE -> labyrinth.client.enums.MoveState.MOVE;
            };
            board.setCurrentMoveState(moveState);
        }

        // Find current player index by ID
        if (turnInfo.getCurrentPlayerId() != null && players != null) {
            for (int i = 0; i < players.size(); i++) {
                if (players.get(i).getId().equals(turnInfo.getCurrentPlayerId())) {
                    board.setCurrentPlayerIndex(i);
                    break;
                }
            }
        }
    }
}
