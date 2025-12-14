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
 * Wichtig:
 * - Das Board wird vollständig vom Server geliefert (GameBoard).
 * - Diese Factory generiert KEINE zufälligen lokalen Boards mehr.
 */
public class BoardFactory implements IBoardFactory {

    /**
     * Alte Debug-Methode. Im echten Spiel nicht mehr nutzen.
     * Wenn du sie noch irgendwo aufrufst, siehst du es sofort durch die Exception.
     */
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

    /**
     * Baut aus einem vom Server gelieferten GameBoard ein Client-Board.
     * Verwendet direkt die Contracts-Tiles.
     *
     * @param gb Server-Board
     * @return Client-Board
     */
    public static Board fromContracts(GameBoard gb) {
        if (gb == null)
            throw new IllegalArgumentException("GameBoard is null");

        int rows = gb.getRows() != null ? gb.getRows() : 0;
        int cols = gb.getCols() != null ? gb.getCols() : 0;

        if (rows <= 0 || cols <= 0)
            throw new IllegalArgumentException("GameBoard.rows/cols must be > 0");

        Tile[][] tiles = gb.getTiles();
        if (tiles == null)
            throw new IllegalArgumentException("GameBoard.tiles is null");

        if (tiles.length != rows || tiles[0].length != cols) {
            throw new IllegalArgumentException(
                    "GameBoard.tiles dimensions do not match rows/cols: " +
                            "rows=" + rows + ", cols=" + cols +
                            ", tiles.length=" + tiles.length +
                            ", tiles[0].length=" + (tiles.length > 0 ? tiles[0].length : -1)
            );
        }

        // Extra-Tile kommt im PlayerTurnEventPayload (extraTile), nicht im GameBoard.
        // Wir setzen sie hier auf null; sie wird später durch PlayerTurnEvent aktualisiert.
        Tile extraTile = null;

        // Board-Konstruktor: (width, height, tiles, extraTile)
        // width = cols (Spalten), height = rows (Zeilen)
        return new Board(cols, rows, tiles, extraTile);
    }

    // =================================================================================
    // Mapping: PlayerState[] (Contracts) -> List<Player> (Client)
    // =================================================================================

    /**
     * Basis-Mapping von PlayerState[] → Client-Player.
     */
    public static List<Player> playersFromState(PlayerState[] states) {
        List<Player> list = new ArrayList<>();
        if (states == null) return list;

        for (PlayerState s : states) {
            if (s == null) continue;

            String id = s.getPlayerInfo().getId();
            String name = s.getPlayerInfo().getName();
            Player p = new Player(id, name);

            // aktuelle Position
            Coordinates currentPos = s.getCurrentPosition();
            if (currentPos != null) {
                p.setCurrentPosition(new Position(currentPos.getRow(), currentPos.getColumn()));
            }

            // Heimatposition
            Coordinates homePos = s.getHomePosition();
            if (homePos != null) {
                p.setHomePosition(new Position(homePos.getRow(), homePos.getColumn()));
            }

            // hier könntest du später noch Farbe, Achievements, Boni etc. mappen

            list.add(p);
        }

        return list;
    }

    /**
     * Alias, damit Aufrufer den alten Namen verwenden können.
     * Wird in LabyrinthApplication genutzt.
     */
    public static List<Player> convertPlayerStates(PlayerState[] states) {
        return playersFromState(states);
    }

    // =================================================================================
    // Extra-Tile aus PlayerTurnEventPayload anwenden
    // =================================================================================

    public static void applyExtraTile(Board board, Tile extraTile) {
        if (board == null) return;
        board.setExtraTile(extraTile);
    }
}
