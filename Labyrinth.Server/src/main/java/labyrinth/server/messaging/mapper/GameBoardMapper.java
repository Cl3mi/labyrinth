package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.GameBoard;
import labyrinth.contracts.models.Tile;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.records.Position;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class GameBoardMapper {

    private final TileMapper tileMapper;

    public GameBoard toDto(Board board) {
        GameBoard dto = new GameBoard();

        int maxRow = 0;
        int maxCol = 0;

        var entries = board.getTileMap().entrySet();

        for (var entry : entries) {
            Position pos = entry.getKey();
            if (pos.row() > maxRow) maxRow = pos.row();
            if (pos.column() > maxCol) maxCol = pos.column();
        }

        Tile[][] tiles = new Tile[maxRow + 1][maxCol + 1];

        for (var entry : board.getTileMap().entrySet()) {
            Position pos = entry.getKey();
            tiles[pos.row()][pos.column()] = tileMapper.toDto(entry.getValue());
        }

        dto.setTiles(tiles);
        dto.setSpareTile(tileMapper.toDto(board.getExtraTile()));
        dto.setRows(maxRow + 1);
        dto.setCols(maxCol + 1);

        return dto;
    }
}
