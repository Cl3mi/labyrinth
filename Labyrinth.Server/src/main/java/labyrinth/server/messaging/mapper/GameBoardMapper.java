package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.GameBoard;
import labyrinth.contracts.models.Tile;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Position;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class GameBoardMapper {

    private final DirectionMapper directionMapper;
    private final TreasureMapper treasureMapper;

    public GameBoard toDto(Board board) {
        GameBoard dto = new GameBoard();

        int maxRow = 0;
        int maxCol = 0;

        var entries = board.getTileMap().entrySet();

        for (var entry : entries) {
            Position pos = entry.getKey();
            if (pos.getRow() > maxRow) maxRow = pos.getRow();
            if (pos.getColumn() > maxCol) maxCol = pos.getColumn();
        }

        Tile[][] tiles = new Tile[maxRow + 1][maxCol + 1];

        for (var entry : board.getTileMap().entrySet()) {

            Position pos = entry.getKey();
            labyrinth.server.game.models.Tile tile = entry.getValue();

            var tileDto = new labyrinth.contracts.models.Tile();
            tileDto.setIsFixed(tile.isFixed());
            tileDto.setTreasure(treasureMapper.toDto(tile.getTreasureCard()));
            //tileDto.setBonus(tile.getBonus); TODO add bonus when available

            tileDto.setEntrances(
                    tile.getEntrances().stream()
                            .map(directionMapper::toDto)
                            .toArray(Direction[]::new)
            );

            tiles[pos.getRow()][pos.getColumn()] = tileDto;
        }

        dto.setTiles(tiles);
        dto.setRows(maxRow);
        dto.setCols(maxCol);

        return dto;
    }
}
