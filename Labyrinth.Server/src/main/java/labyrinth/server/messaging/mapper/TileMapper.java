package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TileMapper {
    private final TreasureMapper treasureMapper;
    private final DirectionMapper directionMapper;
    private final BonusMapper bonusMapper;

    public Tile toDto(labyrinth.server.game.models.Tile tile) {
        var tileDto = new labyrinth.contracts.models.Tile();
        tileDto.setIsFixed(tile.isFixed());
        if (tile.getTreasureCard() != null) {
            tileDto.setTreasure(treasureMapper.toDto(tile.getTreasureCard()));
        } else {
            tileDto.setTreasure(null);
        }

        var bonus = tile.getBonus();
        if(bonus != null) {
            tileDto.setBonus(bonusMapper.toDto(bonus));
        }

        tileDto.setEntrances(
                tile.getEntrances().stream()
                        .map(directionMapper::toDto)
                        .toArray(Direction[]::new)
        );

        return tileDto;
    }
}
