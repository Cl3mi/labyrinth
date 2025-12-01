package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Treasure;
import labyrinth.server.game.models.TreasureCard;
import org.springframework.stereotype.Component;

@Component
public class TreasureMapper {

    public Treasure toDto(TreasureCard treasureCard) {
        Treasure dto = new Treasure();
        dto.setName(treasureCard.getTreasureName());
        dto.setId(treasureCard.getId());

        return dto;
    }
}
