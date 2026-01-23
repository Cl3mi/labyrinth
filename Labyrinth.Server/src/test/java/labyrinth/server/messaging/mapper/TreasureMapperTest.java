package labyrinth.server.messaging.mapper;


import labyrinth.server.game.models.TreasureCard;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TreasureMapperTest {
    private final TreasureMapper mapper = new TreasureMapper();

    @Test
    void mapsTreasureCard() {
        var card = new TreasureCard(1, "Gold");
        var dto = mapper.toDto(card);
        assertEquals("Gold", dto.getName());
        assertEquals(1, dto.getId());
    }
}
