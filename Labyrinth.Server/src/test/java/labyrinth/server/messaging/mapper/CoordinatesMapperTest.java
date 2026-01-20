package labyrinth.server.messaging.mapper;


import labyrinth.server.game.models.records.Position;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CoordinatesMapperTest {
    private final CoordinatesMapper mapper = new CoordinatesMapper();

    @Test
    void mapsPositionToCoordinates() {
        var pos = new Position(3, 5);
        var dto = mapper.toDto(pos);
        assertEquals(3, dto.getRow());
        assertEquals(5, dto.getColumn());
    }
}
