package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Direction;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DirectionMapperTest {
    private final DirectionMapper mapper = new DirectionMapper();

    @Test
    void mapsDirectionToDtoAndBack() {
        var model = labyrinth.server.game.enums.Direction.UP;
        var dto = mapper.toDto(model);
        assertEquals(Direction.UP, dto);
        var back = mapper.toModel(dto);
        assertEquals(model, back);
    }
}
