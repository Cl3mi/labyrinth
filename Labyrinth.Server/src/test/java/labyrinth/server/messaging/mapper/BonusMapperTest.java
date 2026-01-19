package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.BonusType;
import labyrinth.server.game.enums.BonusTypes;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BonusMapperTest {
    private final BonusMapper mapper = new BonusMapper();

    @Test
    void mapsBonuses() {
        assertEquals(BonusType.SWAP, mapper.toDto(BonusTypes.SWAP));
        assertEquals(BonusType.BEAM, mapper.toDto(BonusTypes.BEAM));
        assertEquals(BonusType.PUSH_TWICE, mapper.toDto(BonusTypes.PUSH_TWICE));
        assertEquals(BonusType.PUSH_FIXED, mapper.toDto(BonusTypes.PUSH_FIXED));
    }
}
