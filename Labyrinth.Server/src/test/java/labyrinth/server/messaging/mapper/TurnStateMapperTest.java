package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.TurnState;
import labyrinth.server.game.enums.MoveState;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TurnStateMapperTest {
    private final TurnStateMapper mapper = new TurnStateMapper();

    @Test
    void mapsPlaceTileToWaitingForPush() {
        assertEquals(TurnState.WAITING_FOR_PUSH, mapper.toDto(MoveState.PLACE_TILE));
    }

    @Test
    void mapsMoveToWaitingForMove() {
        assertEquals(TurnState.WAITING_FOR_MOVE, mapper.toDto(MoveState.MOVE));
    }
}
