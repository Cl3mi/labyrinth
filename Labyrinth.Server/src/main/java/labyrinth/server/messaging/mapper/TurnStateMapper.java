package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.TurnState;
import labyrinth.server.game.enums.MoveState;
import org.springframework.stereotype.Component;

@Component
public class TurnStateMapper {
    public TurnState toDto(MoveState moveState) {
        return switch (moveState) {
            case PLACE_TILE -> TurnState.WAITING_FOR_PUSH;
            case MOVE -> TurnState.WAITING_FOR_MOVE;
            //TODO what about wait for bonus?
        };
    }
}
