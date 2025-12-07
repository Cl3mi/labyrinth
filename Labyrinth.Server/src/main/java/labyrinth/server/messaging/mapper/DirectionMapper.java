package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Direction;
import org.springframework.stereotype.Component;


@Component
public class DirectionMapper {
    public Direction toDto(labyrinth.server.game.enums.Direction direction) {
        return Direction.fromValue(direction.name());
    }

    public labyrinth.server.game.enums.Direction toModel(Direction direction) {
        return labyrinth.server.game.enums.Direction.valueOf(direction.name());
    }
}
