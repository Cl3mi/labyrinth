package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Direction;
import org.springframework.stereotype.Component;


@Component
public class DirectionMapper {
    public Direction toDto(labyrinth.server.game.enums.Direction direction) {
        return switch (direction) {
            case UP -> Direction.UP;
            case DOWN -> Direction.DOWN;
            case LEFT -> Direction.LEFT;
            case RIGHT -> Direction.RIGHT;
        };
    }
}
