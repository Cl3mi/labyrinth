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

    public labyrinth.server.game.enums.Direction toModel(Direction direction) {
        return switch (direction) {
            case UP -> labyrinth.server.game.enums.Direction.UP;
            case DOWN -> labyrinth.server.game.enums.Direction.DOWN;
            case LEFT -> labyrinth.server.game.enums.Direction.LEFT;
            case RIGHT -> labyrinth.server.game.enums.Direction.RIGHT;
        };
    }
}
