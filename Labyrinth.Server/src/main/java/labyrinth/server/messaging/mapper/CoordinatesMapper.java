package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Coordinates;
import labyrinth.server.game.models.Position;
import org.springframework.stereotype.Component;

@Component
public class CoordinatesMapper {
    public Coordinates toDto(Position position) {
        var coordinates = new Coordinates();
        coordinates.setX(position.getColumn());
        coordinates.setY(position.getRow());
        return coordinates;
    }
}
