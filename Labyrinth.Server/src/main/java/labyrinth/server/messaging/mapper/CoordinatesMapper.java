package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.Coordinates;
import labyrinth.server.game.models.records.Position;
import org.springframework.stereotype.Component;

@Component
public class CoordinatesMapper {
    public Coordinates toDto(Position position) {
        var coordinates = new Coordinates();
        coordinates.setRow(position.row());
        coordinates.setColumn(position.column());
        return coordinates;
    }
}
