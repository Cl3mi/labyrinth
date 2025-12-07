package labyrinth.server.messaging.mapper;


import labyrinth.contracts.models.AchievementType;
import org.springframework.stereotype.Component;

@Component
public class AchievementTypeMapper {
    public AchievementType toDto(labyrinth.server.game.enums.Achievement type) {
        return AchievementType.fromValue(type.name());
    }
}
