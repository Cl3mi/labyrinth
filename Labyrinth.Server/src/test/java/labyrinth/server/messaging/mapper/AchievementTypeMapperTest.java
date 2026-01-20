package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.AchievementType;
import labyrinth.server.game.enums.Achievement;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AchievementTypeMapperTest {
    private final AchievementTypeMapper mapper = new AchievementTypeMapper();

    @Test
    void mapsAchievement() {
        var dto = mapper.toDto(Achievement.PUSHER);
        assertEquals(AchievementType.fromValue("PUSHER"), dto);
    }
}
