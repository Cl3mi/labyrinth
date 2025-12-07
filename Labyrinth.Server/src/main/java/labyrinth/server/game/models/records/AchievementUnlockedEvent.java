package labyrinth.server.game.models.records;

import labyrinth.server.game.models.Player;

public record AchievementUnlockedEvent(
        Player player,
        String achievementId
) {}
