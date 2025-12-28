package labyrinth.server.game.services;

import labyrinth.server.game.enums.Achievement;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.PlayerStatistics;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

class AchievementServiceTest {

    private AchievementService achievementService;
    private Player player;
    private PlayerStatistics statistics;

    @BeforeEach
    void setUp() {
        achievementService = new AchievementService();
        player = new Player(UUID.randomUUID(), "TestPlayer");
        statistics = player.getStatistics();
    }

    // PUSHER Achievement Tests

    @Test
    void checkPusherAchievement_shouldAwardAchievement_whenPlayerPushes20Tiles() {
        // Arrange
        statistics.increaseTilesPushed(20);

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert
        assertTrue(result.isPresent());
        assertEquals(Achievement.PUSHER, result.get());
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.PUSHER));
    }

    @Test
    void checkPusherAchievement_shouldAwardAchievement_whenPlayerPushesMoreThan20Tiles() {
        // Arrange
        statistics.increaseTilesPushed(25);

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert
        assertTrue(result.isPresent());
        assertEquals(Achievement.PUSHER, result.get());
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.PUSHER));
    }

    @Test
    void checkPusherAchievement_shouldNotAwardAchievement_whenPlayerPushesLessThan20Tiles() {
        // Arrange
        statistics.increaseTilesPushed(19);

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert
        assertFalse(result.isPresent());
        assertFalse(statistics.getCollectedAchievements().contains(Achievement.PUSHER));
    }

    @Test
    void checkPusherAchievement_shouldNotAwardAgain_whenPlayerAlreadyHasAchievement() {
        // Arrange
        statistics.increaseTilesPushed(20);
        achievementService.checkPusherAchievement(player); // First award
        statistics.increaseTilesPushed(10); // Push more tiles

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert
        assertFalse(result.isPresent());
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.PUSHER));
    }

    @Test
    void checkPusherAchievement_shouldNotAwardAchievement_whenPlayerHasZeroPushes() {
        // Arrange - player has 0 pushes by default

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert
        assertFalse(result.isPresent());
        assertFalse(statistics.getCollectedAchievements().contains(Achievement.PUSHER));
    }

    // RUNNER Achievement Tests

    @Test
    void checkRunnerAchievement_shouldAwardAchievement_whenPlayerTakes200Steps() {
        // Arrange
        statistics.increaseStepsTaken(200);

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert
        assertTrue(result.isPresent());
        assertEquals(Achievement.RUNNER, result.get());
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.RUNNER));
    }

    @Test
    void checkRunnerAchievement_shouldAwardAchievement_whenPlayerTakesMoreThan200Steps() {
        // Arrange
        statistics.increaseStepsTaken(300);

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert
        assertTrue(result.isPresent());
        assertEquals(Achievement.RUNNER, result.get());
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.RUNNER));
    }

    @Test
    void checkRunnerAchievement_shouldNotAwardAchievement_whenPlayerTakesLessThan200Steps() {
        // Arrange
        statistics.increaseStepsTaken(199);

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert
        assertFalse(result.isPresent());
        assertFalse(statistics.getCollectedAchievements().contains(Achievement.RUNNER));
    }

    @Test
    void checkRunnerAchievement_shouldNotAwardAgain_whenPlayerAlreadyHasAchievement() {
        // Arrange
        statistics.increaseStepsTaken(200);
        achievementService.checkRunnerAchievement(player); // First award
        statistics.increaseStepsTaken(100); // Take more steps

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert
        assertFalse(result.isPresent());
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.RUNNER));
    }

    @Test
    void checkRunnerAchievement_shouldNotAwardAchievement_whenPlayerHasZeroSteps() {
        // Arrange - player has 0 steps by default

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert
        assertFalse(result.isPresent());
        assertFalse(statistics.getCollectedAchievements().contains(Achievement.RUNNER));
    }

    // Edge Case Tests

    @Test
    void multiplePlayers_shouldTrackAchievementsIndependently() {
        // Arrange
        Player player1 = new Player(UUID.randomUUID(), "Player1");
        Player player2 = new Player(UUID.randomUUID(), "Player2");

        player1.getStatistics().increaseTilesPushed(20);
        player2.getStatistics().increaseTilesPushed(10);

        // Act
        Optional<Achievement> result1 = achievementService.checkPusherAchievement(player1);
        Optional<Achievement> result2 = achievementService.checkPusherAchievement(player2);

        // Assert
        assertTrue(result1.isPresent());
        assertFalse(result2.isPresent());
        assertTrue(player1.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
        assertFalse(player2.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
    }

    @Test
    void playerCanEarnBothAchievements() {
        // Arrange
        statistics.increaseTilesPushed(20);
        statistics.increaseStepsTaken(200);

        // Act
        Optional<Achievement> pusherResult = achievementService.checkPusherAchievement(player);
        Optional<Achievement> runnerResult = achievementService.checkRunnerAchievement(player);

        // Assert
        assertTrue(pusherResult.isPresent());
        assertTrue(runnerResult.isPresent());
        assertEquals(2, statistics.getCollectedAchievements().size());
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.PUSHER));
        assertTrue(statistics.getCollectedAchievements().contains(Achievement.RUNNER));
    }
}
