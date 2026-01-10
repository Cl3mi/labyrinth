package labyrinth.server.game.services;

import labyrinth.server.game.enums.Achievement;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.PlayerStatistics;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for deprecated threshold-based achievement methods.
 * Note: These methods are deprecated. For current behavior, see EndGameAchievementTest.
 */
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

    // PUSHER Achievement Tests - Testing DEPRECATED methods

    @Test
    void checkPusherAchievement_shouldReturnEmpty_asMethodIsDeprecated() {
        // Arrange
        statistics.increaseTilesPushed(20);

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert - Deprecated method returns empty
        assertFalse(result.isPresent());
    }

    @Test
    void checkPusherAchievement_shouldReturnEmpty_evenWithMoreThan20Tiles() {
        // Arrange
        statistics.increaseTilesPushed(25);

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert - Deprecated method returns empty
        assertFalse(result.isPresent());
    }

    @Test
    void checkPusherAchievement_shouldReturnEmpty_whenPlayerPushesLessThan20Tiles() {
        // Arrange
        statistics.increaseTilesPushed(19);

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert
        assertFalse(result.isPresent());
    }

    @Test
    void checkPusherAchievement_shouldReturnEmpty_whenPlayerAlreadyHasAchievement() {
        // Arrange
        statistics.increaseTilesPushed(20);
        statistics.collectAchievement(Achievement.PUSHER);

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert - Deprecated method returns empty
        assertFalse(result.isPresent());
    }

    @Test
    void checkPusherAchievement_shouldReturnEmpty_whenPlayerHasZeroPushes() {
        // Arrange - player has 0 pushes by default

        // Act
        Optional<Achievement> result = achievementService.checkPusherAchievement(player);

        // Assert
        assertFalse(result.isPresent());
    }

    // RUNNER Achievement Tests - Testing DEPRECATED methods

    @Test
    void checkRunnerAchievement_shouldReturnEmpty_asMethodIsDeprecated() {
        // Arrange
        statistics.increaseStepsTaken(200);

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert - Deprecated method returns empty
        assertFalse(result.isPresent());
    }

    @Test
    void checkRunnerAchievement_shouldReturnEmpty_evenWithMoreThan200Steps() {
        // Arrange
        statistics.increaseStepsTaken(300);

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert - Deprecated method returns empty
        assertFalse(result.isPresent());
    }

    @Test
    void checkRunnerAchievement_shouldReturnEmpty_whenPlayerTakesLessThan200Steps() {
        // Arrange
        statistics.increaseStepsTaken(199);

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert
        assertFalse(result.isPresent());
    }

    @Test
    void checkRunnerAchievement_shouldReturnEmpty_whenPlayerAlreadyHasAchievement() {
        // Arrange
        statistics.increaseStepsTaken(200);
        statistics.collectAchievement(Achievement.RUNNER);

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert - Deprecated method returns empty
        assertFalse(result.isPresent());
    }

    @Test
    void checkRunnerAchievement_shouldReturnEmpty_whenPlayerHasZeroSteps() {
        // Arrange - player has 0 steps by default

        // Act
        Optional<Achievement> result = achievementService.checkRunnerAchievement(player);

        // Assert
        assertFalse(result.isPresent());
    }

    // Edge Case Tests - These now test the NEW achievement system

    @Test
    void multiplePlayers_shouldTrackAchievementsIndependently() {
        // Arrange
        Player player1 = new Player(UUID.randomUUID(), "Player1");
        Player player2 = new Player(UUID.randomUUID(), "Player2");

        player1.getStatistics().increaseTilesPushed(20);
        player2.getStatistics().increaseTilesPushed(10);

        // Act - Using new end-game achievement system
        var awards = achievementService.awardEndGameAchievements(List.of(player1, player2));

        // Assert - Player1 has most pushes, should get PUSHER
        assertEquals(1, awards.size());
        assertEquals(Achievement.PUSHER, awards.get(0).achievement());
        assertEquals(player1, awards.get(0).player());
        assertTrue(player1.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
        assertFalse(player2.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
    }

    @Test
    void playerCanEarnBothAchievements() {
        // Arrange
        Player player1 = new Player(UUID.randomUUID(), "Player1");
        Player player2 = new Player(UUID.randomUUID(), "Player2");

        player1.getStatistics().increaseTilesPushed(20);
        player1.getStatistics().increaseStepsTaken(200);
        player2.getStatistics().increaseTilesPushed(10);
        player2.getStatistics().increaseStepsTaken(100);

        // Act - Using new end-game achievement system
        var awards = achievementService.awardEndGameAchievements(List.of(player1, player2));

        // Assert - Player1 has most pushes AND most steps, should get both
        assertEquals(2, awards.size());
        assertEquals(2, player1.getStatistics().getCollectedAchievements().size());
        assertTrue(player1.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
        assertTrue(player1.getStatistics().getCollectedAchievements().contains(Achievement.RUNNER));
    }
}
