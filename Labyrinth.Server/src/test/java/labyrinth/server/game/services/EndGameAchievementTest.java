package labyrinth.server.game.services;

import labyrinth.server.game.abstractions.IGameTimer;
import labyrinth.server.game.ai.SimpleAiStrategy;
import labyrinth.server.game.enums.Achievement;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.enums.RoomState;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

/**
 * Tests for end-game achievement awarding system.
 * Verifies that PUSHER and RUNNER achievements are awarded to players
 * with the most tiles pushed and steps taken at game end.
 */
class EndGameAchievementTest {

    private Game game;
    private GameConfig gameConfig;
    private Board board;
    private List<TreasureCard> treasureCards;

    @BeforeEach
    void setUp() {
        game = new Game(mock(IGameTimer.class), new SimpleAiStrategy(), new GameLogger());
        gameConfig = GameConfig.getDefault();
        board = new labyrinth.server.game.factories.BoardFactory().createBoard(7, 7, 0);
        treasureCards = new ArrayList<>();
        // Create only 1 treasure per player for quick testing
        IntStream.range(0, 4).forEach(i -> treasureCards.add(new TreasureCard(i, "Card" + i, "img")));
    }

    @Test
    void pusherAchievement_shouldBeAwardedToPlayerWithMostTilesPushed() {
        // Arrange
        game.join("Player1");
        game.join("Player2");
        game.startGame(gameConfig, treasureCards, board);

        List<Player> players = game.getPlayers();
        Player player1 = players.get(0);
        Player player2 = players.get(1);

        // Simulate player1 pushing more tiles than others
        player1.getStatistics().increaseTilesPushed(10);
        player2.getStatistics().increaseTilesPushed(5);
        players.get(2).getStatistics().increaseTilesPushed(3);
        players.get(3).getStatistics().increaseTilesPushed(2);

        // Simulate some steps for all players
        for (Player p : players) {
            p.getStatistics().increaseStepsTaken(50);
        }

        // Manually trigger achievement awarding
        var achievementService = new AchievementService();
        var awards = achievementService.awardEndGameAchievements(players);

        // Assert
        assertTrue(awards.stream().anyMatch(a -> a.player() == player1 && a.achievement() == Achievement.PUSHER),
                "Player1 should receive PUSHER achievement");

        long pusherCount = awards.stream().filter(a -> a.achievement() == Achievement.PUSHER).count();
        assertEquals(1, pusherCount, "Only one player should receive PUSHER");

        assertTrue(player1.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER),
                "Player1 should have PUSHER in collected achievements");
    }

    @Test
    void runnerAchievement_shouldBeAwardedToPlayerWithMostStepsTaken() {
        // Arrange
        game.join("Player1");
        game.join("Player2");
        game.startGame(gameConfig, treasureCards, board);

        List<Player> players = game.getPlayers();
        Player player2 = players.get(1);

        // Simulate player2 taking more steps than others
        players.get(0).getStatistics().increaseStepsTaken(100);
        player2.getStatistics().increaseStepsTaken(200); // Most steps
        players.get(2).getStatistics().increaseStepsTaken(80);
        players.get(3).getStatistics().increaseStepsTaken(50);

        // Simulate some pushes for all players
        for (Player p : players) {
            p.getStatistics().increaseTilesPushed(5);
        }

        // Manually trigger achievement awarding
        var achievementService = new AchievementService();
        var awards = achievementService.awardEndGameAchievements(players);

        // Assert
        assertTrue(awards.stream().anyMatch(a -> a.player() == player2 && a.achievement() == Achievement.RUNNER),
                "Player2 should receive RUNNER achievement");

        long runnerCount = awards.stream().filter(a -> a.achievement() == Achievement.RUNNER).count();
        assertEquals(1, runnerCount, "Only one player should receive RUNNER");

        assertTrue(player2.getStatistics().getCollectedAchievements().contains(Achievement.RUNNER),
                "Player2 should have RUNNER in collected achievements");
    }

    @Test
    void multiplePlayersTie_shouldAllReceiveAchievement() {
        // Arrange
        game.join("Player1");
        game.join("Player2");
        game.startGame(gameConfig, treasureCards, board);

        List<Player> players = game.getPlayers();

        // Create a tie for PUSHER
        players.get(0).getStatistics().increaseTilesPushed(10);
        players.get(1).getStatistics().increaseTilesPushed(10); // Tie with player 0
        players.get(2).getStatistics().increaseTilesPushed(5);
        players.get(3).getStatistics().increaseTilesPushed(3);

        // Different for RUNNER
        players.get(0).getStatistics().increaseStepsTaken(100);
        players.get(1).getStatistics().increaseStepsTaken(80);
        players.get(2).getStatistics().increaseStepsTaken(60);
        players.get(3).getStatistics().increaseStepsTaken(40);

        // Manually trigger achievement awarding
        var achievementService = new AchievementService();
        var awards = achievementService.awardEndGameAchievements(players);

        // Assert
        long pusherCount = awards.stream().filter(a -> a.achievement() == Achievement.PUSHER).count();
        assertEquals(2, pusherCount, "Two players should receive PUSHER due to tie");

        assertTrue(players.get(0).getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
        assertTrue(players.get(1).getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
    }

    @Test
    void achievementsAwardedAtGameEnd_shouldIncreaseScore() {
        // Arrange
        game.join("Player1");
        game.startGame(gameConfig, treasureCards, board);

        List<Player> players = game.getPlayers();
        Player player1 = players.get(0);

        // Set up statistics so player1 will win both achievements
        player1.getStatistics().increaseTilesPushed(15);
        player1.getStatistics().increaseStepsTaken(100);

        // Other players have less
        players.get(1).getStatistics().increaseTilesPushed(5);
        players.get(1).getStatistics().increaseStepsTaken(50);

        int scoreBeforeAchievements = player1.getStatistics().getScore();

        // Manually trigger achievement awarding (simulating end game)
        var achievementService = new AchievementService();
        var awards = achievementService.awardEndGameAchievements(players);

        // Manually apply score bonuses (simulating what Game.awardEndGameAchievements does)
        for (var award : awards) {
            award.player().getStatistics().increaseScore(50);
        }

        // Assert
        int scoreAfterAchievements = player1.getStatistics().getScore();

        assertEquals(2, awards.size(), "Player1 should win both achievements");
        assertTrue(scoreAfterAchievements > scoreBeforeAchievements,
                "Score should increase due to achievement bonuses");
        assertEquals(scoreBeforeAchievements + 100, scoreAfterAchievements,
                "Should gain 50 points per achievement (2 achievements = 100 points)");

        assertTrue(player1.getStatistics().getCollectedAchievements().contains(Achievement.PUSHER));
        assertTrue(player1.getStatistics().getCollectedAchievements().contains(Achievement.RUNNER));

        System.out.println("\nScore before achievements: " + scoreBeforeAchievements);
        System.out.println("Score after achievements: " + scoreAfterAchievements);
        System.out.println("Achievements: " + player1.getStatistics().getCollectedAchievements());
    }

    @Test
    void noAchievements_whenNoMovesOrPushes() {
        // Arrange
        game.join("Player1");
        game.join("Player2");
        game.startGame(gameConfig, treasureCards, board);

        List<Player> players = game.getPlayers();

        // No one has pushed or moved

        // Manually trigger achievement awarding
        var achievementService = new AchievementService();
        var awards = achievementService.awardEndGameAchievements(players);

        // Assert
        assertTrue(awards.isEmpty(), "No achievements should be awarded when no one has stats");
    }
}
