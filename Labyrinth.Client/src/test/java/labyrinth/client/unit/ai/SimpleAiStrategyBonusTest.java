package labyrinth.client.unit.ai;

import labyrinth.client.ai.AiDecision;
import labyrinth.client.ai.SimpleAiStrategy;
import labyrinth.client.ai.SimulationResult;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("SimpleAiStrategy - Bonus Scenarios")
class SimpleAiStrategyBonusTest {

    private static final int BOARD_SIZE = 7;
    private SimpleAiStrategy aiStrategy;
    private Board board;
    private Player player;
    private Tile[][] tiles;

    @BeforeEach
    void setUp() {
        aiStrategy = new SimpleAiStrategy();
        tiles = new Tile[BOARD_SIZE][BOARD_SIZE];
        for (int row = 0; row < BOARD_SIZE; row++) {
            for (int col = 0; col < BOARD_SIZE; col++) {
                tiles[row][col] = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
            }
        }
        Tile extraTile = createTile(Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT);
        board = new Board(BOARD_SIZE, BOARD_SIZE, tiles, extraTile);

        player = new Player("p1", "Player 1");
        player.setCurrentPosition(new Position(0, 0));
        player.setHomePosition(new Position(0, 0));
    }

    private Tile createTile(Direction... entrances) {
        Tile tile = new Tile();
        tile.setEntrances(entrances);
        tile.setIsFixed(false);
        return tile;
    }

    @Test
    @DisplayName("trySwapToStealTreasure - opponent on target - should SWAP")
    void trySwapToStealTreasure_opponentOnTarget_shouldSwap() {
        // Given
        player.setAvailableBonuses(List.of(BonusType.SWAP));
        
        Treasure target = new Treasure();
        target.setId(1);
        player.setCurrentTargetTreasure(target);
        
        // Treasure is at (6,6)
        tiles[6][6].setTreasure(target);
        
        // Opponent is at (6,6)
        Player opponent = new Player("p2", "Opponent");
        opponent.setCurrentPosition(new Position(6, 6));
        
        List<Player> allPlayers = new ArrayList<>();
        allPlayers.add(player);
        allPlayers.add(opponent);

        // When
        AiDecision decision = aiStrategy.computeBestMove(board, player, allPlayers);

        // Then
        assertThat(decision).isNotNull();
        assertThat(decision.bonusAction()).isNotNull();
        assertThat(decision.bonusAction().bonusType()).isEqualTo(BonusType.SWAP);
        assertThat(decision.bonusAction().targetPlayerId()).isEqualTo("p2");
    }

    @Test
    @DisplayName("tryDoubleTreasureCollection - BEAM to T1 and walk to T2 - should BEAM")
    void tryDoubleTreasureCollection_beamToT1WalkToT2_shouldBeam() {
        // Given
        player.setAvailableBonuses(List.of(BonusType.BEAM));
        player.setRemainingTreasureCount(2); // Needs > 1 treasure remaining
        
        Treasure t1 = new Treasure(); t1.setId(1);
        Treasure t2 = new Treasure(); t2.setId(2);
        
        player.setCurrentTargetTreasure(t1);
        
        // T1 at (3,3), T2 at (3,4) - connected
        tiles[3][3].setTreasure(t1);
        tiles[3][4].setTreasure(t2);
        
        // Player is far away and blocked from walking
        player.setCurrentPosition(new Position(0, 0));
        tiles[0][0].setEntrances(new Direction[]{}); // No way out
        tiles[0][1].setEntrances(new Direction[]{});
        tiles[1][0].setEntrances(new Direction[]{});

        // When
        AiDecision decision = aiStrategy.computeBestMove(board, player);

        // Then
        assertThat(decision).isNotNull();
        assertThat(decision.bonusAction()).isNotNull();
        assertThat(decision.bonusAction().bonusType()).isEqualTo(BonusType.BEAM);
        // Should beam to T1 (3,3)
        assertThat(decision.bonusAction().targetPosition()).isEqualTo(new Position(3, 3));
    }

    @Test
    @DisplayName("tryDoubleTreasureCollection - SWAP to T1 and walk to T2 - should SWAP")
    void tryDoubleTreasureCollection_swapToT1WalkToT2_shouldSwap() {
        // Given
        player.setAvailableBonuses(List.of(BonusType.SWAP));
        player.setRemainingTreasureCount(2); 
        
        Treasure t1 = new Treasure(); t1.setId(1);
        Treasure t2 = new Treasure(); t2.setId(2);
        
        player.setCurrentTargetTreasure(t1);
        
        // T1 at (3,3), T2 at (3,4)
        tiles[3][3].setTreasure(t1);
        tiles[3][4].setTreasure(t2);
        
        // Player is far away and blocked
        player.setCurrentPosition(new Position(0, 0));
        tiles[0][0].setEntrances(new Direction[]{}); // No way out
        
        // Opponent is at T1 (3,3)
        Player opponent = new Player("p2", "Opponent");
        opponent.setCurrentPosition(new Position(3, 3));
        
        List<Player> allPlayers = List.of(player, opponent);

        // When
        AiDecision decision = aiStrategy.computeBestMove(board, player, allPlayers);

        // Then
        assertThat(decision).isNotNull();
        assertThat(decision.bonusAction()).isNotNull();
        assertThat(decision.bonusAction().bonusType()).isEqualTo(BonusType.SWAP);
        assertThat(decision.bonusAction().targetPlayerId()).isEqualTo("p2");
    }
}
