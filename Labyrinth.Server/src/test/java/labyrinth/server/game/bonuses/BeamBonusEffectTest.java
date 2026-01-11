package labyrinth.server.game.bonuses;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.PlayerStatistics;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class BeamBonusEffectTest {

    private Game game;
    private Board board;
    private Player player;
    private Tile targetTile;
    private BeamBonusEffect beamBonusEffect;

    @BeforeEach
    void setUp() {
        game = mock(Game.class);
        board = mock(Board.class);
        player = mock(Player.class);
        targetTile = mock(Tile.class);
        beamBonusEffect = new BeamBonusEffect();

        when(game.getBoard()).thenReturn(board);
        when(board.getTileAt(anyInt(), anyInt())).thenReturn(targetTile);
        when(game.getPlayers()).thenReturn(Collections.singletonList(player));

        PlayerStatistics stats = mock(PlayerStatistics.class);
        when(player.getStatistics()).thenReturn(stats);
    }

    @Test
    void apply_shouldMovePlayer_whenTileIsFreeAndBonusAvailable() {
        // Arrange
        when(player.getCurrentTile()).thenReturn(mock(Tile.class)); // Current tile is different
        when(player.useBonus(BonusTypes.BEAM)).thenReturn(true);

        // Act
        boolean result = beamBonusEffect.apply(game, player, 1, 2);

        // Assert
        assertTrue(result);
        verify(player).setCurrentTile(targetTile);
        verify(player).useBonus(BonusTypes.BEAM);
    }

    @Test
    void apply_shouldFail_whenPlayerDoNotHaveBonus() {
        // Arrange
        when(player.useBonus(BonusTypes.BEAM)).thenReturn(false);

        // Act
        boolean result = beamBonusEffect.apply(game, player, 1, 2);

        // Assert
        assertFalse(result);
        verify(player, never()).setCurrentTile(any());
    }

    @Test
    void apply_shouldFail_whenTargetTileIsOccupied() {
        // Arrange
        Player otherPlayer = mock(Player.class);
        when(otherPlayer.getCurrentTile()).thenReturn(targetTile);
        when(game.getPlayers()).thenReturn(List.of(player, otherPlayer));

        // Act
        boolean result = beamBonusEffect.apply(game, player, 1, 2);

        // Assert
        assertFalse(result);
        verify(player, never()).setCurrentTile(any());
    }
}
