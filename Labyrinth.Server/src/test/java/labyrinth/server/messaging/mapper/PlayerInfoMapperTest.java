package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.PlayerInfo;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.PlayerSessionRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

class PlayerInfoMapperTest {

    @Mock
    private PlayerSessionRegistry registry;

    private PlayerInfoMapper mapper;

    @BeforeEach
    void setup() {
        MockitoAnnotations.openMocks(this);
        mapper = new PlayerInfoMapper(registry);
    }

    @Test
    void mapsPlayerInfoAndConnectedFlag() {
        UUID id = UUID.fromString("00000000-0000-0000-0000-000000000001");
        var player = new Player(id, "TestPlayer");
        player.setColor(labyrinth.contracts.models.PlayerColor.RED);
        player.setAdmin(true);
        player.setAiActive(false);

        when(registry.isPlayerConnected(player.getId())).thenReturn(true);

        PlayerInfo dto = mapper.toDto(player);

        assertEquals("00000000-0000-0000-0000-000000000001", dto.getId());
        assertEquals("TestPlayer", dto.getName());
        assertEquals(labyrinth.contracts.models.PlayerColor.RED, dto.getColor());
        assertEquals(true, dto.getIsAdmin());
        assertEquals(false, dto.getIsAiControlled());
        assertEquals(true, dto.getIsConnected());
    }
}
