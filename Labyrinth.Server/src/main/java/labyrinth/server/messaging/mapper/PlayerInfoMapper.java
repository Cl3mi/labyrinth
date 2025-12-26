package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.PlayerInfo;
import labyrinth.server.game.models.Player;
import labyrinth.server.messaging.PlayerSessionRegistry;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PlayerInfoMapper {
    private final PlayerSessionRegistry playerSessionRegistry;

    public PlayerInfo toDto(Player player) {
        PlayerInfo dto = new PlayerInfo();
        dto.setId(player.getId().toString());
        dto.setName(player.getUsername());
        dto.setColor(player.getColor());
        dto.setIsAdmin(player.isAdmin());
        dto.setIsAiControlled(player.isAiActive());
        dto.setIsConnected(playerSessionRegistry.isPlayerConnected(player.getId()));

        return dto;
    }
}
