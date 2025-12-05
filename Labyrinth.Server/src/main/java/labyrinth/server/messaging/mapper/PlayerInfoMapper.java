package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.PlayerInfo;
import labyrinth.server.game.models.Player;
import org.springframework.stereotype.Component;

@Component
public class PlayerInfoMapper {
    public PlayerInfo toDto(Player player) {
        PlayerInfo dto = new PlayerInfo();
        dto.setId(player.getId().toString());
        dto.setName(player.getUsername());
        dto.setColor(player.getColor());
        dto.setIsAdmin(player.isAdmin());

        return dto;
    }
}
