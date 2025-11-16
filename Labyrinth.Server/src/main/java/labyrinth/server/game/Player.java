package labyrinth.server.game;

import labyrinth.contracts.models.PlayerColor;
import lombok.Getter;
import lombok.Setter;

import java.time.OffsetDateTime;
import java.util.UUID;

@Setter
@Getter
public class Player {
    private UUID id;
    private String username;
    private OffsetDateTime joinDate;
    private int score;
    private boolean isAiActive;
    private boolean isAdmin;
    private PlayerColor color;
}
