package labyrinth.server.messaging.mapper;

import labyrinth.contracts.models.BonusType;
import labyrinth.server.game.enums.BonusTypes;
import org.springframework.stereotype.Component;

@Component
public class BonusMapper {
    public BonusType toDto(BonusTypes bonusType) {
        return switch (bonusType) {
            case SWAP -> BonusType.SWAP;
            case BEAM -> BonusType.BEAM;
            case PUSH_TWICE -> BonusType.PUSH_TWICE;
            case PUSH_FIXED -> BonusType.PUSH_FIXED;
        };
    }
}
