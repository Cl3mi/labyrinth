package labyrinth.server.config;

import labyrinth.server.game.factories.BonusFactory;
import labyrinth.server.game.services.GameInitializerService;
import labyrinth.server.game.services.TreasureBonusDistributionService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class GameConfig {

    @Bean
    public TreasureBonusDistributionService treasureBonusDistributionService(BonusFactory bonusFactory) {
        return new TreasureBonusDistributionService(bonusFactory);
    }

    @Bean
    public GameInitializerService gameInitializer(TreasureBonusDistributionService distributionService) {
        return new GameInitializerService(distributionService);
    }
}
