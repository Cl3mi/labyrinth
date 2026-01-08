package labyrinth.server.config;

import labyrinth.managementclient.ApiClient;
import labyrinth.managementclient.api.ServersApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ManagementClientConfig {

    @Value("${management.api.url:https://game.clxmi.com}")
    private String basePath;

    @Bean
    public ApiClient managementApiClient() {
        ApiClient client = new ApiClient();
        client.setBasePath(basePath);
        return client;
    }

    @Bean
    public ServersApi serversApi(ApiClient apiClient) {
        return new ServersApi(apiClient);
    }
}
