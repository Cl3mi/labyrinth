package labyrinth.client.messaging;

import labyrinth.managementclient.ApiClient;
import labyrinth.managementclient.api.ServersApi;

public class ServerClientFactory {
    public static ServersApi create(String serverUrl) {
        var apiClient = new ApiClient();
        apiClient.setBasePath(serverUrl);

        return new ServersApi(apiClient);
    }
}
