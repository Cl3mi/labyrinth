package labyrinth.client.messaging;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import labyrinth.managementclient.model.GameServer;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.List;

public class ServerClientFactory {

    /**
     * Simple API wrapper that uses Java's built-in HttpClient instead of Spring RestTemplate.
     * This avoids issues with shaded JAR and Spring's complex dependency chain.
     */
    public static class SimpleServersApi {
        private final String baseUrl;
        private final HttpClient httpClient;
        private final ObjectMapper objectMapper;

        public SimpleServersApi(String baseUrl) {
            this.baseUrl = baseUrl.endsWith("/") ? baseUrl.substring(0, baseUrl.length() - 1) : baseUrl;
            this.httpClient = HttpClient.newBuilder()
                    .connectTimeout(Duration.ofSeconds(5))
                    .build();
            this.objectMapper = new ObjectMapper();
            this.objectMapper.registerModule(new JavaTimeModule());
            this.objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        }

        public List<GameServer> listServers() {
            try {
                HttpRequest request = HttpRequest.newBuilder()
                        .uri(URI.create(baseUrl + "/servers"))
                        .timeout(Duration.ofSeconds(10))
                        .header("Accept", "application/json")
                        .GET()
                        .build();

                HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

                if (response.statusCode() == 200) {
                    return objectMapper.readValue(response.body(), new TypeReference<List<GameServer>>() {});
                } else {
                    System.err.println("[SimpleServersApi] HTTP " + response.statusCode() + ": " + response.body());
                    return List.of();
                }
            } catch (Exception e) {
                System.err.println("[SimpleServersApi] Error: " + e.getMessage());
                throw new RuntimeException("Failed to fetch servers", e);
            }
        }
    }

    public static SimpleServersApi create(String serverUrl) {
        System.out.println("[ServerClientFactory] Creating SimpleServersApi with URL: " + serverUrl);
        return new SimpleServersApi(serverUrl);
    }
}
