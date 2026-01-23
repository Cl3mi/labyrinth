package labyrinth.client.unit.util;

import labyrinth.client.util.UriHelper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.net.URI;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("UriHelper")
class UriHelperTest {

    @ParameterizedTest(name = "getGameUri({0}) should return {1}")
    @CsvSource({
        "localhost:8080, ws://localhost:8080/game",
        "ws://localhost:8080, ws://localhost:8080/game",
        "wss://example.com, wss://example.com/game",
        "localhost:8080/, ws://localhost:8080/game",
        "ws://localhost:8080/game, ws://localhost:8080/game",
        "192.168.1.1:9090, ws://192.168.1.1:9090/game",
        "example.com/api, ws://example.com/api/game"
    })
    void getGameUri_formatsUriCorrectly(String input, String expected) {
        URI uri = UriHelper.getGameUri(input);
        assertThat(uri.toString()).isEqualTo(expected);
    }
}
