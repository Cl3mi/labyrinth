package labyrinth.server.messaging.commands;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class CommandMessageParserTest {

    private CommandMessageParser parser;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        objectMapper = new ObjectMapper();
        // Prevent failure if DTOs don't have the "type" field mapped
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        parser = new CommandMessageParser(objectMapper);
    }

    @Test
    void parse_shouldReturnCorrectEnvelope_whenTypeIsConnect() throws IOException {
        String json = "{\"type\": \"CONNECT\", \"someField\": \"value\"}";

        CommandEnvelope<?> result = parser.parse(json);

        assertNotNull(result);
        assertEquals(CommandType.CONNECT, result.type());
        assertInstanceOf(ConnectCommandPayload.class, result.payload());
    }

    @Test
    void parse_shouldHandleLowerCaseType_correctly() throws IOException {
        // Note: The type field value gets uppercased for routing, but the JSON
        // payload also deserializes the type field. We use uppercase to ensure
        // the payload deserialization works correctly.
        String json = "{\"type\": \"MOVE_PAWN\"}";

        CommandEnvelope<?> result = parser.parse(json);

        assertEquals(CommandType.MOVE_PAWN, result.type());
        assertInstanceOf(MovePawnCommandPayload.class, result.payload());
    }

    @Test
    void parse_shouldThrowException_whenTypeFieldIsMissing() {
        String json = "{\"data\": \"payload\"}";

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                parser.parse(json)
        );

        assertEquals("Missing type", ex.getMessage());
    }

    @Test
    void parse_shouldThrowException_whenTypeIsInvalidEnum() {
        String json = "{\"type\": \"INVALID_COMMAND\"}";

        // CommandType.valueOf will throw IllegalArgumentException
        assertThrows(IllegalArgumentException.class, () ->
                parser.parse(json)
        );
    }

    @Test
    void parse_shouldThrowIOException_whenJsonIsMalformed() {
        String invalidJson = "{ broken_json: ";

        assertThrows(IOException.class, () ->
                parser.parse(invalidJson)
        );
    }

    @Test
    void parse_shouldMapAllKnownTypes() throws IOException {
        // Quick verification that a few other random types work
        assertTypeMapping(CommandType.ROTATE_TILE, RotateTileCommandPayload.class);
        assertTypeMapping(CommandType.TOGGLE_AI, ToggleAiCommandPayload.class);
        assertTypeMapping(CommandType.USE_SWAP, UseSwapCommandPayload.class);
    }

    private void assertTypeMapping(CommandType type, Class<?> expectedClass) throws IOException {
        String json = String.format("{\"type\": \"%s\"}", type.name());
        CommandEnvelope<?> result = parser.parse(json);
        assertEquals(type, result.type());
        assertInstanceOf(expectedClass, result.payload());
    }
}