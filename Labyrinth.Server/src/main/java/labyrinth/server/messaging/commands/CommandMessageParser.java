package labyrinth.server.messaging.commands;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import labyrinth.contracts.models.*;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Map;

@Component
public class CommandMessageParser {
    private final ObjectMapper mapper;

    private final Map<CommandType, Class<?>> dtoTypes = Map.ofEntries(
            Map.entry(CommandType.CONNECT, ConnectCommandPayload.class),
            Map.entry(CommandType.START_GAME, StartGameCommandPayload.class),
            Map.entry(CommandType.PUSH_TILE, PushTileCommandPayload.class),
            Map.entry(CommandType.MOVE_PAWN, MovePawnCommandPayload.class),
            Map.entry(CommandType.TOGGLE_AI, ToggleAiCommandPayload.class),
            Map.entry(CommandType.USE_BEAM, UseBeamCommandPayload.class),
            Map.entry(CommandType.USE_PUSH_FIXED, UsePushFixedCommandPayload.class),
            Map.entry(CommandType.USE_PUSH_TWICE, UsePushTwiceCommandPayload.class),
            Map.entry(CommandType.USE_SWAP, UseSwapCommandPayload.class)
    );

    public CommandMessageParser(ObjectMapper mapper) {
        this.mapper = mapper;
    }

    public CommandEnvelope<?> parse(String jsonString) throws IOException {
        JsonNode json = mapper.readTree(jsonString);

        if (!json.has("type")) {
            throw new IllegalArgumentException("Missing type");
        }

        String typeString = json.get("type").asText().toUpperCase();
        CommandType command = CommandType.valueOf(typeString);

        Class<?> dtoClass = dtoTypes.get(command);
        if (dtoClass == null) {
            throw new IllegalArgumentException("Unknown message type: " + typeString);
        }

        var payload = mapper.treeToValue(json, dtoClass);
        return new CommandEnvelope<>(command, payload);
    }
}
