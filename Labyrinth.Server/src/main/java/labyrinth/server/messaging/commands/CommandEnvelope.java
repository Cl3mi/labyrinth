package labyrinth.server.messaging.commands;

import labyrinth.contracts.models.CommandType;

public record CommandEnvelope<T>(CommandType type, T payload) {
}
