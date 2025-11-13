package labyrinth.server.models;

import labyrinth.contracts.models.CommandType;

public record CommandMessage(CommandType type, Object payload) {
}
