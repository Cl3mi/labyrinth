package labyrinth.server.messaging.events;

import labyrinth.contracts.models.EventType;

public record EventEnvelope<T>(EventType type, T payload) {
}