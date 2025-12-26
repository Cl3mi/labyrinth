package labyrinth.client.messaging;

/**
 * Represents the current state of the WebSocket connection.
 * Used to manage reconnection logic and prevent race conditions.
 */
public enum ConnectionState {
    /** Not connected, no active connection attempt */
    DISCONNECTED,

    /** Initial connection attempt in progress */
    CONNECTING,

    /** Successfully connected and active */
    CONNECTED,

    /** Automatic reconnection attempts in progress (after unintentional disconnect) */
    RECONNECTING,

    /** Intentional disconnect initiated by user (clean shutdown) */
    DISCONNECTING
}
