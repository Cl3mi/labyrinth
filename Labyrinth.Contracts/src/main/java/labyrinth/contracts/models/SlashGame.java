package labyrinth.contracts.models;
import labyrinth.contracts.models.ServerInfoPayload;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.ConnectAckEventPayload;
import labyrinth.contracts.models.GameStartedEventPayload;
import labyrinth.contracts.models.GameStateUpdateEventPayload;
import labyrinth.contracts.models.PlayerTurnEventPayload;
import labyrinth.contracts.models.ActionErrorEventPayload;
import labyrinth.contracts.models.PlayerDisconnectedEventPayload;
import labyrinth.contracts.models.AchievementUnlockedEventPayload;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.NextTreasureCardEventPayload;
import java.util.Map;
/**
 * SlashGame represents a union of types: ServerInfoPayload, LobbyStateEventPayload, ConnectAckEventPayload, GameStartedEventPayload, GameStateUpdateEventPayload, PlayerTurnEventPayload, ActionErrorEventPayload, PlayerDisconnectedEventPayload, AchievementUnlockedEventPayload, GameOverEventPayload, NextTreasureCardEventPayload
 */
public interface SlashGame {
  
}