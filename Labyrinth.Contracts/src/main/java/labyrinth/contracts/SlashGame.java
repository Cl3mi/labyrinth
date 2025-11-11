package labyrinth.contracts;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.GameStartedEventPayload;
import labyrinth.contracts.models.GameStateUpdateEventPayload;
import labyrinth.contracts.models.PlayerTurnEventPayload;
import labyrinth.contracts.models.ActionErrorEventPayload;
import labyrinth.contracts.models.PlayerDisconnectedEventPayload;
import labyrinth.contracts.models.AchievementUnlockedEventPayload;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.NextTreasureCardEventPayload;
import com.fasterxml.jackson.annotation.*;
@JsonTypeInfo(use=JsonTypeInfo.Id.DEDUCTION)
@JsonSubTypes({
  @JsonSubTypes.Type(value = LobbyStateEventPayload.class, name = "LobbyStateEventPayload"),
  @JsonSubTypes.Type(value = GameStartedEventPayload.class, name = "GameStartedEventPayload"),
  @JsonSubTypes.Type(value = GameStateUpdateEventPayload.class, name = "GameStateUpdateEventPayload"),
  @JsonSubTypes.Type(value = PlayerTurnEventPayload.class, name = "PlayerTurnEventPayload"),
  @JsonSubTypes.Type(value = ActionErrorEventPayload.class, name = "ActionErrorEventPayload"),
  @JsonSubTypes.Type(value = PlayerDisconnectedEventPayload.class, name = "PlayerDisconnectedEventPayload"),
  @JsonSubTypes.Type(value = AchievementUnlockedEventPayload.class, name = "AchievementUnlockedEventPayload"),
  @JsonSubTypes.Type(value = GameOverEventPayload.class, name = "GameOverEventPayload"),
  @JsonSubTypes.Type(value = NextTreasureCardEventPayload.class, name = "NextTreasureCardEventPayload")
})
/**
 * SlashGame represents a union of types: LobbyStateEventPayload, GameStartedEventPayload, GameStateUpdateEventPayload, PlayerTurnEventPayload, ActionErrorEventPayload, PlayerDisconnectedEventPayload, AchievementUnlockedEventPayload, GameOverEventPayload, NextTreasureCardEventPayload
 */
public interface SlashGame {
  
}