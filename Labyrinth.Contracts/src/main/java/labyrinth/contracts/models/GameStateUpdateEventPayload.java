package labyrinth.contracts.models;
import labyrinth.contracts.SlashGame;
import labyrinth.contracts.model.EventType;
import labyrinth.contracts.model.TurnState;
import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class GameStateUpdateEventPayload implements SlashGame {
  @JsonProperty("type")
  private EventType type;
  @JsonProperty("board")
  private GameBoard board;
  @JsonProperty("players")
  private PlayerState[] players;
  @JsonProperty("currentPlayerId")
  private String currentPlayerId;
  @JsonProperty("currentTurnState")
  private TurnState currentTurnState;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public GameBoard getBoard() { return this.board; }
  public void setBoard(GameBoard board) { this.board = board; }

  public PlayerState[] getPlayers() { return this.players; }
  public void setPlayers(PlayerState[] players) { this.players = players; }

  public String getCurrentPlayerId() { return this.currentPlayerId; }
  public void setCurrentPlayerId(String currentPlayerId) { this.currentPlayerId = currentPlayerId; }

  public TurnState getCurrentTurnState() { return this.currentTurnState; }
  public void setCurrentTurnState(TurnState currentTurnState) { this.currentTurnState = currentTurnState; }

  @JsonAnyGetter
  public Map<String, Object> getAdditionalProperties() { return this.additionalProperties; }
  public void setAdditionalProperties(Map<String, Object> additionalProperties) { this.additionalProperties = additionalProperties; }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    GameStateUpdateEventPayload self = (GameStateUpdateEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.board, self.board) &&
        Objects.equals(this.players, self.players) &&
        Objects.equals(this.currentPlayerId, self.currentPlayerId) &&
        Objects.equals(this.currentTurnState, self.currentTurnState) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)board, (Object)players, (Object)currentPlayerId, (Object)currentTurnState, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class GameStateUpdateEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    board: " + toIndentedString(board) + "\n" +
      "    players: " + toIndentedString(players) + "\n" +
      "    currentPlayerId: " + toIndentedString(currentPlayerId) + "\n" +
      "    currentTurnState: " + toIndentedString(currentTurnState) + "\n" +
      "    additionalProperties: " + toIndentedString(additionalProperties) + "\n" +
    "}";
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}