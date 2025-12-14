package labyrinth.contracts.models;
import labyrinth.contracts.models.SlashGame;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.GameBoard;
import labyrinth.contracts.models.PlayerState;
import labyrinth.contracts.models.CurrentTurnInfo;
import java.util.Map;
import java.util.Objects;
public class GameStateEventPayload implements SlashGame {
  private EventType type;
  private GameBoard board;
  private PlayerState[] players;
  private CurrentTurnInfo currentTurnInfo;
  private java.time.OffsetDateTime gameEndTime;
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public GameBoard getBoard() { return this.board; }
  public void setBoard(GameBoard board) { this.board = board; }

  public PlayerState[] getPlayers() { return this.players; }
  public void setPlayers(PlayerState[] players) { this.players = players; }

  public CurrentTurnInfo getCurrentTurnInfo() { return this.currentTurnInfo; }
  public void setCurrentTurnInfo(CurrentTurnInfo currentTurnInfo) { this.currentTurnInfo = currentTurnInfo; }

  public java.time.OffsetDateTime getGameEndTime() { return this.gameEndTime; }
  public void setGameEndTime(java.time.OffsetDateTime gameEndTime) { this.gameEndTime = gameEndTime; }

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
    GameStateEventPayload self = (GameStateEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.board, self.board) &&
        Objects.equals(this.players, self.players) &&
        Objects.equals(this.currentTurnInfo, self.currentTurnInfo) &&
        Objects.equals(this.gameEndTime, self.gameEndTime) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)board, (Object)players, (Object)currentTurnInfo, (Object)gameEndTime, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class GameStateEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    board: " + toIndentedString(board) + "\n" +
      "    players: " + toIndentedString(players) + "\n" +
      "    currentTurnInfo: " + toIndentedString(currentTurnInfo) + "\n" +
      "    gameEndTime: " + toIndentedString(gameEndTime) + "\n" +
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