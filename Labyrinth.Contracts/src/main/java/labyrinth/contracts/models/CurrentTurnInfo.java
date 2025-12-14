package labyrinth.contracts.models;
import labyrinth.contracts.models.TurnState;
import java.util.Map;
import java.util.Objects;
public class CurrentTurnInfo {
  private String currentPlayerId;
  private java.time.OffsetDateTime turnEndTime;
  private TurnState state;
  private Map<String, Object> additionalProperties;

  public String getCurrentPlayerId() { return this.currentPlayerId; }
  public void setCurrentPlayerId(String currentPlayerId) { this.currentPlayerId = currentPlayerId; }

  public java.time.OffsetDateTime getTurnEndTime() { return this.turnEndTime; }
  public void setTurnEndTime(java.time.OffsetDateTime turnEndTime) { this.turnEndTime = turnEndTime; }

  public TurnState getState() { return this.state; }
  public void setState(TurnState state) { this.state = state; }

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
    CurrentTurnInfo self = (CurrentTurnInfo) o;
      return 
        Objects.equals(this.currentPlayerId, self.currentPlayerId) &&
        Objects.equals(this.turnEndTime, self.turnEndTime) &&
        Objects.equals(this.state, self.state) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)currentPlayerId, (Object)turnEndTime, (Object)state, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class CurrentTurnInfo {\n" +   
      "    currentPlayerId: " + toIndentedString(currentPlayerId) + "\n" +
      "    turnEndTime: " + toIndentedString(turnEndTime) + "\n" +
      "    state: " + toIndentedString(state) + "\n" +
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