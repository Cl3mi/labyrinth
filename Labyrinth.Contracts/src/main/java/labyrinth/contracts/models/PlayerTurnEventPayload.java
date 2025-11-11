package labyrinth.contracts.models;
import labyrinth.contracts.SlashGame;
import labyrinth.contracts.model.EventType;

import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class PlayerTurnEventPayload implements SlashGame {
  @JsonProperty("type")
  private EventType type;
  @JsonProperty("playerId")
  private String playerId;
  @JsonProperty("extraTile")
  private Tile extraTile;
  @JsonProperty("turnTimeLimitSeconds")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer turnTimeLimitSeconds;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public String getPlayerId() { return this.playerId; }
  public void setPlayerId(String playerId) { this.playerId = playerId; }

  public Tile getExtraTile() { return this.extraTile; }
  public void setExtraTile(Tile extraTile) { this.extraTile = extraTile; }

  public Integer getTurnTimeLimitSeconds() { return this.turnTimeLimitSeconds; }
  public void setTurnTimeLimitSeconds(Integer turnTimeLimitSeconds) { this.turnTimeLimitSeconds = turnTimeLimitSeconds; }

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
    PlayerTurnEventPayload self = (PlayerTurnEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.playerId, self.playerId) &&
        Objects.equals(this.extraTile, self.extraTile) &&
        Objects.equals(this.turnTimeLimitSeconds, self.turnTimeLimitSeconds) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)playerId, (Object)extraTile, (Object)turnTimeLimitSeconds, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PlayerTurnEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    playerId: " + toIndentedString(playerId) + "\n" +
      "    extraTile: " + toIndentedString(extraTile) + "\n" +
      "    turnTimeLimitSeconds: " + toIndentedString(turnTimeLimitSeconds) + "\n" +
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