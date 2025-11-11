package labyrinth.contracts.models;
import labyrinth.contracts.SlashGame;
import labyrinth.contracts.model.EventType;

import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class LobbyStateEventPayload implements SlashGame {
  @JsonProperty("type")
  private EventType type;
  @JsonProperty("players")
  private PlayerInfo[] players;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public PlayerInfo[] getPlayers() { return this.players; }
  public void setPlayers(PlayerInfo[] players) { this.players = players; }

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
    LobbyStateEventPayload self = (LobbyStateEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.players, self.players) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)players, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class LobbyStateEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    players: " + toIndentedString(players) + "\n" +
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