package labyrinth.contracts.models;
import labyrinth.contracts.models.SlashGame;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.PlayerInfo;
import java.util.Map;
import java.util.Objects;
public class PlayerUpdatedEventPayload implements SlashGame {
  private EventType type;
  private PlayerInfo player;
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public PlayerInfo getPlayer() { return this.player; }
  public void setPlayer(PlayerInfo player) { this.player = player; }

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
    PlayerUpdatedEventPayload self = (PlayerUpdatedEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.player, self.player) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)player, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PlayerUpdatedEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    player: " + toIndentedString(player) + "\n" +
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