package labyrinth.contracts.models;
import labyrinth.contracts.SlashGame;
import labyrinth.contracts.model.EventType;

import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class NextTreasureCardEventPayload implements SlashGame {
  @JsonProperty("type")
  private EventType type;
  @JsonProperty("treasure")
  private Treasure treasure;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public Treasure getTreasure() { return this.treasure; }
  public void setTreasure(Treasure treasure) { this.treasure = treasure; }

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
    NextTreasureCardEventPayload self = (NextTreasureCardEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.treasure, self.treasure) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)treasure, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class NextTreasureCardEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    treasure: " + toIndentedString(treasure) + "\n" +
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