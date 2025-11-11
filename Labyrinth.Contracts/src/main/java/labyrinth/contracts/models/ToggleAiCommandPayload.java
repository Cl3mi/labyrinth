package labyrinth.contracts.models;
import labyrinth.contracts.models.CommandType;
import java.util.Map;
import java.util.Objects;
public class ToggleAiCommandPayload {
  private CommandType type;
  private boolean enabled;
  private Map<String, Object> additionalProperties;

  public CommandType getType() { return this.type; }
  public void setType(CommandType type) { this.type = type; }

  public boolean getEnabled() { return this.enabled; }
  public void setEnabled(boolean enabled) { this.enabled = enabled; }

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
    ToggleAiCommandPayload self = (ToggleAiCommandPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.enabled, self.enabled) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)enabled, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class ToggleAiCommandPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    enabled: " + toIndentedString(enabled) + "\n" +
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