package labyrinth.contracts.models;
import labyrinth.contracts.models.CommandType;
import java.util.Map;
import java.util.Objects;
public class DisconnectCommandPayload {
  private CommandType type;
  private String username;
  private Map<String, Object> additionalProperties;

  public CommandType getType() { return this.type; }
  public void setType(CommandType type) { this.type = type; }

  public String getUsername() { return this.username; }
  public void setUsername(String username) { this.username = username; }

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
    DisconnectCommandPayload self = (DisconnectCommandPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.username, self.username) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)username, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class DisconnectCommandPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    username: " + toIndentedString(username) + "\n" +
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