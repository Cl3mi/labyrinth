package labyrinth.contracts.models;
import labyrinth.contracts.models.CommandType;
import java.util.Map;
import java.util.Objects;
public class ConnectCommandPayload {
  private CommandType type;
  private String username;
  private String identifierToken;
  private Map<String, Object> additionalProperties;

  public CommandType getType() { return this.type; }
  public void setType(CommandType type) { this.type = type; }

  public String getUsername() { return this.username; }
  public void setUsername(String username) { this.username = username; }

  public String getIdentifierToken() { return this.identifierToken; }
  public void setIdentifierToken(String identifierToken) { this.identifierToken = identifierToken; }

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
    ConnectCommandPayload self = (ConnectCommandPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.username, self.username) &&
        Objects.equals(this.identifierToken, self.identifierToken) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)username, (Object)identifierToken, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class ConnectCommandPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    username: " + toIndentedString(username) + "\n" +
      "    identifierToken: " + toIndentedString(identifierToken) + "\n" +
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