package labyrinth.contracts.models;
import labyrinth.contracts.SlashGame;
import labyrinth.contracts.model.EventType;
import labyrinth.contracts.model.ErrorCode;
import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class ActionErrorEventPayload implements SlashGame {
  @JsonProperty("type")
  private EventType type;
  @JsonProperty("errorCode")
  private ErrorCode errorCode;
  @JsonProperty("message")
  private String message;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public ErrorCode getErrorCode() { return this.errorCode; }
  public void setErrorCode(ErrorCode errorCode) { this.errorCode = errorCode; }

  public String getMessage() { return this.message; }
  public void setMessage(String message) { this.message = message; }

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
    ActionErrorEventPayload self = (ActionErrorEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.errorCode, self.errorCode) &&
        Objects.equals(this.message, self.message) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)errorCode, (Object)message, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class ActionErrorEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    errorCode: " + toIndentedString(errorCode) + "\n" +
      "    message: " + toIndentedString(message) + "\n" +
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