package labyrinth.contracts.models;
import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.Coordinates;
import java.util.Map;
import java.util.Objects;
public class UseBeamCommandPayload {
  private CommandType type;
  private Coordinates targetCoordinates;
  private Map<String, Object> additionalProperties;

  public CommandType getType() { return this.type; }
  public void setType(CommandType type) { this.type = type; }

  public Coordinates getTargetCoordinates() { return this.targetCoordinates; }
  public void setTargetCoordinates(Coordinates targetCoordinates) { this.targetCoordinates = targetCoordinates; }

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
    UseBeamCommandPayload self = (UseBeamCommandPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.targetCoordinates, self.targetCoordinates) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)targetCoordinates, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class UseBeamCommandPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    targetCoordinates: " + toIndentedString(targetCoordinates) + "\n" +
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