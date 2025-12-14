package labyrinth.contracts.models;
import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.Direction;
import java.util.Map;
import java.util.Objects;
public class UsePushFixedCommandPayload {
  private CommandType type;
  private int rowOrColIndex;
  private Direction direction;
  private Map<String, Object> additionalProperties;

  public CommandType getType() { return this.type; }
  public void setType(CommandType type) { this.type = type; }

  public int getRowOrColIndex() { return this.rowOrColIndex; }
  public void setRowOrColIndex(int rowOrColIndex) { this.rowOrColIndex = rowOrColIndex; }

  public Direction getDirection() { return this.direction; }
  public void setDirection(Direction direction) { this.direction = direction; }

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
    UsePushFixedCommandPayload self = (UsePushFixedCommandPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.rowOrColIndex, self.rowOrColIndex) &&
        Objects.equals(this.direction, self.direction) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)rowOrColIndex, (Object)direction, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class UsePushFixedCommandPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    rowOrColIndex: " + toIndentedString(rowOrColIndex) + "\n" +
      "    direction: " + toIndentedString(direction) + "\n" +
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