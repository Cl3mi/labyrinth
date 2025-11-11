package labyrinth.contracts.models;
import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
import labyrinth.contracts.enums.Direction;

public class PushActionInfo {
  @JsonProperty("rowOrColIndex")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer rowOrColIndex;
  @JsonProperty("direction")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Direction direction;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public Integer getRowOrColIndex() { return this.rowOrColIndex; }
  public void setRowOrColIndex(Integer rowOrColIndex) { this.rowOrColIndex = rowOrColIndex; }

  public Direction getDirection() { return this.direction; }
  public void setDirection(Direction direction) { this.direction = direction; }

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
    PushActionInfo self = (PushActionInfo) o;
      return 
        Objects.equals(this.rowOrColIndex, self.rowOrColIndex) &&
        Objects.equals(this.direction, self.direction) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)rowOrColIndex, (Object)direction, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PushActionInfo {\n" +   
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