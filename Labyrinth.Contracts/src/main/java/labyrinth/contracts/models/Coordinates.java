package labyrinth.contracts.models;

import java.util.Map;
import java.util.Objects;
public class Coordinates {
  private int x;
  private int y;
  private Map<String, Object> additionalProperties;

  public int getX() { return this.x; }
  public void setX(int x) { this.x = x; }

  public int getY() { return this.y; }
  public void setY(int y) { this.y = y; }

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
    Coordinates self = (Coordinates) o;
      return 
        Objects.equals(this.x, self.x) &&
        Objects.equals(this.y, self.y) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)x, (Object)y, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class Coordinates {\n" +   
      "    x: " + toIndentedString(x) + "\n" +
      "    y: " + toIndentedString(y) + "\n" +
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