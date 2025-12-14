package labyrinth.contracts.models;

import java.util.Map;
import java.util.Objects;
public class Coordinates {
  private int row;
  private int column;
  private Map<String, Object> additionalProperties;

  public int getRow() { return this.row; }
  public void setRow(int row) { this.row = row; }

  public int getColumn() { return this.column; }
  public void setColumn(int column) { this.column = column; }

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
        Objects.equals(this.row, self.row) &&
        Objects.equals(this.column, self.column) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)row, (Object)column, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class Coordinates {\n" +   
      "    row: " + toIndentedString(row) + "\n" +
      "    column: " + toIndentedString(column) + "\n" +
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