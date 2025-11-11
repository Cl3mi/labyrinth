package labyrinth.contracts.models;

import java.util.Map;
import java.util.Objects;
public class BoardSize {
  private int rows = 7;
  private int cols = 7;
  private Map<String, Object> additionalProperties;

  public int getRows() { return this.rows; }
  public void setRows(int rows) { this.rows = rows; }

  public int getCols() { return this.cols; }
  public void setCols(int cols) { this.cols = cols; }

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
    BoardSize self = (BoardSize) o;
      return 
        Objects.equals(this.rows, self.rows) &&
        Objects.equals(this.cols, self.cols) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)rows, (Object)cols, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class BoardSize {\n" +   
      "    rows: " + toIndentedString(rows) + "\n" +
      "    cols: " + toIndentedString(cols) + "\n" +
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