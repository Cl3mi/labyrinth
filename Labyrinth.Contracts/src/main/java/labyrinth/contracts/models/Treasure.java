package labyrinth.contracts.models;

import java.util.Map;
import java.util.Objects;
public class Treasure {
  private int id;
  private String name;
  private Map<String, Object> additionalProperties;

  public int getId() { return this.id; }
  public void setId(int id) { this.id = id; }

  public String getName() { return this.name; }
  public void setName(String name) { this.name = name; }

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
    Treasure self = (Treasure) o;
      return 
        Objects.equals(this.id, self.id) &&
        Objects.equals(this.name, self.name) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)id, (Object)name, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class Treasure {\n" +   
      "    id: " + toIndentedString(id) + "\n" +
      "    name: " + toIndentedString(name) + "\n" +
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