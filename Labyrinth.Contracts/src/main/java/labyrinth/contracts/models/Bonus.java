package labyrinth.contracts.models;
import labyrinth.contracts.models.BonusType;
import java.util.Map;
import java.util.Objects;
public class Bonus {
  private Integer id;
  private BonusType type;
  private Map<String, Object> additionalProperties;

  public Integer getId() { return this.id; }
  public void setId(Integer id) { this.id = id; }

  public BonusType getType() { return this.type; }
  public void setType(BonusType type) { this.type = type; }

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
    Bonus self = (Bonus) o;
      return 
        Objects.equals(this.id, self.id) &&
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)id, (Object)type, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class Bonus {\n" +   
      "    id: " + toIndentedString(id) + "\n" +
      "    type: " + toIndentedString(type) + "\n" +
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