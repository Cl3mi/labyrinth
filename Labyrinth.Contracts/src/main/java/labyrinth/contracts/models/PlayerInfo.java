package labyrinth.contracts.models;
import labyrinth.contracts.models.PlayerColor;
import java.util.Map;
import java.util.Objects;
public class PlayerInfo {
  private String id;
  private String name;
  private PlayerColor color;
  private Boolean isAdmin;
  private Boolean isAiControlled;
  private Map<String, Object> additionalProperties;

  public String getId() { return this.id; }
  public void setId(String id) { this.id = id; }

  public String getName() { return this.name; }
  public void setName(String name) { this.name = name; }

  public PlayerColor getColor() { return this.color; }
  public void setColor(PlayerColor color) { this.color = color; }

  public Boolean getIsAdmin() { return this.isAdmin; }
  public void setIsAdmin(Boolean isAdmin) { this.isAdmin = isAdmin; }

  public Boolean getIsAiControlled() { return this.isAiControlled; }
  public void setIsAiControlled(Boolean isAiControlled) { this.isAiControlled = isAiControlled; }

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
    PlayerInfo self = (PlayerInfo) o;
      return 
        Objects.equals(this.id, self.id) &&
        Objects.equals(this.name, self.name) &&
        Objects.equals(this.color, self.color) &&
        Objects.equals(this.isAdmin, self.isAdmin) &&
        Objects.equals(this.isAiControlled, self.isAiControlled) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)id, (Object)name, (Object)color, (Object)isAdmin, (Object)isAiControlled, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PlayerInfo {\n" +   
      "    id: " + toIndentedString(id) + "\n" +
      "    name: " + toIndentedString(name) + "\n" +
      "    color: " + toIndentedString(color) + "\n" +
      "    isAdmin: " + toIndentedString(isAdmin) + "\n" +
      "    isAiControlled: " + toIndentedString(isAiControlled) + "\n" +
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