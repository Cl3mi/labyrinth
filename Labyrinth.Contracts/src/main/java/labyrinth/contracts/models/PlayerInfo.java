package labyrinth.contracts.models;
import labyrinth.contracts.model.PlayerColor;
import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class PlayerInfo {
  @JsonProperty("id")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String id;
  @JsonProperty("name")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String name;
  @JsonProperty("color")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private PlayerColor color;
  @JsonProperty("isAdmin")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Boolean isAdmin;
  @JsonProperty("isReady")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Boolean isReady;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public String getId() { return this.id; }
  public void setId(String id) { this.id = id; }

  public String getName() { return this.name; }
  public void setName(String name) { this.name = name; }

  public PlayerColor getColor() { return this.color; }
  public void setColor(PlayerColor color) { this.color = color; }

  public Boolean getIsAdmin() { return this.isAdmin; }
  public void setIsAdmin(Boolean isAdmin) { this.isAdmin = isAdmin; }

  public Boolean getIsReady() { return this.isReady; }
  public void setIsReady(Boolean isReady) { this.isReady = isReady; }

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
    PlayerInfo self = (PlayerInfo) o;
      return 
        Objects.equals(this.id, self.id) &&
        Objects.equals(this.name, self.name) &&
        Objects.equals(this.color, self.color) &&
        Objects.equals(this.isAdmin, self.isAdmin) &&
        Objects.equals(this.isReady, self.isReady) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)id, (Object)name, (Object)color, (Object)isAdmin, (Object)isReady, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PlayerInfo {\n" +   
      "    id: " + toIndentedString(id) + "\n" +
      "    name: " + toIndentedString(name) + "\n" +
      "    color: " + toIndentedString(color) + "\n" +
      "    isAdmin: " + toIndentedString(isAdmin) + "\n" +
      "    isReady: " + toIndentedString(isReady) + "\n" +
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