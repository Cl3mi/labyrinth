package labyrinth.contracts.models;
import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
import labyrinth.contracts.enums.Direction;

public class Tile {
  @JsonProperty("entrances")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Direction[] entrances;
  @JsonProperty("treasure")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Treasure treasure;
  @JsonProperty("bonus")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Bonus bonus;
  @JsonProperty("isFixed")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Boolean isFixed;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public Direction[] getEntrances() { return this.entrances; }
  public void setEntrances(Direction[] entrances) { this.entrances = entrances; }

  public Treasure getTreasure() { return this.treasure; }
  public void setTreasure(Treasure treasure) { this.treasure = treasure; }

  public Bonus getBonus() { return this.bonus; }
  public void setBonus(Bonus bonus) { this.bonus = bonus; }

  public Boolean getIsFixed() { return this.isFixed; }
  public void setIsFixed(Boolean isFixed) { this.isFixed = isFixed; }

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
    Tile self = (Tile) o;
      return 
        Objects.equals(this.entrances, self.entrances) &&
        Objects.equals(this.treasure, self.treasure) &&
        Objects.equals(this.bonus, self.bonus) &&
        Objects.equals(this.isFixed, self.isFixed) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)entrances, (Object)treasure, (Object)bonus, (Object)isFixed, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class Tile {\n" +   
      "    entrances: " + toIndentedString(entrances) + "\n" +
      "    treasure: " + toIndentedString(treasure) + "\n" +
      "    bonus: " + toIndentedString(bonus) + "\n" +
      "    isFixed: " + toIndentedString(isFixed) + "\n" +
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