package labyrinth.contracts.models;

import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class PlayerGameStats {
  @JsonProperty("stepsTaken")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer stepsTaken;
  @JsonProperty("tilesPushed")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer tilesPushed;
  @JsonProperty("treasuresCollected")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer treasuresCollected;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public Integer getStepsTaken() { return this.stepsTaken; }
  public void setStepsTaken(Integer stepsTaken) { this.stepsTaken = stepsTaken; }

  public Integer getTilesPushed() { return this.tilesPushed; }
  public void setTilesPushed(Integer tilesPushed) { this.tilesPushed = tilesPushed; }

  public Integer getTreasuresCollected() { return this.treasuresCollected; }
  public void setTreasuresCollected(Integer treasuresCollected) { this.treasuresCollected = treasuresCollected; }

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
    PlayerGameStats self = (PlayerGameStats) o;
      return 
        Objects.equals(this.stepsTaken, self.stepsTaken) &&
        Objects.equals(this.tilesPushed, self.tilesPushed) &&
        Objects.equals(this.treasuresCollected, self.treasuresCollected) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)stepsTaken, (Object)tilesPushed, (Object)treasuresCollected, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PlayerGameStats {\n" +   
      "    stepsTaken: " + toIndentedString(stepsTaken) + "\n" +
      "    tilesPushed: " + toIndentedString(tilesPushed) + "\n" +
      "    treasuresCollected: " + toIndentedString(treasuresCollected) + "\n" +
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