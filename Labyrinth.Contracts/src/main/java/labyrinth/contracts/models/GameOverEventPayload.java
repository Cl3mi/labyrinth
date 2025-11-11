package labyrinth.contracts.models;
import labyrinth.contracts.SlashGame;
import labyrinth.contracts.model.EventType;

import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class GameOverEventPayload implements SlashGame {
  @JsonProperty("type")
  private EventType type;
  @JsonProperty("winnerId")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String winnerId;
  @JsonProperty("ranking")
  private RankingEntry[] ranking;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public String getWinnerId() { return this.winnerId; }
  public void setWinnerId(String winnerId) { this.winnerId = winnerId; }

  public RankingEntry[] getRanking() { return this.ranking; }
  public void setRanking(RankingEntry[] ranking) { this.ranking = ranking; }

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
    GameOverEventPayload self = (GameOverEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.winnerId, self.winnerId) &&
        Objects.equals(this.ranking, self.ranking) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)winnerId, (Object)ranking, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class GameOverEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    winnerId: " + toIndentedString(winnerId) + "\n" +
      "    ranking: " + toIndentedString(ranking) + "\n" +
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