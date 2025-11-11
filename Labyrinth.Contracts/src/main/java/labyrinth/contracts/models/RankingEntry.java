package labyrinth.contracts.models;
import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.*;
public class RankingEntry {
  @JsonProperty("playerId")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String playerId;
  @JsonProperty("rank")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer rank;
  @JsonProperty("score")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Integer score;
  @JsonProperty("stats")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private PlayerGameStats stats;
  @JsonAnySetter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Object> additionalProperties;

  public String getPlayerId() { return this.playerId; }
  public void setPlayerId(String playerId) { this.playerId = playerId; }

  public Integer getRank() { return this.rank; }
  public void setRank(Integer rank) { this.rank = rank; }

  public Integer getScore() { return this.score; }
  public void setScore(Integer score) { this.score = score; }

  public PlayerGameStats getStats() { return this.stats; }
  public void setStats(PlayerGameStats stats) { this.stats = stats; }

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
    RankingEntry self = (RankingEntry) o;
      return 
        Objects.equals(this.playerId, self.playerId) &&
        Objects.equals(this.rank, self.rank) &&
        Objects.equals(this.score, self.score) &&
        Objects.equals(this.stats, self.stats) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)playerId, (Object)rank, (Object)score, (Object)stats, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class RankingEntry {\n" +   
      "    playerId: " + toIndentedString(playerId) + "\n" +
      "    rank: " + toIndentedString(rank) + "\n" +
      "    score: " + toIndentedString(score) + "\n" +
      "    stats: " + toIndentedString(stats) + "\n" +
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