package labyrinth.contracts.models;
import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.BoardSize;
import java.util.Map;
import java.util.Objects;
public class StartGameCommandPayload {
  private CommandType type;
  private BoardSize boardSize;
  private Integer treasureCardCount = 24;
  private Integer totalBonusCount;
  private Integer gameDurationInSeconds = 3600;
  private Integer turnTimeInSeconds = 30;
  private Map<String, Object> additionalProperties;

  public CommandType getType() { return this.type; }
  public void setType(CommandType type) { this.type = type; }

  public BoardSize getBoardSize() { return this.boardSize; }
  public void setBoardSize(BoardSize boardSize) { this.boardSize = boardSize; }

  public Integer getTreasureCardCount() { return this.treasureCardCount; }
  public void setTreasureCardCount(Integer treasureCardCount) { this.treasureCardCount = treasureCardCount; }

  public Integer getTotalBonusCount() { return this.totalBonusCount; }
  public void setTotalBonusCount(Integer totalBonusCount) { this.totalBonusCount = totalBonusCount; }

  public Integer getGameDurationInSeconds() { return this.gameDurationInSeconds; }
  public void setGameDurationInSeconds(Integer gameDurationInSeconds) { this.gameDurationInSeconds = gameDurationInSeconds; }

  public Integer getTurnTimeInSeconds() { return this.turnTimeInSeconds; }
  public void setTurnTimeInSeconds(Integer turnTimeInSeconds) { this.turnTimeInSeconds = turnTimeInSeconds; }

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
    StartGameCommandPayload self = (StartGameCommandPayload) o;
      return
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.boardSize, self.boardSize) &&
        Objects.equals(this.treasureCardCount, self.treasureCardCount) &&
        Objects.equals(this.totalBonusCount, self.totalBonusCount) &&
        Objects.equals(this.gameDurationInSeconds, self.gameDurationInSeconds) &&
        Objects.equals(this.turnTimeInSeconds, self.turnTimeInSeconds) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)boardSize, (Object)treasureCardCount, (Object)totalBonusCount, (Object)gameDurationInSeconds, (Object)turnTimeInSeconds, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class StartGameCommandPayload {\n" +
      "    type: " + toIndentedString(type) + "\n" +
      "    boardSize: " + toIndentedString(boardSize) + "\n" +
      "    treasureCardCount: " + toIndentedString(treasureCardCount) + "\n" +
      "    totalBonusCount: " + toIndentedString(totalBonusCount) + "\n" +
      "    gameDurationInSeconds: " + toIndentedString(gameDurationInSeconds) + "\n" +
      "    turnTimeInSeconds: " + toIndentedString(turnTimeInSeconds) + "\n" +
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