package labyrinth.contracts.models;
import labyrinth.contracts.models.SlashGame;
import labyrinth.contracts.models.EventType;
import labyrinth.contracts.models.AchievementType;
import java.util.Map;
import java.util.Objects;
public class AchievementUnlockedEventPayload implements SlashGame {
  private EventType type;
  private String playerId;
  private AchievementType achievement;
  private Map<String, Object> additionalProperties;

  public EventType getType() { return this.type; }
  public void setType(EventType type) { this.type = type; }

  public String getPlayerId() { return this.playerId; }
  public void setPlayerId(String playerId) { this.playerId = playerId; }

  public AchievementType getAchievement() { return this.achievement; }
  public void setAchievement(AchievementType achievement) { this.achievement = achievement; }

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
    AchievementUnlockedEventPayload self = (AchievementUnlockedEventPayload) o;
      return 
        Objects.equals(this.type, self.type) &&
        Objects.equals(this.playerId, self.playerId) &&
        Objects.equals(this.achievement, self.achievement) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)type, (Object)playerId, (Object)achievement, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class AchievementUnlockedEventPayload {\n" +   
      "    type: " + toIndentedString(type) + "\n" +
      "    playerId: " + toIndentedString(playerId) + "\n" +
      "    achievement: " + toIndentedString(achievement) + "\n" +
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