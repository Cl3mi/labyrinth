package labyrinth.contracts.models;
import labyrinth.contracts.models.PlayerInfo;
import labyrinth.contracts.models.Coordinates;
import labyrinth.contracts.models.Treasure;
import labyrinth.contracts.models.AchievementType;
import labyrinth.contracts.models.BonusType;
import java.util.Map;
import java.util.Objects;
public class PlayerState {
  private PlayerInfo playerInfo;
  private Coordinates currentPosition;
  private Coordinates homePosition;
  private Treasure[] treasuresFound;
  private Integer remainingTreasureCount;
  private Treasure currentTreasureCard;  // The current treasure the player needs to find
  private AchievementType[] achievements;
  private BonusType[] availableBonuses;
  private Map<String, Object> additionalProperties;

  public PlayerInfo getPlayerInfo() { return this.playerInfo; }
  public void setPlayerInfo(PlayerInfo playerInfo) { this.playerInfo = playerInfo; }

  public Coordinates getCurrentPosition() { return this.currentPosition; }
  public void setCurrentPosition(Coordinates currentPosition) { this.currentPosition = currentPosition; }

  public Coordinates getHomePosition() { return this.homePosition; }
  public void setHomePosition(Coordinates homePosition) { this.homePosition = homePosition; }

  public Treasure[] getTreasuresFound() { return this.treasuresFound; }
  public void setTreasuresFound(Treasure[] treasuresFound) { this.treasuresFound = treasuresFound; }

  public Integer getRemainingTreasureCount() { return this.remainingTreasureCount; }
  public void setRemainingTreasureCount(Integer remainingTreasureCount) { this.remainingTreasureCount = remainingTreasureCount; }

  public Treasure getCurrentTreasureCard() { return this.currentTreasureCard; }
  public void setCurrentTreasureCard(Treasure currentTreasureCard) { this.currentTreasureCard = currentTreasureCard; }

  public AchievementType[] getAchievements() { return this.achievements; }
  public void setAchievements(AchievementType[] achievements) { this.achievements = achievements; }

  public BonusType[] getAvailableBonuses() { return this.availableBonuses; }
  public void setAvailableBonuses(BonusType[] availableBonuses) { this.availableBonuses = availableBonuses; }

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
    PlayerState self = (PlayerState) o;
      return 
        Objects.equals(this.playerInfo, self.playerInfo) &&
        Objects.equals(this.currentPosition, self.currentPosition) &&
        Objects.equals(this.homePosition, self.homePosition) &&
        Objects.equals(this.treasuresFound, self.treasuresFound) &&
        Objects.equals(this.remainingTreasureCount, self.remainingTreasureCount) &&
        Objects.equals(this.achievements, self.achievements) &&
        Objects.equals(this.availableBonuses, self.availableBonuses) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)playerInfo, (Object)currentPosition, (Object)homePosition, (Object)treasuresFound, (Object)remainingTreasureCount, (Object)achievements, (Object)availableBonuses, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PlayerState {\n" +   
      "    playerInfo: " + toIndentedString(playerInfo) + "\n" +
      "    currentPosition: " + toIndentedString(currentPosition) + "\n" +
      "    homePosition: " + toIndentedString(homePosition) + "\n" +
      "    treasuresFound: " + toIndentedString(treasuresFound) + "\n" +
      "    remainingTreasureCount: " + toIndentedString(remainingTreasureCount) + "\n" +
      "    achievements: " + toIndentedString(achievements) + "\n" +
      "    availableBonuses: " + toIndentedString(availableBonuses) + "\n" +
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