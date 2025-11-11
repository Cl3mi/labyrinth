package labyrinth.contracts.models;
import labyrinth.contracts.models.PlayerColor;
import labyrinth.contracts.models.Coordinates;
import labyrinth.contracts.models.Treasure;
import labyrinth.contracts.models.AchievementType;
import labyrinth.contracts.models.BonusType;
import java.util.Map;
import java.util.Objects;
public class PlayerState {
  private String id;
  private String name;
  private PlayerColor color;
  private Boolean isAdmin;
  private Boolean isReady;
  private Coordinates currentPosition;
  private Coordinates homePosition;
  private Treasure[] treasuresFound;
  private Treasure currentTreasure;
  private Integer remainingTreasureCount;
  private AchievementType[] achievements;
  private BonusType[] availableBonuses;
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

  public Coordinates getCurrentPosition() { return this.currentPosition; }
  public void setCurrentPosition(Coordinates currentPosition) { this.currentPosition = currentPosition; }

  public Coordinates getHomePosition() { return this.homePosition; }
  public void setHomePosition(Coordinates homePosition) { this.homePosition = homePosition; }

  public Treasure[] getTreasuresFound() { return this.treasuresFound; }
  public void setTreasuresFound(Treasure[] treasuresFound) { this.treasuresFound = treasuresFound; }

  public Treasure getCurrentTreasure() { return this.currentTreasure; }
  public void setCurrentTreasure(Treasure currentTreasure) { this.currentTreasure = currentTreasure; }

  public Integer getRemainingTreasureCount() { return this.remainingTreasureCount; }
  public void setRemainingTreasureCount(Integer remainingTreasureCount) { this.remainingTreasureCount = remainingTreasureCount; }

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
        Objects.equals(this.id, self.id) &&
        Objects.equals(this.name, self.name) &&
        Objects.equals(this.color, self.color) &&
        Objects.equals(this.isAdmin, self.isAdmin) &&
        Objects.equals(this.isReady, self.isReady) &&
        Objects.equals(this.currentPosition, self.currentPosition) &&
        Objects.equals(this.homePosition, self.homePosition) &&
        Objects.equals(this.treasuresFound, self.treasuresFound) &&
        Objects.equals(this.currentTreasure, self.currentTreasure) &&
        Objects.equals(this.remainingTreasureCount, self.remainingTreasureCount) &&
        Objects.equals(this.achievements, self.achievements) &&
        Objects.equals(this.availableBonuses, self.availableBonuses) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)id, (Object)name, (Object)color, (Object)isAdmin, (Object)isReady, (Object)currentPosition, (Object)homePosition, (Object)treasuresFound, (Object)currentTreasure, (Object)remainingTreasureCount, (Object)achievements, (Object)availableBonuses, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class PlayerState {\n" +   
      "    id: " + toIndentedString(id) + "\n" +
      "    name: " + toIndentedString(name) + "\n" +
      "    color: " + toIndentedString(color) + "\n" +
      "    isAdmin: " + toIndentedString(isAdmin) + "\n" +
      "    isReady: " + toIndentedString(isReady) + "\n" +
      "    currentPosition: " + toIndentedString(currentPosition) + "\n" +
      "    homePosition: " + toIndentedString(homePosition) + "\n" +
      "    treasuresFound: " + toIndentedString(treasuresFound) + "\n" +
      "    currentTreasure: " + toIndentedString(currentTreasure) + "\n" +
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