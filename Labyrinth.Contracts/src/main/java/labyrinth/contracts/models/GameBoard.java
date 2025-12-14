package labyrinth.contracts.models;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.PushActionInfo;
import java.util.Map;
import java.util.Objects;
public class GameBoard {
  private Integer rows;
  private Integer cols;
  private Tile[][] tiles;
  private Tile spareTile;
  private PushActionInfo lastPush;
  private Map<String, Object> additionalProperties;

  public Integer getRows() { return this.rows; }
  public void setRows(Integer rows) { this.rows = rows; }

  public Integer getCols() { return this.cols; }
  public void setCols(Integer cols) { this.cols = cols; }

  public Tile[][] getTiles() { return this.tiles; }
  public void setTiles(Tile[][] tiles) { this.tiles = tiles; }

  public Tile getSpareTile() { return this.spareTile; }
  public void setSpareTile(Tile spareTile) { this.spareTile = spareTile; }

  public PushActionInfo getLastPush() { return this.lastPush; }
  public void setLastPush(PushActionInfo lastPush) { this.lastPush = lastPush; }

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
    GameBoard self = (GameBoard) o;
      return 
        Objects.equals(this.rows, self.rows) &&
        Objects.equals(this.cols, self.cols) &&
        Objects.equals(this.tiles, self.tiles) &&
        Objects.equals(this.spareTile, self.spareTile) &&
        Objects.equals(this.lastPush, self.lastPush) &&
        Objects.equals(this.additionalProperties, self.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash((Object)rows, (Object)cols, (Object)tiles, (Object)spareTile, (Object)lastPush, (Object)additionalProperties);
  }

  @Override
  public String toString() {
    return "class GameBoard {\n" +   
      "    rows: " + toIndentedString(rows) + "\n" +
      "    cols: " + toIndentedString(cols) + "\n" +
      "    tiles: " + toIndentedString(tiles) + "\n" +
      "    spareTile: " + toIndentedString(spareTile) + "\n" +
      "    lastPush: " + toIndentedString(lastPush) + "\n" +
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