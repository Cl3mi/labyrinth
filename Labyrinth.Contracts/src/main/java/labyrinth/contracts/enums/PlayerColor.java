package labyrinth.contracts.model;

import com.fasterxml.jackson.annotation.*;
public enum PlayerColor {
  RED((String)"RED"), BLUE((String)"BLUE"), GREEN((String)"GREEN"), YELLOW((String)"YELLOW");

  private String value;

  PlayerColor(String value) {
    this.value = value;
  }

  @JsonValue
  public String getValue() {
    return value;
  }

  @JsonCreator
  public static PlayerColor fromValue(String value) {
    for (PlayerColor e : PlayerColor.values()) {
      if (e.value.equals(value)) {
        return e;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }
}