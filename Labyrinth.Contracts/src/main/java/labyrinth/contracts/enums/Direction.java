package labyrinth.contracts.enums;

import com.fasterxml.jackson.annotation.*;
public enum Direction {
  UP((String)"UP"), DOWN((String)"DOWN"), LEFT((String)"LEFT"), RIGHT((String)"RIGHT");

  private String value;

  Direction(String value) {
    this.value = value;
  }

  @JsonValue
  public String getValue() {
    return value;
  }

  @JsonCreator
  public static Direction fromValue(String value) {
    for (Direction e : Direction.values()) {
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