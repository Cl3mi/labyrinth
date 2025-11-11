package labyrinth.contracts.model;

import com.fasterxml.jackson.annotation.*;
public enum AchievementType {
  RUNNER((String)"RUNNER"), PUSHER((String)"PUSHER"), BLOCKER((String)"BLOCKER"), TIME_WASTER((String)"TIME_WASTER"), HATTRICK((String)"HATTRICK");

  private String value;

  AchievementType(String value) {
    this.value = value;
  }

  @JsonValue
  public String getValue() {
    return value;
  }

  @JsonCreator
  public static AchievementType fromValue(String value) {
    for (AchievementType e : AchievementType.values()) {
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