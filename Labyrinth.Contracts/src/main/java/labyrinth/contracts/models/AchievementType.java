package labyrinth.contracts.models;


public enum AchievementType {
  RUNNER((String)"RUNNER"), PUSHER((String)"PUSHER"), BLOCKER((String)"BLOCKER"), TIME_WASTER((String)"TIME_WASTER"), HATTRICK((String)"HATTRICK");

  private final String value;

  AchievementType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

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