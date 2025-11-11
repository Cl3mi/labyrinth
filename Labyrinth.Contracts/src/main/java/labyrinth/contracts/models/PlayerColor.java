package labyrinth.contracts.models;


public enum PlayerColor {
  RED((String)"RED"), BLUE((String)"BLUE"), GREEN((String)"GREEN"), YELLOW((String)"YELLOW");

  private final String value;

  PlayerColor(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

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