package labyrinth.contracts.models;


public enum Direction {
  UP((String)"UP"), DOWN((String)"DOWN"), LEFT((String)"LEFT"), RIGHT((String)"RIGHT");

  private final String value;

  Direction(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

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