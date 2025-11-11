package labyrinth.contracts.models;


public enum ErrorCode {
  INVALID_MOVE((String)"INVALID_MOVE"), NOT_YOUR_TURN((String)"NOT_YOUR_TURN"), INVALID_PUSH((String)"INVALID_PUSH"), BONUS_NOT_AVAILABLE((String)"BONUS_NOT_AVAILABLE");

  private final String value;

  ErrorCode(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static ErrorCode fromValue(String value) {
    for (ErrorCode e : ErrorCode.values()) {
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