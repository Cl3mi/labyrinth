package labyrinth.contracts.models;


public enum ErrorCode {
  GENERAL((String)"GENERAL"), INVALID_MOVE((String)"INVALID_MOVE"), NOT_YOUR_TURN((String)"NOT_YOUR_TURN"), INVALID_PUSH((String)"INVALID_PUSH"), BONUS_NOT_AVAILABLE((String)"BONUS_NOT_AVAILABLE"), LOBBY_FULL((String)"LOBBY_FULL"), GAME_ALREADY_STARTED((String)"GAME_ALREADY_STARTED"), NOT_ADMIN((String)"NOT_ADMIN"), INVALID_COMMAND((String)"INVALID_COMMAND"), USERNAME_TAKEN((String)"USERNAME_TAKEN");

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