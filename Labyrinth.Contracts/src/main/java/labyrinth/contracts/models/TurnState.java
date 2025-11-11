package labyrinth.contracts.models;


public enum TurnState {
  WAITING_FOR_PUSH((String)"WAITING_FOR_PUSH"), WAITING_FOR_MOVE((String)"WAITING_FOR_MOVE"), WAITING_FOR_BONUS((String)"WAITING_FOR_BONUS");

  private final String value;

  TurnState(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static TurnState fromValue(String value) {
    for (TurnState e : TurnState.values()) {
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