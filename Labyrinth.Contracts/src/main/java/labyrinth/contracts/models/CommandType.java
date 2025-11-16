package labyrinth.contracts.models;


public enum CommandType {
  CONNECT((String)"CONNECT"), DISCONNECT((String)"DISCONNECT"), START_GAME((String)"START_GAME"), PUSH_TILE((String)"PUSH_TILE"), MOVE_PAWN((String)"MOVE_PAWN"), TOGGLE_AI((String)"TOGGLE_AI"), USE_BEAM((String)"USE_BEAM"), USE_SWAP((String)"USE_SWAP"), USE_PUSH_FIXED((String)"USE_PUSH_FIXED"), USE_PUSH_TWICE((String)"USE_PUSH_TWICE");

  private final String value;

  CommandType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static CommandType fromValue(String value) {
    for (CommandType e : CommandType.values()) {
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