package labyrinth.contracts.models;


public enum EventType {
  SERVER_INFO((String)"SERVER_INFO"), LOBBY_STATE((String)"LOBBY_STATE"), GAME_STARTED((String)"GAME_STARTED"), GAME_STATE_UPDATE((String)"GAME_STATE_UPDATE"), PLAYER_TURN((String)"PLAYER_TURN"), ACTION_ERROR((String)"ACTION_ERROR"), PLAYER_DISCONNECTED((String)"PLAYER_DISCONNECTED"), ACHIEVEMENT_UNLOCKED((String)"ACHIEVEMENT_UNLOCKED"), GAME_OVER((String)"GAME_OVER"), NEXT_TREASURE((String)"NEXT_TREASURE");

  private final String value;

  EventType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static EventType fromValue(String value) {
    for (EventType e : EventType.values()) {
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