package labyrinth.contracts.models;


public enum BonusType {
  BEAM((String)"BEAM"), PUSH_FIXED((String)"PUSH_FIXED"), SWAP((String)"SWAP"), PUSH_TWICE((String)"PUSH_TWICE");

  private final String value;

  BonusType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static BonusType fromValue(String value) {
    for (BonusType e : BonusType.values()) {
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