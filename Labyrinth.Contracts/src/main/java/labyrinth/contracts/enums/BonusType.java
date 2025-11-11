package labyrinth.contracts.model;

import com.fasterxml.jackson.annotation.*;
public enum BonusType {
  BEAM((String)"BEAM"), PUSH_FIXED((String)"PUSH_FIXED"), SWAP((String)"SWAP"), PUSH_TWICE((String)"PUSH_TWICE");

  private String value;

  BonusType(String value) {
    this.value = value;
  }

  @JsonValue
  public String getValue() {
    return value;
  }

  @JsonCreator
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