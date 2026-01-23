package labyrinth.client.unit.models.extensions;

import labyrinth.client.models.extensions.TreasureUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("TreasureUtils")
class TreasureUtilsTest {

    @ParameterizedTest(name = "getLocalName({0}) should return {1}")
    @CsvSource({
        "1, Geist",
        "2, Drache",
        "3, Hexe",
        "4, Eule",
        "5, Ratte",
        "6, Käfer",
        "7, Spinne",
        "8, Schlange",
        "9, Fledermaus",
        "10, Krone",
        "11, Schlüssel",
        "12, Schatztruhe",
        "13, Helm",
        "14, Buch",
        "15, Kerze",
        "16, Ring",
        "17, Beutel",
        "18, Totenkopf",
        "19, Karte",
        "20, Schwert",
        "21, Kelch",
        "22, Edelstein",
        "23, Krug",
        "24, Maus",
        "0, Unknown Treasure",
        "25, Unknown Treasure",
        "-1, Unknown Treasure"
    })
    void getLocalName_returnsCorrectName(int id, String expectedName) {
        assertThat(TreasureUtils.getLocalName(id)).isEqualTo(expectedName);
    }
}
