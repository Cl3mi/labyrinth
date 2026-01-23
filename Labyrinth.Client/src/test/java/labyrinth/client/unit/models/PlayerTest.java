package labyrinth.client.unit.models;

import labyrinth.client.models.Player;
import labyrinth.client.models.Position;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.PlayerColor;
import labyrinth.contracts.models.Treasure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("Player")
class PlayerTest {

    private static final String PLAYER_ID = "player-123";
    private static final String PLAYER_NAME = "TestPlayer";

    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {

        @Test
        @DisplayName("constructor_validIdAndName_createsPlayer")
        void constructor_validIdAndName_createsPlayer() {
            // Given
            String id = PLAYER_ID;
            String name = PLAYER_NAME;

            // When
            Player player = new Player(id, name);

            // Then
            assertThat(player.getId()).isEqualTo(id);
            assertThat(player.getName()).isEqualTo(name);
        }

        @Test
        @DisplayName("constructor_nullId_throwsException")
        void constructor_nullId_throwsException() {
            // Given
            String name = PLAYER_NAME;

            // When/Then
            assertThatThrownBy(() -> new Player(null, name))
                    .isInstanceOf(NullPointerException.class)
                    .hasMessageContaining("id");
        }

        @Test
        @DisplayName("constructor_nullName_throwsException")
        void constructor_nullName_throwsException() {
            // Given
            String id = PLAYER_ID;

            // When/Then
            assertThatThrownBy(() -> new Player(id, null))
                    .isInstanceOf(NullPointerException.class)
                    .hasMessageContaining("name");
        }

        @Test
        @DisplayName("constructor_emptyId_createsPlayer")
        void constructor_emptyId_createsPlayer() {
            // Given
            String id = "";
            String name = PLAYER_NAME;

            // When
            Player player = new Player(id, name);

            // Then
            assertThat(player.getId()).isEmpty();
        }

        @Test
        @DisplayName("constructor_emptyName_createsPlayer")
        void constructor_emptyName_createsPlayer() {
            // Given
            String id = PLAYER_ID;
            String name = "";

            // When
            Player player = new Player(id, name);

            // Then
            assertThat(player.getName()).isEmpty();
        }
    }

    @Nested
    @DisplayName("Position")
    class PositionTests {

        private Player player;

        @BeforeEach
        void setUp() {
            player = new Player(PLAYER_ID, PLAYER_NAME);
        }

        @Test
        @DisplayName("getCurrentPosition_initially_returnsNull")
        void getCurrentPosition_initially_returnsNull() {
            // When/Then
            assertThat(player.getCurrentPosition()).isNull();
        }

        @Test
        @DisplayName("setCurrentPosition_validPosition_updatesPosition")
        void setCurrentPosition_validPosition_updatesPosition() {
            // Given
            Position position = new Position(3, 4);

            // When
            player.setCurrentPosition(position);

            // Then
            assertThat(player.getCurrentPosition()).isEqualTo(position);
            assertThat(player.getCurrentPosition().getRow()).isEqualTo(3);
            assertThat(player.getCurrentPosition().getColumn()).isEqualTo(4);
        }

        @Test
        @DisplayName("setCurrentPosition_null_setsToNull")
        void setCurrentPosition_null_setsToNull() {
            // Given
            player.setCurrentPosition(new Position(1, 1));

            // When
            player.setCurrentPosition(null);

            // Then
            assertThat(player.getCurrentPosition()).isNull();
        }

        @Test
        @DisplayName("getHomePosition_initially_returnsNull")
        void getHomePosition_initially_returnsNull() {
            // When/Then
            assertThat(player.getHomePosition()).isNull();
        }

        @Test
        @DisplayName("setHomePosition_validPosition_updatesHomePosition")
        void setHomePosition_validPosition_updatesHomePosition() {
            // Given
            Position homePosition = new Position(0, 0);

            // When
            player.setHomePosition(homePosition);

            // Then
            assertThat(player.getHomePosition()).isEqualTo(homePosition);
        }
    }

    @Nested
    @DisplayName("Color")
    class ColorTests {

        private Player player;

        @BeforeEach
        void setUp() {
            player = new Player(PLAYER_ID, PLAYER_NAME);
        }

        @Test
        @DisplayName("getColor_initially_returnsNull")
        void getColor_initially_returnsNull() {
            // When/Then
            assertThat(player.getColor()).isNull();
        }

        @Test
        @DisplayName("setColor_validColor_updatesColor")
        void setColor_validColor_updatesColor() {
            // Given
            PlayerColor color = PlayerColor.RED;

            // When
            player.setColor(color);

            // Then
            assertThat(player.getColor()).isEqualTo(PlayerColor.RED);
        }

        @Test
        @DisplayName("setColor_allColors_worksCorrectly")
        void setColor_allColors_worksCorrectly() {
            // Test all player colors
            for (PlayerColor color : PlayerColor.values()) {
                // When
                player.setColor(color);

                // Then
                assertThat(player.getColor()).isEqualTo(color);
            }
        }
    }

    @Nested
    @DisplayName("Treasures")
    class TreasureTests {

        private Player player;

        @BeforeEach
        void setUp() {
            player = new Player(PLAYER_ID, PLAYER_NAME);
        }

        @Test
        @DisplayName("getTreasuresFound_initially_isEmpty")
        void getTreasuresFound_initially_isEmpty() {
            // When/Then
            assertThat(player.getTreasuresFound()).isEmpty();
        }

        @Test
        @DisplayName("getTreasuresFound_addTreasure_containsTreasure")
        void getTreasuresFound_addTreasure_containsTreasure() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(1);
            treasure.setName("Gold");

            // When
            player.getTreasuresFound().add(treasure);

            // Then
            assertThat(player.getTreasuresFound()).hasSize(1);
            assertThat(player.getTreasuresFound().get(0).getId()).isEqualTo(1);
        }

        @Test
        @DisplayName("getRemainingTreasureCount_initially_returnsZero")
        void getRemainingTreasureCount_initially_returnsZero() {
            // When/Then
            assertThat(player.getRemainingTreasureCount()).isZero();
        }

        @Test
        @DisplayName("setRemainingTreasureCount_validCount_updatesCount")
        void setRemainingTreasureCount_validCount_updatesCount() {
            // Given
            int count = 5;

            // When
            player.setRemainingTreasureCount(count);

            // Then
            assertThat(player.getRemainingTreasureCount()).isEqualTo(count);
        }

        @Test
        @DisplayName("getCurrentTargetTreasure_initially_returnsNull")
        void getCurrentTargetTreasure_initially_returnsNull() {
            // When/Then
            assertThat(player.getCurrentTargetTreasure()).isNull();
        }

        @Test
        @DisplayName("setCurrentTargetTreasure_validTreasure_updatesTreasure")
        void setCurrentTargetTreasure_validTreasure_updatesTreasure() {
            // Given
            Treasure treasure = new Treasure();
            treasure.setId(42);
            treasure.setName("Diamond");

            // When
            player.setCurrentTargetTreasure(treasure);

            // Then
            assertThat(player.getCurrentTargetTreasure()).isEqualTo(treasure);
            assertThat(player.getCurrentTargetTreasure().getId()).isEqualTo(42);
        }
    }

    @Nested
    @DisplayName("Status Flags")
    class StatusFlagTests {

        private Player player;

        @BeforeEach
        void setUp() {
            player = new Player(PLAYER_ID, PLAYER_NAME);
        }

        @Test
        @DisplayName("isConnected_initially_returnsTrue")
        void isConnected_initially_returnsTrue() {
            // When/Then
            assertThat(player.isConnected()).isTrue();
        }

        @Test
        @DisplayName("setConnected_false_updatesFlag")
        void setConnected_false_updatesFlag() {
            // When
            player.setConnected(false);

            // Then
            assertThat(player.isConnected()).isFalse();
        }

        @Test
        @DisplayName("isAdmin_initially_returnsFalse")
        void isAdmin_initially_returnsFalse() {
            // When/Then
            assertThat(player.isAdmin()).isFalse();
        }

        @Test
        @DisplayName("setAdmin_true_updatesFlag")
        void setAdmin_true_updatesFlag() {
            // When
            player.setAdmin(true);

            // Then
            assertThat(player.isAdmin()).isTrue();
        }

        @Test
        @DisplayName("isAiControlled_initially_returnsFalse")
        void isAiControlled_initially_returnsFalse() {
            // When/Then
            assertThat(player.isAiControlled()).isFalse();
        }

        @Test
        @DisplayName("setAiControlled_true_updatesFlag")
        void setAiControlled_true_updatesFlag() {
            // When
            player.setAiControlled(true);

            // Then
            assertThat(player.isAiControlled()).isTrue();
        }
    }

    @Nested
    @DisplayName("Bonuses")
    class BonusTests {

        private Player player;

        @BeforeEach
        void setUp() {
            player = new Player(PLAYER_ID, PLAYER_NAME);
        }

        @Test
        @DisplayName("getAvailableBonuses_initially_isEmpty")
        void getAvailableBonuses_initially_isEmpty() {
            // When/Then
            assertThat(player.getAvailableBonuses()).isEmpty();
        }

        @Test
        @DisplayName("setAvailableBonuses_validList_updatesBonuses")
        void setAvailableBonuses_validList_updatesBonuses() {
            // Given
            List<BonusType> bonuses = Arrays.asList(BonusType.BEAM, BonusType.SWAP);

            // When
            player.setAvailableBonuses(bonuses);

            // Then
            assertThat(player.getAvailableBonuses())
                    .hasSize(2)
                    .contains(BonusType.BEAM, BonusType.SWAP);
        }

        @Test
        @DisplayName("setAvailableBonuses_null_clearsListOnly")
        void setAvailableBonuses_null_clearsListOnly() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.BEAM));

            // When
            player.setAvailableBonuses(null);

            // Then
            assertThat(player.getAvailableBonuses()).isEmpty();
        }

        @Test
        @DisplayName("setAvailableBonuses_emptyList_clearsExisting")
        void setAvailableBonuses_emptyList_clearsExisting() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.BEAM, BonusType.SWAP));

            // When
            player.setAvailableBonuses(Collections.emptyList());

            // Then
            assertThat(player.getAvailableBonuses()).isEmpty();
        }

        @Test
        @DisplayName("setAvailableBonuses_allBonusTypes_worksCorrectly")
        void setAvailableBonuses_allBonusTypes_worksCorrectly() {
            // Given
            List<BonusType> allBonuses = Arrays.asList(
                    BonusType.BEAM,
                    BonusType.SWAP,
                    BonusType.PUSH_FIXED,
                    BonusType.PUSH_TWICE
            );

            // When
            player.setAvailableBonuses(allBonuses);

            // Then
            assertThat(player.getAvailableBonuses())
                    .hasSize(4)
                    .containsExactlyInAnyOrderElementsOf(allBonuses);
        }

        @Test
        @DisplayName("setAvailableBonuses_replacesExisting")
        void setAvailableBonuses_replacesExisting() {
            // Given
            player.setAvailableBonuses(Arrays.asList(BonusType.BEAM));

            // When
            player.setAvailableBonuses(Arrays.asList(BonusType.SWAP, BonusType.PUSH_TWICE));

            // Then
            assertThat(player.getAvailableBonuses())
                    .hasSize(2)
                    .contains(BonusType.SWAP, BonusType.PUSH_TWICE)
                    .doesNotContain(BonusType.BEAM);
        }
    }

    @Nested
    @DisplayName("ReachableTiles")
    class ReachableTilesTests {

        private Player player;

        @BeforeEach
        void setUp() {
            player = new Player(PLAYER_ID, PLAYER_NAME);
        }

        @Test
        @DisplayName("getReachableTiles_initially_isEmpty")
        void getReachableTiles_initially_isEmpty() {
            // When/Then
            assertThat(player.getReachableTiles()).isEmpty();
        }
    }

    @Nested
    @DisplayName("ToString")
    class ToStringTests {

        @Test
        @DisplayName("toString_basicPlayer_containsIdAndName")
        void toString_basicPlayer_containsIdAndName() {
            // Given
            Player player = new Player(PLAYER_ID, PLAYER_NAME);

            // When
            String result = player.toString();

            // Then
            assertThat(result)
                    .contains(PLAYER_ID)
                    .contains(PLAYER_NAME);
        }

        @Test
        @DisplayName("toString_playerWithPosition_containsPosition")
        void toString_playerWithPosition_containsPosition() {
            // Given
            Player player = new Player(PLAYER_ID, PLAYER_NAME);
            player.setCurrentPosition(new Position(2, 3));

            // When
            String result = player.toString();

            // Then
            assertThat(result).contains("currentPosition");
        }
    }

    @Nested
    @DisplayName("EdgeCases")
    class EdgeCaseTests {

        @Test
        @DisplayName("player_withSpecialCharactersInName_createsSuccessfully")
        void player_withSpecialCharactersInName_createsSuccessfully() {
            // Given
            String specialName = "Player@123!#$%";
            String id = "id-special";

            // When
            Player player = new Player(id, specialName);

            // Then
            assertThat(player.getName()).isEqualTo(specialName);
        }

        @Test
        @DisplayName("player_withUnicodeInName_createsSuccessfully")
        void player_withUnicodeInName_createsSuccessfully() {
            // Given
            String unicodeName = "Spieler_äöü_中文";
            String id = "id-unicode";

            // When
            Player player = new Player(id, unicodeName);

            // Then
            assertThat(player.getName()).isEqualTo(unicodeName);
        }

        @Test
        @DisplayName("player_withLongName_createsSuccessfully")
        void player_withLongName_createsSuccessfully() {
            // Given
            String longName = "A".repeat(1000);
            String id = "id-long";

            // When
            Player player = new Player(id, longName);

            // Then
            assertThat(player.getName()).hasSize(1000);
        }

        @Test
        @DisplayName("player_multiplePropertyChanges_maintainsConsistency")
        void player_multiplePropertyChanges_maintainsConsistency() {
            // Given
            Player player = new Player(PLAYER_ID, PLAYER_NAME);

            // When - make multiple changes
            player.setCurrentPosition(new Position(1, 2));
            player.setHomePosition(new Position(0, 0));
            player.setColor(PlayerColor.BLUE);
            player.setAdmin(true);
            player.setAiControlled(false);
            player.setConnected(true);
            player.setRemainingTreasureCount(3);
            player.setAvailableBonuses(Arrays.asList(BonusType.BEAM));

            // Then - verify all changes are consistent
            assertThat(player.getCurrentPosition()).isEqualTo(new Position(1, 2));
            assertThat(player.getHomePosition()).isEqualTo(new Position(0, 0));
            assertThat(player.getColor()).isEqualTo(PlayerColor.BLUE);
            assertThat(player.isAdmin()).isTrue();
            assertThat(player.isAiControlled()).isFalse();
            assertThat(player.isConnected()).isTrue();
            assertThat(player.getRemainingTreasureCount()).isEqualTo(3);
            assertThat(player.getAvailableBonuses()).containsExactly(BonusType.BEAM);
        }
    }
}
