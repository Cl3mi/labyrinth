package labyrinth.client.unit.contracts;

import labyrinth.contracts.models.BoardSize;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.*;

/**
 * Tests for BoardSize configuration with separate width and height.
 */
@DisplayName("BoardSize")
class BoardSizeTest {

    @Nested
    @DisplayName("Default Values")
    class DefaultValueTests {

        @Test
        @DisplayName("defaultRows_is7")
        void defaultRows_is7() {
            BoardSize boardSize = new BoardSize();
            assertThat(boardSize.getRows()).isEqualTo(7);
        }

        @Test
        @DisplayName("defaultCols_is7")
        void defaultCols_is7() {
            BoardSize boardSize = new BoardSize();
            assertThat(boardSize.getCols()).isEqualTo(7);
        }
    }

    @Nested
    @DisplayName("Separate Width and Height")
    class SeparateWidthHeightTests {

        @Test
        @DisplayName("setDifferentWidthAndHeight_storesCorrectly")
        void setDifferentWidthAndHeight_storesCorrectly() {
            BoardSize boardSize = new BoardSize();
            boardSize.setRows(5);
            boardSize.setCols(9);

            assertThat(boardSize.getRows()).isEqualTo(5);
            assertThat(boardSize.getCols()).isEqualTo(9);
        }

        @Test
        @DisplayName("setWidth3Height11_storesCorrectly")
        void setWidth3Height11_storesCorrectly() {
            BoardSize boardSize = new BoardSize();
            boardSize.setRows(11);
            boardSize.setCols(3);

            assertThat(boardSize.getRows()).isEqualTo(11);
            assertThat(boardSize.getCols()).isEqualTo(3);
        }

        @Test
        @DisplayName("rectangularBoard_widthGreaterThanHeight")
        void rectangularBoard_widthGreaterThanHeight() {
            BoardSize boardSize = new BoardSize();
            boardSize.setRows(5);
            boardSize.setCols(11);

            assertThat(boardSize.getCols()).isGreaterThan(boardSize.getRows());
        }

        @Test
        @DisplayName("rectangularBoard_heightGreaterThanWidth")
        void rectangularBoard_heightGreaterThanWidth() {
            BoardSize boardSize = new BoardSize();
            boardSize.setRows(11);
            boardSize.setCols(5);

            assertThat(boardSize.getRows()).isGreaterThan(boardSize.getCols());
        }

        @Test
        @DisplayName("squareBoard_widthEqualsHeight")
        void squareBoard_widthEqualsHeight() {
            BoardSize boardSize = new BoardSize();
            boardSize.setRows(7);
            boardSize.setCols(7);

            assertThat(boardSize.getRows()).isEqualTo(boardSize.getCols());
        }
    }

    @Nested
    @DisplayName("Equality")
    class EqualityTests {

        @Test
        @DisplayName("equalBoardSizes_areEqual")
        void equalBoardSizes_areEqual() {
            BoardSize bs1 = new BoardSize();
            bs1.setRows(5);
            bs1.setCols(7);

            BoardSize bs2 = new BoardSize();
            bs2.setRows(5);
            bs2.setCols(7);

            assertThat(bs1).isEqualTo(bs2);
        }

        @Test
        @DisplayName("differentRows_areNotEqual")
        void differentRows_areNotEqual() {
            BoardSize bs1 = new BoardSize();
            bs1.setRows(5);
            bs1.setCols(7);

            BoardSize bs2 = new BoardSize();
            bs2.setRows(3);
            bs2.setCols(7);

            assertThat(bs1).isNotEqualTo(bs2);
        }

        @Test
        @DisplayName("differentCols_areNotEqual")
        void differentCols_areNotEqual() {
            BoardSize bs1 = new BoardSize();
            bs1.setRows(5);
            bs1.setCols(7);

            BoardSize bs2 = new BoardSize();
            bs2.setRows(5);
            bs2.setCols(9);

            assertThat(bs1).isNotEqualTo(bs2);
        }

        @Test
        @DisplayName("swappedDimensions_areNotEqual")
        void swappedDimensions_areNotEqual() {
            BoardSize bs1 = new BoardSize();
            bs1.setRows(5);
            bs1.setCols(9);

            BoardSize bs2 = new BoardSize();
            bs2.setRows(9);
            bs2.setCols(5);

            assertThat(bs1).isNotEqualTo(bs2);
        }
    }

    @Nested
    @DisplayName("Valid Board Sizes")
    class ValidBoardSizeTests {

        @Test
        @DisplayName("minimumSize_3x3_isValid")
        void minimumSize_3x3_isValid() {
            BoardSize boardSize = new BoardSize();
            boardSize.setRows(3);
            boardSize.setCols(3);

            assertThat(boardSize.getRows()).isEqualTo(3);
            assertThat(boardSize.getCols()).isEqualTo(3);
        }

        @Test
        @DisplayName("maximumSize_11x11_isValid")
        void maximumSize_11x11_isValid() {
            BoardSize boardSize = new BoardSize();
            boardSize.setRows(11);
            boardSize.setCols(11);

            assertThat(boardSize.getRows()).isEqualTo(11);
            assertThat(boardSize.getCols()).isEqualTo(11);
        }

        @Test
        @DisplayName("oddSizes_areAllowed")
        void oddSizes_areAllowed() {
            int[] oddSizes = {3, 5, 7, 9, 11};

            for (int rowSize : oddSizes) {
                for (int colSize : oddSizes) {
                    BoardSize boardSize = new BoardSize();
                    boardSize.setRows(rowSize);
                    boardSize.setCols(colSize);

                    assertThat(boardSize.getRows()).isEqualTo(rowSize);
                    assertThat(boardSize.getCols()).isEqualTo(colSize);
                }
            }
        }
    }

    @Nested
    @DisplayName("HashCode")
    class HashCodeTests {

        @Test
        @DisplayName("equalBoardSizes_haveSameHashCode")
        void equalBoardSizes_haveSameHashCode() {
            BoardSize bs1 = new BoardSize();
            bs1.setRows(5);
            bs1.setCols(9);

            BoardSize bs2 = new BoardSize();
            bs2.setRows(5);
            bs2.setCols(9);

            assertThat(bs1.hashCode()).isEqualTo(bs2.hashCode());
        }
    }
}
