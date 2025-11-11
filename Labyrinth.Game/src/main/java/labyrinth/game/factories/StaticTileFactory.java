package labyrinth.game.factories;

import labyrinth.game.enums.Direction;
import labyrinth.game.models.Tile;

import java.util.EnumSet;

/**
 * A factory class for creating new Tile instances
 * with common 2-way and 3-way shapes.
 */
public final class StaticTileFactory {

    private StaticTileFactory() {}


    // 2-Way Shapes
    private static final EnumSet<Direction> SHAPE_STRAIGHT_VERTICAL =
            EnumSet.of(Direction.UP, Direction.DOWN);
    private static final EnumSet<Direction> SHAPE_STRAIGHT_HORIZONTAL =
            EnumSet.of(Direction.LEFT, Direction.RIGHT);
    private static final EnumSet<Direction> SHAPE_CORNER_UP_RIGHT =
            EnumSet.of(Direction.UP, Direction.RIGHT);
    private static final EnumSet<Direction> SHAPE_CORNER_UP_LEFT =
            EnumSet.of(Direction.UP, Direction.LEFT);
    private static final EnumSet<Direction> SHAPE_CORNER_DOWN_RIGHT =
            EnumSet.of(Direction.DOWN, Direction.RIGHT);
    private static final EnumSet<Direction> SHAPE_CORNER_DOWN_LEFT =
            EnumSet.of(Direction.DOWN, Direction.LEFT);

    // 3-Way Shapes
    private static final EnumSet<Direction> SHAPE_T_NO_DOWN =
            EnumSet.of(Direction.UP, Direction.LEFT, Direction.RIGHT);
    private static final EnumSet<Direction> SHAPE_T_NO_UP =
            EnumSet.of(Direction.DOWN, Direction.LEFT, Direction.RIGHT);
    private static final EnumSet<Direction> SHAPE_T_NO_LEFT =
            EnumSet.of(Direction.UP, Direction.DOWN, Direction.RIGHT);
    private static final EnumSet<Direction> SHAPE_T_NO_RIGHT =
            EnumSet.of(Direction.UP, Direction.DOWN, Direction.LEFT);


    // --- Public Static Factory Methods ---

    // 2-Way Tile Creators
    public static Tile createStraightVertical() {
        return new Tile(SHAPE_STRAIGHT_VERTICAL);
    }

    public static Tile createStraightHorizontal() {
        return new Tile(SHAPE_STRAIGHT_HORIZONTAL);
    }

    public static Tile createCornerUpRight() {
        return new Tile(SHAPE_CORNER_UP_RIGHT);
    }

    public static Tile createCornerUpLeft() {
        return new Tile(SHAPE_CORNER_UP_LEFT);
    }

    public static Tile createCornerDownRight() {
        return new Tile(SHAPE_CORNER_DOWN_RIGHT);
    }

    public static Tile createCornerDownLeft() {
        return new Tile(SHAPE_CORNER_DOWN_LEFT);
    }

    // 3-Way Tile Creators
    public static Tile createTNoDown() {
        return new Tile(SHAPE_T_NO_DOWN);
    }

    public static Tile createTNoUp() {
        return new Tile(SHAPE_T_NO_UP);
    }

    public static Tile createTNoLeft() {
        return new Tile(SHAPE_T_NO_LEFT);
    }

    public static Tile createTNoRight() {
        return new Tile(SHAPE_T_NO_RIGHT);
    }
}