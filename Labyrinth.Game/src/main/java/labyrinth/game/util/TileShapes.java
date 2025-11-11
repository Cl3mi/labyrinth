package labyrinth.game.util;

import labyrinth.game.enums.Direction;

import java.util.EnumSet;

public final class TileShapes {

    private TileShapes() {}


    /** A straight vertical tile (UP, DOWN). */
    public static final EnumSet<Direction> SHAPE_STRAIGHT_VERTICAL =
            EnumSet.of(Direction.UP, Direction.DOWN);

    /** A straight horizontal tile (LEFT, RIGHT). */
    public static final EnumSet<Direction> SHAPE_STRAIGHT_HORIZONTAL =
            EnumSet.of(Direction.LEFT, Direction.RIGHT);

    /** A corner tile (UP, RIGHT). */
    public static final EnumSet<Direction> SHAPE_CORNER_UP_RIGHT =
            EnumSet.of(Direction.UP, Direction.RIGHT);

    /** A corner tile (UP, LEFT). */
    public static final EnumSet<Direction> SHAPE_CORNER_UP_LEFT =
            EnumSet.of(Direction.UP, Direction.LEFT);

    /** A corner tile (DOWN, RIGHT). */
    public static final EnumSet<Direction> SHAPE_CORNER_DOWN_RIGHT =
            EnumSet.of(Direction.DOWN, Direction.RIGHT);

    /** A corner tile (DOWN, LEFT). */
    public static final EnumSet<Direction> SHAPE_CORNER_DOWN_LEFT =
            EnumSet.of(Direction.DOWN, Direction.LEFT);


    /** A T-junction tile with connections UP, LEFT, and RIGHT. */
    public static final EnumSet<Direction> SHAPE_T_NO_DOWN =
            EnumSet.of(Direction.UP, Direction.LEFT, Direction.RIGHT);

    /** A T-junction tile with connections DOWN, LEFT, and RIGHT */
    public static final EnumSet<Direction> SHAPE_T_NO_UP =
            EnumSet.of(Direction.DOWN, Direction.LEFT, Direction.RIGHT);

    /** A T-junction tile with connections UP, DOWN, and RIGHT. */
    public static final EnumSet<Direction> SHAPE_T_NO_LEFT =
            EnumSet.of(Direction.UP, Direction.DOWN, Direction.RIGHT);

    /** A T-junction tile with connections UP, DOWN, and LEFT. */
    public static final EnumSet<Direction> SHAPE_T_NO_RIGHT =
            EnumSet.of(Direction.UP, Direction.DOWN, Direction.LEFT);

}