package labyrinth.client.ui.components;

import labyrinth.contracts.models.Direction;

import java.awt.*;
import java.awt.geom.Path2D;

public class ArrowButton {
    public final Rectangle bounds;
    public final Direction direction;
    public final int index;
    public final boolean isRow;
    public final boolean isFixed;
    public final Path2D.Double arrowShape;

    public ArrowButton(Rectangle bounds, Direction direction, int index, boolean isRow, boolean isFixed) {
        this.bounds = bounds;
        this.direction = direction;
        this.index = index;
        this.isRow = isRow;
        this.isFixed = isFixed;
        this.arrowShape = createArrowShape(bounds, direction);
    }

    private Path2D.Double createArrowShape(Rectangle bounds, Direction dir) {
        Path2D.Double arrow = new Path2D.Double();
        int cx = bounds.x + bounds.width / 2;
        int cy = bounds.y + bounds.height / 2;
        int size = Math.min(bounds.width, bounds.height) / 2;

        switch (dir) {
            case LEFT -> {
                arrow.moveTo(cx + size / 2.0, cy - size / 2.0);
                arrow.lineTo(cx - size / 2.0, cy);
                arrow.lineTo(cx + size / 2.0, cy + size / 2.0);
            }
            case RIGHT -> {
                arrow.moveTo(cx - size / 2.0, cy - size / 2.0);
                arrow.lineTo(cx + size / 2.0, cy);
                arrow.lineTo(cx - size / 2.0, cy + size / 2.0);
            }
            case UP -> {
                arrow.moveTo(cx - size / 2.0, cy + size / 2.0);
                arrow.lineTo(cx, cy - size / 2.0);
                arrow.lineTo(cx + size / 2.0, cy + size / 2.0);
            }
            case DOWN -> {
                arrow.moveTo(cx - size / 2.0, cy - size / 2.0);
                arrow.lineTo(cx, cy + size / 2.0);
                arrow.lineTo(cx + size / 2.0, cy - size / 2.0);
            }
        }
        arrow.closePath();
        return arrow;
    }

    public boolean contains(Point p) {
        return bounds.contains(p);
    }
}
