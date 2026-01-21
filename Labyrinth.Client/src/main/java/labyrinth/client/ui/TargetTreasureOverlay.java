package labyrinth.client.ui;

import javax.swing.*;
import java.awt.*;

/**
 * Lightweight overlay panel for the animated target treasure highlight.
 * Runs its own 60fps animation timer independently from the board.
 * This panel is transparent and only draws the pulsing highlight.
 */
public class TargetTreasureOverlay extends JPanel {

    // State
    private Rectangle targetTileBounds = null;
    private boolean hasTarget = false;

    // Animation
    private final Timer animationTimer;
    private static final int ANIMATION_INTERVAL = 16; // ~60fps

    public TargetTreasureOverlay() {
        setOpaque(false); // Transparent background
        setLayout(null);

        // Animation timer - only runs when there's a target to highlight
        animationTimer = new Timer(ANIMATION_INTERVAL, e -> {
            if (hasTarget && targetTileBounds != null) {
                repaint();
            }
        });
    }

    /**
     * Set the bounds of the target treasure tile on the board.
     * Pass null to clear the highlight.
     */
    public void setTargetTileBounds(Rectangle bounds) {
        boolean hadTarget = hasTarget;
        this.targetTileBounds = bounds;
        this.hasTarget = (bounds != null);

        if (hasTarget && !hadTarget) {
            animationTimer.start();
            repaint();
        } else if (!hasTarget && hadTarget) {
            animationTimer.stop();
            repaint();
        } else if (hasTarget) {
            repaint();
        }
    }

    public void clearTarget() {
        setTargetTileBounds(null);
    }

    public void dispose() {
        animationTimer.stop();
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        if (!hasTarget || targetTileBounds == null) {
            return;
        }

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            drawTargetHighlight(g2);
        } finally {
            g2.dispose();
        }
    }

    private void drawTargetHighlight(Graphics2D g2) {
        long time = System.currentTimeMillis();
        int highlightAlpha = 150 + (int) (50 * Math.sin(time / 400.0));

        int tileX = targetTileBounds.x;
        int tileY = targetTileBounds.y;
        int size = targetTileBounds.width;

        // Outer golden border
        g2.setColor(new Color(255, 215, 0, highlightAlpha));
        g2.setStroke(new BasicStroke(6));
        g2.drawRoundRect(tileX - 3, tileY - 3, size + 6, size + 6, 12, 12);

        // Thicker inner border
        g2.setColor(new Color(255, 255, 0, highlightAlpha + 50));
        g2.setStroke(new BasicStroke(3));
        g2.drawRoundRect(tileX - 6, tileY - 6, size + 12, size + 12, 15, 15);
    }
}
