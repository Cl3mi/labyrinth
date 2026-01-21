package labyrinth.client.ui;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Lightweight overlay panel for animated turn indicators.
 * Runs its own 60fps animation timer independently from the board.
 * This panel is transparent and only draws the animated elements.
 */
public class TurnIndicatorOverlay extends JPanel {

    // Font cache
    private static final Map<String, Font> fontCache = new HashMap<>();
    private static final Font FONT_SANSSERIF_BOLD_12 = new Font("SansSerif", Font.BOLD, 12);

    private static Font getCachedFont(String family, int style, int size) {
        String key = family + "_" + style + "_" + size;
        return fontCache.computeIfAbsent(key, k -> new Font(family, style, size));
    }

    // State
    private boolean isLocalPlayerTurn = false;
    private Rectangle boardBounds = null; // The bounds of the game board for the glow effect

    // Animation
    private final Timer animationTimer;
    private static final int ANIMATION_INTERVAL = 16; // ~60fps

    public TurnIndicatorOverlay() {
        setOpaque(false); // Transparent background
        setLayout(null);

        // Animation timer - only runs when it's our turn
        animationTimer = new Timer(ANIMATION_INTERVAL, e -> {
            if (isLocalPlayerTurn) {
                repaint();
            }
        });
        // Don't start until needed
    }

    public void setLocalPlayerTurn(boolean isLocalPlayerTurn) {
        boolean wasOurTurn = this.isLocalPlayerTurn;
        this.isLocalPlayerTurn = isLocalPlayerTurn;

        if (isLocalPlayerTurn && !wasOurTurn) {
            // Start animation
            animationTimer.start();
            repaint();
        } else if (!isLocalPlayerTurn && wasOurTurn) {
            // Stop animation
            animationTimer.stop();
            repaint();
        }
    }

    public void setBoardBounds(Rectangle bounds) {
        this.boardBounds = bounds;
        if (isLocalPlayerTurn) {
            repaint();
        }
    }

    public void dispose() {
        animationTimer.stop();
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        if (!isLocalPlayerTurn) {
            return; // Nothing to draw
        }

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            drawYourTurnBanner(g2);
            drawBoardHighlight(g2);
        } finally {
            g2.dispose();
        }
    }

    private void drawYourTurnBanner(Graphics2D g2) {
        long time = System.currentTimeMillis();
        int pulseAlpha = 180 + (int) (75 * Math.sin(time / 250.0));

        // Banner positioned below options button area
        int bannerWidth = 350;
        int bannerHeight = 26;
        int bannerX = 10;
        int bannerY = 60;

        // Glowing background
        g2.setColor(new Color(0, 180, 80, Math.min(pulseAlpha - 30, 150)));
        g2.fillRoundRect(bannerX - 2, bannerY - 2, bannerWidth + 4, bannerHeight + 4, 10, 10);

        // Main background gradient
        GradientPaint gradient = new GradientPaint(
                bannerX, bannerY, new Color(30, 160, 60, pulseAlpha),
                bannerX, bannerY + bannerHeight, new Color(20, 120, 40, pulseAlpha)
        );
        g2.setPaint(gradient);
        g2.fillRoundRect(bannerX, bannerY, bannerWidth, bannerHeight, 8, 8);

        // Border
        g2.setColor(new Color(100, 255, 150, pulseAlpha));
        g2.setStroke(new BasicStroke(1.5f));
        g2.drawRoundRect(bannerX, bannerY, bannerWidth, bannerHeight, 8, 8);

        // Text
        g2.setFont(FONT_SANSSERIF_BOLD_12);
        g2.setColor(Color.WHITE);
        String text = "DEIN ZUG";
        FontMetrics fm = g2.getFontMetrics();
        int textX = bannerX + (bannerWidth - fm.stringWidth(text)) / 2;
        int textY = bannerY + (bannerHeight + fm.getAscent() - fm.getDescent()) / 2;
        g2.drawString(text, textX, textY);
    }

    private void drawBoardHighlight(Graphics2D g2) {
        if (boardBounds == null) {
            return;
        }

        long time = System.currentTimeMillis();
        int pulseAlpha = 80 + (int) (40 * Math.sin(time / 400.0));

        int boardX = boardBounds.x - 10;
        int boardY = boardBounds.y - 10;
        int boardWidth = boardBounds.width + 20;
        int boardHeight = boardBounds.height + 20;

        // Outer glow
        g2.setColor(new Color(50, 200, 100, pulseAlpha / 2));
        g2.setStroke(new BasicStroke(8));
        g2.drawRoundRect(boardX - 4, boardY - 4, boardWidth + 8, boardHeight + 8, 20, 20);

        // Inner glow
        g2.setColor(new Color(100, 255, 150, pulseAlpha));
        g2.setStroke(new BasicStroke(4));
        g2.drawRoundRect(boardX, boardY, boardWidth, boardHeight, 15, 15);
    }
}
