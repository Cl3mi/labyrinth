package labyrinth.client.ui;

import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.client.util.ImageAssetManager;

import javax.swing.*;
import java.awt.*;

/**
 * Abstract base class for panels that support theming.
 * Automatically handles background image loading and theme change events.
 */
public abstract class ThemedPanel extends JPanel {

    private Image backgroundImage;
    private boolean drawVignette = true;
    private boolean drawCornerDecorations = false;

    protected ThemedPanel() {
        setOpaque(false);
        loadBackgroundImage();
        ThemeManager.getInstance().addThemeChangeListener(this::handleThemeChange);
    }

    private void handleThemeChange() {
        loadBackgroundImage();
        onThemeChanged();
        repaint();
    }

    /**
     * Called when the theme changes. Subclasses can override to perform
     * additional theme-related updates.
     */
    protected void onThemeChanged() {
    }

    /**
     * Loads the background image from the ImageAssetManager.
     * Can be overridden for custom background loading behavior.
     */
    protected void loadBackgroundImage() {
        backgroundImage = ImageAssetManager.getInstance().getBackgroundImage();
    }

    /**
     * Gets the current background image.
     *
     * @return the background image, or null if none loaded
     */
    protected Image getBackgroundImage() {
        return backgroundImage;
    }

    /**
     * Sets whether to draw a vignette effect on the background.
     *
     * @param draw true to draw vignette, false otherwise
     */
    protected void setDrawVignette(boolean draw) {
        this.drawVignette = draw;
    }

    /**
     * Sets whether to draw decorative corners.
     *
     * @param draw true to draw corners, false otherwise
     */
    protected void setDrawCornerDecorations(boolean draw) {
        this.drawCornerDecorations = draw;
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int w = getWidth();
            int h = getHeight();

            drawBackground(g2, w, h);

            g2.setColor(ThemeManager.getInstance().getShadow());
            g2.fillRect(0, 0, w, h);

            if (drawVignette) {
                drawVignette(g2, w, h);
            }

            if (drawCornerDecorations) {
                drawDecorativeCorners(g2, w, h);
            }
        } finally {
            g2.dispose();
        }
        super.paintComponent(g);
    }

    /**
     * Draws the background. Can be overridden for custom background rendering.
     *
     * @param g2 the graphics context
     * @param w  the width
     * @param h  the height
     */
    protected void drawBackground(Graphics2D g2, int w, int h) {
        if (backgroundImage != null) {
            g2.drawImage(backgroundImage, 0, 0, w, h, this);
        } else {
            GradientPaint gradient = new GradientPaint(
                    0, 0, GameTheme.Colors.stoneDark(),
                    0, h, GameTheme.Colors.stoneMedium()
            );
            g2.setPaint(gradient);
            g2.fillRect(0, 0, w, h);
        }
    }

    /**
     * Draws a vignette effect on the edges of the panel.
     *
     * @param g2 the graphics context
     * @param w  the width
     * @param h  the height
     */
    protected void drawVignette(Graphics2D g2, int w, int h) {
        int centerX = w / 2;
        int centerY = h / 2;
        float radius = Math.max(w, h) * 0.8f;

        RadialGradientPaint vignette = new RadialGradientPaint(
                centerX, centerY, radius,
                new float[]{0.3f, 0.7f, 1.0f},
                new Color[]{
                        new Color(0, 0, 0, 0),
                        ThemeEffects.withAlpha(GameTheme.Colors.shadow(), 50),
                        ThemeEffects.withAlpha(GameTheme.Colors.shadow(), 130)
                }
        );
        g2.setPaint(vignette);
        g2.fillRect(0, 0, w, h);
    }

    /**
     * Draws decorative corner elements.
     *
     * @param g2 the graphics context
     * @param w  the width
     * @param h  the height
     */
    protected void drawDecorativeCorners(Graphics2D g2, int w, int h) {
        Color gold = GameTheme.Colors.PRIMARY_GOLD_LIGHT;
        g2.setColor(new Color(gold.getRed(), gold.getGreen(), gold.getBlue(), 50));
        g2.setStroke(new BasicStroke(2f));

        int size = 50;

        g2.drawLine(25, 25, 25 + size, 25);
        g2.drawLine(25, 25, 25, 25 + size);
        g2.fillOval(22, 22, 6, 6);

        g2.drawLine(w - 25, 25, w - 25 - size, 25);
        g2.drawLine(w - 25, 25, w - 25, 25 + size);
        g2.fillOval(w - 28, 22, 6, 6);

        g2.drawLine(25, h - 25, 25 + size, h - 25);
        g2.drawLine(25, h - 25, 25, h - 25 - size);
        g2.fillOval(22, h - 28, 6, 6);

        g2.drawLine(w - 25, h - 25, w - 25 - size, h - 25);
        g2.drawLine(w - 25, h - 25, w - 25, h - 25 - size);
        g2.fillOval(w - 28, h - 28, 6, 6);
    }
}
