package labyrinth.client.ui.theme;

import java.awt.*;
import java.awt.geom.*;

/**
 * Utility class for rendering medieval-themed visual effects.
 * Provides reusable methods for borders, shadows, glows, and decorative elements.
 */
public final class ThemeEffects {

    private ThemeEffects() {
        // Prevent instantiation
    }

    /**
     * Draw ornate panel border with copper accent and golden glow
     * @param g2 Graphics2D context
     * @param x X coordinate
     * @param y Y coordinate
     * @param width Panel width
     * @param height Panel height
     */
    public static void drawOrnateBorder(Graphics2D g2, int x, int y, int width, int height) {
        Graphics2D g = (Graphics2D) g2.create();

        // Outer glow
        g.setColor(GameTheme.Colors.GLOW_GOLD);
        g.setStroke(new BasicStroke(8f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g.drawRoundRect(x - 4, y - 4, width + 8, height + 8,
                GameTheme.Spacing.RADIUS_LARGE, GameTheme.Spacing.RADIUS_LARGE);

        // Main border
        g.setColor(GameTheme.Colors.ACCENT_COPPER);
        g.setStroke(new BasicStroke(4f));
        g.drawRoundRect(x, y, width, height,
                GameTheme.Spacing.RADIUS_LARGE, GameTheme.Spacing.RADIUS_LARGE);

        // Inner highlight
        g.setColor(new Color(
                GameTheme.Colors.ACCENT_GOLD.getRed(),
                GameTheme.Colors.ACCENT_GOLD.getGreen(),
                GameTheme.Colors.ACCENT_GOLD.getBlue(),
                100
        ));
        g.setStroke(new BasicStroke(2f));
        g.drawRoundRect(x + 4, y + 4, width - 8, height - 8,
                GameTheme.Spacing.RADIUS_MEDIUM, GameTheme.Spacing.RADIUS_MEDIUM);

        g.dispose();
    }

    /**
     * Draw card-style border with drop shadow
     * @param g2 Graphics2D context
     * @param x X coordinate
     * @param y Y coordinate
     * @param width Card width
     * @param height Card height
     */
    public static void drawCardBorder(Graphics2D g2, int x, int y, int width, int height) {
        Graphics2D g = (Graphics2D) g2.create();

        // Drop shadow
        g.setColor(GameTheme.Colors.SHADOW);
        g.fillRoundRect(
                x + GameTheme.Effects.DROP_SHADOW_OFFSET_X,
                y + GameTheme.Effects.DROP_SHADOW_OFFSET_Y,
                width, height,
                GameTheme.Spacing.RADIUS_MEDIUM,
                GameTheme.Spacing.RADIUS_MEDIUM
        );

        // Main border
        g.setColor(GameTheme.Colors.ACCENT_COPPER);
        g.setStroke(new BasicStroke(2f));
        g.drawRoundRect(x, y, width, height,
                GameTheme.Spacing.RADIUS_MEDIUM, GameTheme.Spacing.RADIUS_MEDIUM);

        g.dispose();
    }

    /**
     * Draw embossed button with 3D effect
     * @param g2 Graphics2D context
     * @param x X coordinate
     * @param y Y coordinate
     * @param width Button width
     * @param height Button height
     * @param pressed Whether button is in pressed state
     */
    public static void drawEmbossedButton(Graphics2D g2, int x, int y, int width, int height, boolean pressed) {
        Graphics2D g = (Graphics2D) g2.create();

        if (pressed) {
            // Inset effect when pressed
            g.setColor(new Color(
                    GameTheme.Colors.ACCENT_COPPER.getRed(),
                    GameTheme.Colors.ACCENT_COPPER.getGreen(),
                    GameTheme.Colors.ACCENT_COPPER.getBlue(),
                    180
            ));
            g.setStroke(new BasicStroke(3f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, null, 0));
        } else {
            // Raised effect when not pressed
            g.setColor(GameTheme.Colors.ACCENT_COPPER);
            g.setStroke(new BasicStroke(3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        }

        g.drawRoundRect(x, y, width, height,
                GameTheme.Spacing.RADIUS_MEDIUM, GameTheme.Spacing.RADIUS_MEDIUM);

        if (!pressed) {
            // Top highlight
            g.setColor(GameTheme.Colors.ACCENT_GOLD);
            g.setStroke(new BasicStroke(1f));
            g.drawLine(x + 8, y + 2, x + width - 8, y + 2);

            // Bottom shadow
            Color darkerCopper = new Color(
                    Math.max(0, GameTheme.Colors.ACCENT_COPPER.getRed() - 40),
                    Math.max(0, GameTheme.Colors.ACCENT_COPPER.getGreen() - 40),
                    Math.max(0, GameTheme.Colors.ACCENT_COPPER.getBlue() - 40)
            );
            g.setColor(darkerCopper);
            g.drawLine(x + 8, y + height - 2, x + width - 8, y + height - 2);
        }

        g.dispose();
    }

    /**
     * Draw pulsing glow effect
     * @param g2 Graphics2D context
     * @param centerX Center X coordinate
     * @param centerY Center Y coordinate
     * @param radius Glow radius
     * @param timeMs Current time in milliseconds (for animation)
     */
    public static void drawPulsingGlow(Graphics2D g2, int centerX, int centerY, int radius, long timeMs) {
        Graphics2D g = (Graphics2D) g2.create();

        float pulse = GameTheme.Effects.GLOW_PULSE_MIN +
                (GameTheme.Effects.GLOW_PULSE_MAX - GameTheme.Effects.GLOW_PULSE_MIN) *
                        (0.5f + 0.5f * (float) Math.sin(timeMs / 400.0));

        int alpha = (int) (255 * pulse);

        RadialGradientPaint gradient = new RadialGradientPaint(
                centerX, centerY, radius,
                new float[]{0f, 0.7f, 1f},
                new Color[]{
                        new Color(
                                GameTheme.Colors.GLOW_GOLD.getRed(),
                                GameTheme.Colors.GLOW_GOLD.getGreen(),
                                GameTheme.Colors.GLOW_GOLD.getBlue(),
                                alpha
                        ),
                        new Color(
                                GameTheme.Colors.GLOW_GOLD.getRed(),
                                GameTheme.Colors.GLOW_GOLD.getGreen(),
                                GameTheme.Colors.GLOW_GOLD.getBlue(),
                                alpha / 2
                        ),
                        new Color(0, 0, 0, 0)
                }
        );

        g.setPaint(gradient);
        g.fillOval(centerX - radius, centerY - radius, radius * 2, radius * 2);

        g.dispose();
    }

    /**
     * Draw decorative scroll divider with gradient line and center ornament
     * @param g2 Graphics2D context
     * @param x1 Start X coordinate
     * @param x2 End X coordinate
     * @param y Y coordinate (vertical center of divider)
     */
    public static void drawScrollDivider(Graphics2D g2, int x1, int x2, int y) {
        Graphics2D g = (Graphics2D) g2.create();
        int centerX = (x1 + x2) / 2;

        // Left gradient line
        GradientPaint leftGradient = new GradientPaint(
                x1, y, new Color(GameTheme.Colors.ACCENT_COPPER.getRed(),
                GameTheme.Colors.ACCENT_COPPER.getGreen(),
                GameTheme.Colors.ACCENT_COPPER.getBlue(), 50),
                centerX, y, GameTheme.Colors.ACCENT_COPPER
        );

        g.setPaint(leftGradient);
        g.setStroke(new BasicStroke(2f));
        g.drawLine(x1, y, centerX, y);

        // Right gradient line
        GradientPaint rightGradient = new GradientPaint(
                centerX, y, GameTheme.Colors.ACCENT_COPPER,
                x2, y, new Color(GameTheme.Colors.ACCENT_COPPER.getRed(),
                GameTheme.Colors.ACCENT_COPPER.getGreen(),
                GameTheme.Colors.ACCENT_COPPER.getBlue(), 50)
        );

        g.setPaint(rightGradient);
        g.drawLine(centerX, y, x2, y);

        // Center ornament (small diamond)
        g.setColor(GameTheme.Colors.ACCENT_GOLD);
        int[] xPoints = {centerX, centerX + 4, centerX, centerX - 4};
        int[] yPoints = {y - 4, y, y + 4, y};
        g.fillPolygon(xPoints, yPoints, 4);

        g.dispose();
    }

    /**
     * Draw corner ornaments for medieval card design
     * @param g2 Graphics2D context
     * @param x Card X coordinate
     * @param y Card Y coordinate
     * @param width Card width
     * @param height Card height
     * @param ornamentSize Size of corner ornaments
     */
    public static void drawCornerOrnaments(Graphics2D g2, int x, int y, int width, int height, int ornamentSize) {
        Graphics2D g = (Graphics2D) g2.create();
        g.setColor(GameTheme.Colors.ACCENT_GOLD);
        g.setStroke(new BasicStroke(2f));

        // Top-left corner
        g.drawLine(x, y + ornamentSize, x, y);
        g.drawLine(x, y, x + ornamentSize, y);

        // Top-right corner
        g.drawLine(x + width - ornamentSize, y, x + width, y);
        g.drawLine(x + width, y, x + width, y + ornamentSize);

        // Bottom-left corner
        g.drawLine(x, y + height - ornamentSize, x, y + height);
        g.drawLine(x, y + height, x + ornamentSize, y + height);

        // Bottom-right corner
        g.drawLine(x + width - ornamentSize, y + height, x + width, y + height);
        g.drawLine(x + width, y + height, x + width, y + height - ornamentSize);

        g.dispose();
    }

    /**
     * Create a gradient paint for wood texture effect
     * @param x1 Start X
     * @param y1 Start Y
     * @param x2 End X
     * @param y2 End Y
     * @return GradientPaint for wood effect
     */
    public static GradientPaint createWoodGradient(int x1, int y1, int x2, int y2) {
        return new GradientPaint(
                x1, y1, GameTheme.Colors.SURFACE_PRIMARY,
                x2, y2, GameTheme.Colors.SURFACE_SECONDARY
        );
    }

    /**
     * Create a vertical gradient for panels
     * @param y1 Start Y
     * @param y2 End Y
     * @param topColor Top color
     * @param bottomColor Bottom color
     * @return GradientPaint
     */
    public static GradientPaint createVerticalGradient(int y1, int y2, Color topColor, Color bottomColor) {
        return new GradientPaint(0, y1, topColor, 0, y2, bottomColor);
    }

    /**
     * Brighten a color by a percentage
     * @param color Original color
     * @param percent Percentage to brighten (0.0 to 1.0)
     * @return Brightened color
     */
    public static Color brighten(Color color, float percent) {
        int r = Math.min(255, (int) (color.getRed() + (255 - color.getRed()) * percent));
        int g = Math.min(255, (int) (color.getGreen() + (255 - color.getGreen()) * percent));
        int b = Math.min(255, (int) (color.getBlue() + (255 - color.getBlue()) * percent));
        return new Color(r, g, b, color.getAlpha());
    }

    /**
     * Darken a color by a percentage
     * @param color Original color
     * @param percent Percentage to darken (0.0 to 1.0)
     * @return Darkened color
     */
    public static Color darken(Color color, float percent) {
        int r = Math.max(0, (int) (color.getRed() * (1 - percent)));
        int g = Math.max(0, (int) (color.getGreen() * (1 - percent)));
        int b = Math.max(0, (int) (color.getBlue() * (1 - percent)));
        return new Color(r, g, b, color.getAlpha());
    }

    /**
     * Create color with adjusted alpha
     * @param color Original color
     * @param alpha New alpha value (0-255)
     * @return Color with new alpha
     */
    public static Color withAlpha(Color color, int alpha) {
        return new Color(color.getRed(), color.getGreen(), color.getBlue(), alpha);
    }

    /**
     * Blends two colors together using linear interpolation.
     *
     * @param color1 The first color
     * @param color2 The second color
     * @param ratio  Blend ratio (0.0 = all color1, 1.0 = all color2)
     * @return The blended color
     */
    public static Color blendColors(Color color1, Color color2, float ratio) {
        ratio = Math.max(0f, Math.min(1f, ratio)); // Clamp to 0-1
        float invRatio = 1f - ratio;

        int r = (int) (color1.getRed() * invRatio + color2.getRed() * ratio);
        int g = (int) (color1.getGreen() * invRatio + color2.getGreen() * ratio);
        int b = (int) (color1.getBlue() * invRatio + color2.getBlue() * ratio);
        int a = (int) (color1.getAlpha() * invRatio + color2.getAlpha() * ratio);

        return new Color(r, g, b, a);
    }
}
