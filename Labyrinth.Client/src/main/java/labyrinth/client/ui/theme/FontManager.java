package labyrinth.client.ui.theme;

import java.awt.*;
import java.io.InputStream;

/**
 * Manages custom fonts for the medieval theme.
 * Loads fonts from resources with fallback to system fonts.
 *
 * Primary fonts:
 * - Cinzel (Display): Medieval serif for headers and titles
 * - Crimson Text (UI): Readable serif for body text
 * - JetBrains Mono (Monospace): For coordinates and debug info
 */
public final class FontManager {

    private static Font cinzelBold;
    private static Font crimsonTextRegular;
    private static Font crimsonTextBold;
    private static Font jetBrainsMonoRegular;

    static {
        loadFonts();
    }

    private FontManager() {
        // Prevent instantiation
    }

    /**
     * Load all custom fonts from resources
     */
    private static void loadFonts() {
        cinzelBold = loadFont("/fonts/Cinzel-Bold.ttf", "Serif", Font.BOLD);
        crimsonTextRegular = loadFont("/fonts/CrimsonText-Regular.ttf", "Serif", Font.PLAIN);
        crimsonTextBold = loadFont("/fonts/CrimsonText-Bold.ttf", "Serif", Font.BOLD);
        jetBrainsMonoRegular = loadFont("/fonts/JetBrainsMono-Regular.ttf", "Monospaced", Font.PLAIN);
    }

    /**
     * Load a single font from resources with fallback
     */
    private static Font loadFont(String resourcePath, String fallbackFamily, int fallbackStyle) {
        try {
            InputStream fontStream = FontManager.class.getResourceAsStream(resourcePath);
            if (fontStream != null) {
                Font font = Font.createFont(Font.TRUETYPE_FONT, fontStream);
                GraphicsEnvironment.getLocalGraphicsEnvironment().registerFont(font);
                fontStream.close();
                return font.deriveFont(12f); // Base size
            } else {
                System.err.println("Font resource not found: " + resourcePath + " - Using fallback");
            }
        } catch (Exception e) {
            System.err.println("Failed to load font: " + resourcePath + " - " + e.getMessage());
        }
        // Fallback to system font
        return new Font(fallbackFamily, fallbackStyle, 12);
    }

    /**
     * Get display font (Cinzel) - for headers and titles
     * @param size Font size in points
     * @param style Font style (Font.PLAIN, Font.BOLD, Font.ITALIC)
     * @return Derived font with specified size and style
     */
    public static Font getDisplayFont(float size, int style) {
        if (cinzelBold != null) {
            return cinzelBold.deriveFont(style, size);
        }
        return new Font("Serif", style, (int) size);
    }

    /**
     * Get UI font (Crimson Text) - for body text
     * @param size Font size in points
     * @param style Font style (Font.PLAIN, Font.BOLD, Font.ITALIC)
     * @return Derived font with specified size and style
     */
    public static Font getUIFont(float size, int style) {
        Font baseFont = (style == Font.BOLD) ? crimsonTextBold : crimsonTextRegular;
        if (baseFont != null) {
            return baseFont.deriveFont(style, size);
        }
        return new Font("Serif", style, (int) size);
    }

    /**
     * Get monospace font (JetBrains Mono) - for coordinates and debug
     * @param size Font size in points
     * @return Derived font with specified size
     */
    public static Font getMonoFont(float size) {
        if (jetBrainsMonoRegular != null) {
            return jetBrainsMonoRegular.deriveFont(size);
        }
        return new Font("Monospaced", Font.PLAIN, (int) size);
    }

    // ===== Preset Font Sizes =====

    // Display fonts (Cinzel)
    public static Font getHugeDisplay() {
        return getDisplayFont(48f, Font.BOLD);
    }

    public static Font getLargeDisplay() {
        return getDisplayFont(36f, Font.BOLD);
    }

    public static Font getMediumDisplay() {
        return getDisplayFont(28f, Font.BOLD);
    }

    public static Font getSmallDisplay() {
        return getDisplayFont(22f, Font.BOLD);
    }

    // UI fonts (Crimson Text)
    public static Font getLargeUI() {
        return getUIFont(18f, Font.BOLD);
    }

    public static Font getLargeUIRegular() {
        return getUIFont(18f, Font.PLAIN);
    }

    public static Font getMediumUI() {
        return getUIFont(14f, Font.PLAIN);
    }

    public static Font getMediumUIBold() {
        return getUIFont(14f, Font.BOLD);
    }

    public static Font getSmallUI() {
        return getUIFont(12f, Font.PLAIN);
    }

    public static Font getSmallUIBold() {
        return getUIFont(12f, Font.BOLD);
    }

    public static Font getTinyUI() {
        return getUIFont(10f, Font.PLAIN);
    }

    // Monospace fonts (JetBrains Mono)
    public static Font getMediumMono() {
        return getMonoFont(12f);
    }

    public static Font getSmallMono() {
        return getMonoFont(10f);
    }
}
