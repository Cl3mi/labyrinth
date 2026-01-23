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

    public static Font titleFont;
    public static Font labelFont;
    public static Font buttonFont;

    static {
        loadFonts();
    }

    private FontManager() {
        // Prevent instantiation
    }

    public static void initFonts() {
        // Use SansSerif as the default unified UI font
        titleFont = new Font("SansSerif", Font.BOLD, 28);
        labelFont = new Font("SansSerif", Font.PLAIN, 14);
        buttonFont = new Font("SansSerif", Font.BOLD, 16);
    }


    private static void loadFonts() {
        cinzelBold = loadFont("/fonts/Cinzel-Bold.ttf", "Serif", Font.BOLD);
        crimsonTextRegular = loadFont("/fonts/CrimsonText-Regular.ttf", "Serif", Font.PLAIN);
        crimsonTextBold = loadFont("/fonts/CrimsonText-Bold.ttf", "Serif", Font.BOLD);
        jetBrainsMonoRegular = loadFont("/fonts/JetBrainsMono-Regular.ttf", "Monospaced", Font.PLAIN);
    }


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

    public static Font getHeadingLarge() {
        return new Font("SansSerif", Font.BOLD, 36);
    }

    public static Font getHeadingMedium() {
        return new Font("SansSerif", Font.BOLD, 28);
    }

    public static Font getHeadingSmall() {
        return new Font("SansSerif", Font.BOLD, 22);
    }

    // ===== Body / UI fonts (style can be provided) =====
    public static Font getBodyLarge(int style) { // e.g. headings in sidebars
        return new Font("SansSerif", style, 18);
    }

    public static Font getBodyMedium(int style) {
        return new Font("SansSerif", style, 14);
    }

    public static Font getBodySmall(int style) {
        return new Font("SansSerif", style, 12);
    }

    public static Font getBodyTiny(int style) {
        return new Font("SansSerif", style, 10);
    }

    public static Font getMediumMono() {
        if (jetBrainsMonoRegular != null) return jetBrainsMonoRegular.deriveFont(12f);
        return new Font("Monospaced", Font.PLAIN, 12);
    }

    // Map a requested numeric size to one of the predefined presets.
    // Use this to replace ad-hoc float-based font creation across panels.
    public static Font getFontForSize(float size, int style) {
        if (size >= 28f) return getHeadingLarge();
        if (size >= 20f) return getHeadingMedium();
        if (size >= 16f) return getBodyLarge(style);
        if (size >= 12f) return getBodyMedium(style);
        if (size >= 10f) return getBodySmall(style);
        return getBodyTiny(style);
    }
}
