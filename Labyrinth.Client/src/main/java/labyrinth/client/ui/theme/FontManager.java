package labyrinth.client.ui.theme;

import java.awt.*;
import java.io.InputStream;
import java.util.Map;

/**
 * Manages custom fonts for the medieval theme.
 * Loads fonts from resources with fallback to system fonts.
 * Primary fonts:
 * - Cinzel (Display): Medieval serif for headers and titles
 * - Crimson Text (UI): Readable serif for body text
 */
public final class FontManager {

    private static Font cinzelBold;
    private static Font crimsonTextRegular;
    private static Font crimsonTextBold;
    private static final Map<String, Font> presetCache = new java.util.HashMap<>();

    static {
        loadFonts();
    }

    private FontManager() {
        // Prevent instantiation
    }


    private static void loadFonts() {
        cinzelBold = loadFont("/fonts/Cinzel-Bold.ttf", "Serif", Font.BOLD);
        crimsonTextRegular = loadFont("/fonts/CrimsonText-Regular.ttf", "Serif", Font.PLAIN);
        crimsonTextBold = loadFont("/fonts/CrimsonText-Bold.ttf", "Serif", Font.BOLD);
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
        return makeHeading(36f);
    }

    public static Font getHeadingMedium() {
        return makeHeading(28f);
    }

    public static Font getHeadingSmall() {
        return makeHeading(22f);
    }

    public static Font getBodyLarge() {
        return getBodyLarge(Font.PLAIN);
    }
    public static Font getBodyLarge(int style) {
        return makeBody(18f, style);
    }

    public static Font getBodyMedium() {
        return getBodyMedium(Font.PLAIN);
    }
    public static Font getBodyMedium(int style) {
        return makeBody(14f, style);
    }

    public static Font getBodySmall() {
        return getBodySmall(Font.PLAIN);
    }
    public static Font getBodySmall(int style) {
        return makeBody(12f, style);
    }

    public static Font getBodyTiny() {
        return getBodyTiny(Font.PLAIN);
    }
    public static Font getBodyTiny(int style) {
        return makeBody(10f, style);
    }


    private static Font makeHeading(float size) {
        String key = "heading:" + Math.round(size);
        return presetCache.computeIfAbsent(key, k -> {
            if (cinzelBold != null) return cinzelBold.deriveFont(Font.BOLD, size);
            return new Font("SansSerif", Font.BOLD, Math.round(size));
        });
    }


    private static Font makeBody(float size, int style) {
        String key = "body:" + Math.round(size) + ":" + style;
        return presetCache.computeIfAbsent(key, k -> {
            if (crimsonTextRegular != null) {
                if ((style & Font.BOLD) != 0 && crimsonTextBold != null) {
                    return crimsonTextBold.deriveFont(style, size);
                }
                return crimsonTextRegular.deriveFont(style, size);
            }
            return new Font("SansSerif", style, Math.round(size));
        });
    }

    // Map a requested numeric size to one of the predefined presets.
    // Use this to replace ad-hoc float-based font creation across panels.
    public static Font getFontForSize(float size, int style) {
        if (size >= 28f) return makeHeading(size);
        if (size >= 20f) return makeHeading(size);
        if (size >= 16f) return makeBody(size, style);
        if (size >= 12f) return makeBody(size, style);
        if (size >= 10f) return makeBody(size, style);
        return makeBody(size, style);
    }
}
