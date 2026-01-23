package labyrinth.client.ui.theme;

import java.awt.Font;
import java.util.Map;


public final class FontManager {

    private static final Map<String, Font> presetCache = new java.util.HashMap<>();

    private FontManager() {
        // Prevent instantiation
    }

    private static Font createSans(float size, int style) {
        return new Font("SansSerif", style, Math.round(size));
    }

    public static Font getHeadingLarge() {
        return getHeading(36f);
    }

    public static Font getHeadingMedium() {
        return getHeading(28f);
    }

    public static Font getHeadingSmall() {
        return getHeading(24f);
    }

    public static Font getBodyLarge() {
        return getBody(22f, Font.PLAIN);
    }
    public static Font getBodyLarge(int style) {
        return getBody(22f, style);
    }

    public static Font getBodyMedium() {
        return getBody(16f, Font.PLAIN);
    }
    public static Font getBodyMedium(int style) {
        return getBody(16f, style);
    }

    public static Font getBodySmall() {
        return getBody(14f, Font.PLAIN);
    }
    public static Font getBodySmall(int style) {
        return getBody(14f, style);
    }

    public static Font getBodyTiny() {
        return getBody(12f, Font.PLAIN);
    }
    public static Font getBodyTiny(int style) {
        return getBody(12f, style);
    }

    private static Font getHeading(float size) {
        String key = "heading:" + Math.round(size);
        return presetCache.computeIfAbsent(key, k -> createSans(size, Font.BOLD));
    }

    private static Font getBody(float size, int style) {
        String key = "body:" + Math.round(size) + ":" + style;
        return presetCache.computeIfAbsent(key, k -> createSans(size, style));
    }


    public static Font getFontForSize(float size, int style) {
        if (size >= 32f) return getHeadingLarge();
        if (size >= 28f) return getHeadingMedium();
        if (size >= 24f) return getHeadingSmall();
        if (size >= 22f) return getBodyLarge(style);
        if (size >= 18f) return getBodyMedium(style);
        if (size >= 16f) return getBodySmall(style);

        return getBodyTiny(style);
    }
}
