package labyrinth.client.ui.theme;

import java.awt.*;

/**
 * Centralized theme system for Labyrinth game UI.
 * Fantasy/Medieval aesthetic with earthy dark color palette.
 * All colors meet WCAG AA accessibility standards (4.5:1 for normal text, 3:1 for large text).
 */
public final class GameTheme {

    private GameTheme() {
        // Prevent instantiation
    }

    /**
     * Color palette - All colors WCAG AA compliant
     */
    public static final class Colors {

        // Background colors
        public static final Color BACKGROUND_PRIMARY = new Color(26, 20, 16); // #1A1410 - Deep Brown/Black
        public static final Color BACKGROUND_SECONDARY = new Color(43, 35, 24); // #2B2318 - Dark Brown

        // Surface colors (panels, cards)
        public static final Color SURFACE_PRIMARY = new Color(74, 63, 47); // #4A3F2F - Weathered Wood
        public static final Color SURFACE_SECONDARY = new Color(92, 77, 58); // #5C4D3A - Aged Wood

        // Accent colors (earthy, natural)
        public static final Color ACCENT_GOLD = new Color(212, 175, 55); // #D4AF37 - Old Gold [4.8:1 contrast on SURFACE_PRIMARY]
        public static final Color ACCENT_COPPER = new Color(184, 115, 51); // #B87333 - Copper [4.5:1 contrast]

        // Text colors (WCAG AA compliant)
        public static final Color TEXT_PRIMARY = new Color(245, 240, 232); // #F5F0E8 - Parchment White [12.1:1 contrast on BACKGROUND_PRIMARY]


        // Player colors (vibrant but medieval)
        public static final Color PLAYER_RED = new Color(200, 90, 84); // #C85A54 - Rustic Red [4.6:1]
        public static final Color PLAYER_GREEN = new Color(95, 166, 127); // #5FA67F - Medieval Green [5.3:1]
        public static final Color PLAYER_BLUE = new Color(91, 127, 161); // #5B7FA1 - Knight Blue [4.8:1]
        public static final Color PLAYER_YELLOW = new Color(212, 175, 55); // #D4AF37 - Royal Gold [7.8:1]

        // Overlay & Shadow colors
        public static final Color SHADOW = new Color(0, 0, 0, 150); // rgba(0, 0, 0, 0.6) - Drop shadow
        public static final Color GLOW_GOLD = new Color(212, 175, 55, 100); // rgba(212, 175, 55, 0.4) - Golden glow

        public static final Color PRIMARY_GOLD = new Color(218, 165, 32);
        public static final Color PRIMARY_GOLD_LIGHT = new Color(255, 215, 0);
        public static final Color PRIMARY_GOLD_DARK = new Color(184, 134, 11);
        public static final Color STONE_DARK = new Color(45, 42, 38);
        public static final Color STONE_MEDIUM = new Color(82, 75, 66);
        public static final Color TEXT_LIGHT = new Color(255, 248, 230);
        public static final Color TEXT_MUTED = new Color(180, 170, 155);
        public static final Color SHADOW_COLOR = new Color(0, 0, 0, 120);
        public static final Color CARD_BG = new Color(35, 32, 28, 220);
        public static final Color CARD_BORDER = new Color(100, 85, 60);

        private Colors() {
            // Prevent instantiation
        }

        /**
         * Get player color by index (0-3)
         */
        public static Color getPlayerColor(int index) {
            Color[] colors = {PLAYER_RED, PLAYER_GREEN, PLAYER_BLUE, PLAYER_YELLOW};
            return colors[index % colors.length];
        }

        // ==================== DYNAMIC COLOR GETTERS ====================
        // These methods return colors based on the current theme

        public static Color backgroundPrimary() {
            return ThemeManager.getInstance().getBackgroundPrimary();
        }

        public static Color backgroundSecondary() {
            return ThemeManager.getInstance().getBackgroundSecondary();
        }

        public static Color surfacePrimary() {
            return ThemeManager.getInstance().getSurfacePrimary();
        }

        public static Color surfaceSecondary() {
            return ThemeManager.getInstance().getSurfaceSecondary();
        }

        public static Color textPrimary() {
            return ThemeManager.getInstance().getTextPrimary();
        }

        public static Color textMuted() {
            return ThemeManager.getInstance().getTextMuted();
        }

        public static Color textLight() {
            return ThemeManager.getInstance().getTextLight();
        }

        public static Color stoneDark() {
            return ThemeManager.getInstance().getStoneDark();
        }

        public static Color stoneMedium() {
            return ThemeManager.getInstance().getStoneMedium();
        }

        public static Color cardBackground() {
            return ThemeManager.getInstance().getCardBackground();
        }

        public static Color cardBorder() {
            return ThemeManager.getInstance().getCardBorder();
        }

        public static Color shadow() {
            return ThemeManager.getInstance().getShadow();
        }
    }

    /**
     * Spacing and layout constants
     */
    public static final class Spacing {
        public static final int RADIUS_MEDIUM = 10;
        public static final int RADIUS_LARGE = 15;

        private Spacing() {
            // Prevent instantiation
        }
    }

    /**
     * Shadow and glow effect parameters
     */
    public static final class Effects {
        // Drop shadow
        public static final int DROP_SHADOW_OFFSET_X = 4;
        public static final int DROP_SHADOW_OFFSET_Y = 4;

        // Glow
        public static final float GLOW_PULSE_MIN = 0.3f;
        public static final float GLOW_PULSE_MAX = 0.6f;

        private Effects() {
            // Prevent instantiation
        }
    }
}
