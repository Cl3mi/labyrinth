package labyrinth.client.ui.theme;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.prefs.Preferences;

/**
 * Singleton that manages the application theme (dark/light mode).
 * Components can register as listeners to be notified of theme changes.
 */
public final class ThemeManager {

    private static final ThemeManager INSTANCE = new ThemeManager();
    private static final String PREF_DARK_THEME = "darkTheme";

    private final Preferences prefs;
    private boolean darkMode;
    private final List<Runnable> listeners = new ArrayList<>();

    // ==================== DARK THEME COLORS ====================
    private static final Color DARK_BACKGROUND_PRIMARY = new Color(26, 20, 16);
    private static final Color DARK_BACKGROUND_SECONDARY = new Color(43, 35, 24);
    private static final Color DARK_SURFACE_PRIMARY = new Color(74, 63, 47);
    private static final Color DARK_SURFACE_SECONDARY = new Color(92, 77, 58);
    private static final Color DARK_TEXT_PRIMARY = new Color(245, 240, 232);
    private static final Color DARK_TEXT_MUTED = new Color(180, 170, 155);
    private static final Color DARK_STONE = new Color(45, 42, 38);
    private static final Color DARK_STONE_MEDIUM = new Color(82, 75, 66);
    private static final Color DARK_CARD_BG = new Color(35, 32, 28, 220);
    private static final Color DARK_CARD_BORDER = new Color(100, 85, 60);

    // ==================== LIGHT THEME COLORS ====================
    // Using similar dark/earthy tones as buttons for consistency
    private static final Color LIGHT_BACKGROUND_PRIMARY = new Color(225, 218, 205);
    private static final Color LIGHT_BACKGROUND_SECONDARY = new Color(210, 200, 185);
    private static final Color LIGHT_SURFACE_PRIMARY = new Color(65, 58, 48);         // Dark like buttons (STONE_DARK style)
    private static final Color LIGHT_SURFACE_SECONDARY = new Color(85, 75, 62);       // Dark like buttons (STONE_MEDIUM style)
    private static final Color LIGHT_TEXT_PRIMARY = new Color(245, 240, 230);         // Light text on dark surfaces
    private static final Color LIGHT_TEXT_MUTED = new Color(180, 170, 155);
    private static final Color LIGHT_STONE = new Color(55, 50, 45);
    private static final Color LIGHT_STONE_MEDIUM = new Color(75, 68, 58);
    private static final Color LIGHT_CARD_BG = new Color(55, 50, 45, 230);            // Dark card like buttons
    private static final Color LIGHT_CARD_BORDER = new Color(120, 105, 80);

    // ==================== SHARED ACCENT COLORS ====================
    private static final Color ACCENT_GOLD = new Color(212, 175, 55);
    private static final Color ACCENT_COPPER = new Color(184, 115, 51);
    private static final Color PRIMARY_GOLD = new Color(218, 165, 32);
    private static final Color PRIMARY_GOLD_LIGHT = new Color(255, 215, 0);
    private static final Color PRIMARY_GOLD_DARK = new Color(184, 134, 11);

    private ThemeManager() {
        // Initialize preferences
        prefs = Preferences.userNodeForPackage(ThemeManager.class);
        // Load saved preference
        darkMode = prefs.getBoolean(PREF_DARK_THEME, true);
    }

    public static ThemeManager getInstance() {
        return INSTANCE;
    }

    public boolean isDarkMode() {
        return darkMode;
    }

    public void setDarkMode(boolean dark) {
        System.out.println("[ThemeManager] setDarkMode called: " + dark + " (current: " + darkMode + ")");
        if (this.darkMode != dark) {
            this.darkMode = dark;
            prefs.putBoolean(PREF_DARK_THEME, dark);
            System.out.println("[ThemeManager] Theme changed to: " + (dark ? "DARK" : "LIGHT") + ", notifying " + listeners.size() + " listeners");
            notifyListeners();
        }
    }

    public void toggleTheme() {
        setDarkMode(!darkMode);
    }

    /**
     * Register a listener to be notified when the theme changes.
     * Typically used to trigger repaint() on components.
     */
    public void addThemeChangeListener(Runnable listener) {
        listeners.add(listener);
    }

    public void removeThemeChangeListener(Runnable listener) {
        listeners.remove(listener);
    }

    private void notifyListeners() {
        for (Runnable listener : listeners) {
            listener.run();
        }
    }

    // ==================== COLOR GETTERS ====================

    public Color getBackgroundPrimary() {
        return darkMode ? DARK_BACKGROUND_PRIMARY : LIGHT_BACKGROUND_PRIMARY;
    }

    public Color getBackgroundSecondary() {
        return darkMode ? DARK_BACKGROUND_SECONDARY : LIGHT_BACKGROUND_SECONDARY;
    }

    public Color getSurfacePrimary() {
        return darkMode ? DARK_SURFACE_PRIMARY : LIGHT_SURFACE_PRIMARY;
    }

    public Color getSurfaceSecondary() {
        return darkMode ? DARK_SURFACE_SECONDARY : LIGHT_SURFACE_SECONDARY;
    }

    public Color getTextPrimary() {
        return darkMode ? DARK_TEXT_PRIMARY : LIGHT_TEXT_PRIMARY;
    }

    public Color getTextMuted() {
        return darkMode ? DARK_TEXT_MUTED : LIGHT_TEXT_MUTED;
    }

    public Color getTextLight() {
        return darkMode ? DARK_TEXT_PRIMARY : LIGHT_TEXT_PRIMARY;
    }

    public Color getStoneDark() {
        return darkMode ? DARK_STONE : LIGHT_STONE;
    }

    public Color getStoneMedium() {
        return darkMode ? DARK_STONE_MEDIUM : LIGHT_STONE_MEDIUM;
    }

    public Color getCardBackground() {
        return darkMode ? DARK_CARD_BG : LIGHT_CARD_BG;
    }

    public Color getCardBorder() {
        return darkMode ? DARK_CARD_BORDER : LIGHT_CARD_BORDER;
    }

    // Accent colors remain the same in both themes
    public Color getAccentGold() {
        return ACCENT_GOLD;
    }

    public Color getAccentCopper() {
        return ACCENT_COPPER;
    }

    public Color getPrimaryGold() {
        return PRIMARY_GOLD;
    }

    public Color getPrimaryGoldLight() {
        return PRIMARY_GOLD_LIGHT;
    }

    public Color getPrimaryGoldDark() {
        return PRIMARY_GOLD_DARK;
    }

    public Color getShadow() {
        return new Color(0, 0, 0, darkMode ? 150 : 80);
    }

    public Color getGlowGold() {
        return new Color(212, 175, 55, 100);
    }

    /**
     * Returns the subtitle color for the current theme.
     * Dark mode: Orange, Light mode: Dark brown
     */
    public Color getSubtitleColor() {
        return darkMode ? new Color(46, 68, 246) : new Color(255, 153, 0);
    }

    /**
     * Returns the background image path for the current theme.
     */
    public String getBackgroundImagePath() {
        return darkMode ? "/images/ui/BackgroundDark.png" : "/images/ui/BackgroundLight.png";
    }
}
