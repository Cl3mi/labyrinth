package labyrinth.client.ui.Styles;

import java.awt.*;

public class ColorsFonts {

    // Farben - Mystische Labyrinth-Palette (wie MainMenuPanel)
    public static final Color PRIMARY_GOLD = new Color(218, 165, 32);
    public static final Color PRIMARY_GOLD_LIGHT = new Color(255, 215, 0);
    public static final Color PRIMARY_GOLD_DARK = new Color(184, 134, 11);
    public static final Color STONE_DARK = new Color(45, 42, 38);
    public static final Color STONE_MEDIUM = new Color(82, 75, 66);
    public static final Color STONE_LIGHT = new Color(120, 110, 95);
    public static final Color TEXT_LIGHT = new Color(255, 248, 230);
    public static final Color TEXT_MUTED = new Color(180, 170, 155);
    public static final Color SHADOW_COLOR = new Color(0, 0, 0, 120);
    public static final Color CARD_BG = new Color(35, 32, 28, 220);
    public static final Color CARD_BORDER = new Color(100, 85, 60);

    // Fonts
    public static Font titleFont;
    public static Font labelFont;
    public static Font buttonFont;

    public static void initFonts() {
        titleFont = new Font("Serif", Font.BOLD, 28);
        labelFont = new Font("Serif", Font.PLAIN, 14);
        buttonFont = new Font("Serif", Font.BOLD, 16);

        if (isFontAvailable("Cinzel")) {
            titleFont = new Font("Cinzel", Font.BOLD, 28);
            buttonFont = new Font("Cinzel", Font.BOLD, 16);
        }
    }

    private static boolean isFontAvailable(String fontName) {
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        for (String family : ge.getAvailableFontFamilyNames()) {
            if (family.equalsIgnoreCase(fontName)) return true;
        }
        return false;
    }

}
