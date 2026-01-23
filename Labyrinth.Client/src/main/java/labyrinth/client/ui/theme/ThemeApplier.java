package labyrinth.client.ui.theme;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Helper that applies centralized theme settings to the Swing UI.
 * - Initializes fonts/resources
 * - Applies UIManager defaults
 * - Provides recursive application helpers for already-created component trees
 */
public final class ThemeApplier {

    private static final String THEME_PROPERTIES = "/styles/theme.properties";
    private static final Properties props = new Properties();

    private ThemeApplier() {
        // prevent instantiation
    }

    /**
     * Initialize fonts and load theme properties. Non-fatal on failure.
     */
    public static void initFontsAndResources() {
        try {
            FontManager.initFonts();
        } catch (Exception e) {
            System.err.println("[ThemeApplier] Failed to init fonts: " + e.getMessage());
        }

        try (InputStream in = ThemeApplier.class.getResourceAsStream(THEME_PROPERTIES)) {
            if (in != null) {
                props.load(in);
            } else {
                System.out.println("[ThemeApplier] theme.properties not found, using defaults");
            }
        } catch (IOException e) {
            System.err.println("[ThemeApplier] Failed to load theme properties: " + e.getMessage());
        }
    }

    /**
     * Apply UIManager defaults (fonts, colors, basic component styles)
     */
    public static void applyUIDefaults() {
        try {
            // Fonts
            UIManager.put("Label.font", FontManager.getBodyMedium(Font.PLAIN));
            UIManager.put("Button.font", FontManager.getBodyMedium(Font.BOLD));
            UIManager.put("TextField.font", FontManager.getBodyMedium(Font.PLAIN));
            UIManager.put("TextArea.font", FontManager.getBodyMedium(Font.PLAIN));
            UIManager.put("List.font", FontManager.getBodyMedium(Font.PLAIN));
            UIManager.put("ComboBox.font", FontManager.getBodyMedium(Font.PLAIN));

            // Colors
            UIManager.put("Panel.background", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.backgroundPrimary()));
            UIManager.put("Label.foreground", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.textPrimary()));
            UIManager.put("Button.background", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.surfacePrimary()));
            UIManager.put("Button.foreground", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.textLight()));
            UIManager.put("ToolTip.background", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.cardBackground()));
            UIManager.put("ToolTip.foreground", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.textPrimary()));
            UIManager.put("List.selectionBackground", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.surfaceSecondary()));
            UIManager.put("List.selectionForeground", new javax.swing.plaf.ColorUIResource(GameTheme.Colors.textPrimary()));

            // Borders / other
            UIManager.put("TextField.border", createInputBorder());
            UIManager.put("PasswordField.border", createInputBorder());

        } catch (Exception e) {
            System.err.println("[ThemeApplier] Failed to apply UI defaults: " + e.getMessage());
        }
    }

    /**
     * Recursively apply theme to an existing container and its children.
     */
    public static void applyToContainer(Container root) {
        if (root == null) return;
        applyToComponent(root);
        for (Component c : root.getComponents()) {
            if (c instanceof Container) applyToContainer((Container) c);
            else applyToComponent(c);
        }
    }

    /**
     * Apply theme to a single component (type-aware).
     */
    public static void applyToComponent(Component c) {
        if (c == null) return;

        try {
            if (c instanceof JPanel) {
                c.setBackground(GameTheme.Colors.backgroundPrimary());
            } else if (c instanceof JButton) {
                JButton b = (JButton) c;
                b.setBackground(GameTheme.Colors.surfacePrimary());
                b.setForeground(GameTheme.Colors.textLight());
                b.setFont(FontManager.getBodyMedium(Font.BOLD));
                b.setBorder(createButtonBorder());
                b.setOpaque(true);
            } else if (c instanceof JLabel) {
                JLabel l = (JLabel) c;
                l.setForeground(GameTheme.Colors.textPrimary());
                l.setFont(FontManager.getBodyMedium(Font.PLAIN));
            } else if (c instanceof JTextField) {
                JTextField t = (JTextField) c;
                t.setBackground(GameTheme.Colors.surfacePrimary());
                t.setForeground(GameTheme.Colors.textPrimary());
                t.setFont(FontManager.getBodyMedium(Font.PLAIN));
                t.setBorder(createInputBorder());
            } else if (c instanceof JPasswordField) {
                JPasswordField p = (JPasswordField) c;
                p.setBackground(GameTheme.Colors.surfacePrimary());
                p.setForeground(GameTheme.Colors.textPrimary());
                p.setFont(FontManager.getBodyMedium(Font.PLAIN));
                p.setBorder(createInputBorder());
            } else if (c instanceof JList) {
                JList<?> list = (JList<?>) c;
                list.setBackground(GameTheme.Colors.backgroundSecondary());
                list.setForeground(GameTheme.Colors.textPrimary());
                list.setFont(FontManager.getBodyMedium(Font.PLAIN));
            } else if (c instanceof JScrollPane) {
                JScrollPane sp = (JScrollPane) c;
                sp.getViewport().setBackground(GameTheme.Colors.backgroundPrimary());
            } else if (c instanceof JToolTip) {
                JToolTip tip = (JToolTip) c;
                tip.setBackground(GameTheme.Colors.cardBackground());
                tip.setForeground(GameTheme.Colors.textPrimary());
                tip.setFont(FontManager.getBodySmall(Font.PLAIN));
            }
        } catch (Exception e) {
            System.err.println("[ThemeApplier] Failed to apply to component " + c.getClass().getName() + ": " + e.getMessage());
        }
    }

    public static Border createCardBorder() {
        return new CompoundBorder(new EmptyBorder(8, 8, 8, 8), BorderFactory.createLineBorder(GameTheme.Colors.cardBorder()));
    }

    public static Border createInputBorder() {
        return BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(GameTheme.Colors.cardBorder()), new EmptyBorder(6, 8, 6, 8));
    }

    public static Border createButtonBorder() {
        return BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(GameTheme.Colors.cardBorder()), new EmptyBorder(6, 12, 6, 12));
    }

}
