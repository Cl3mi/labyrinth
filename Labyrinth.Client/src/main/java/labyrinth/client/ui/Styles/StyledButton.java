package labyrinth.client.ui.Styles;

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;

public class StyledButton extends JButton {
    public enum Style { PRIMARY, SECONDARY, DANGER, MENU, DIALOG_PRIMARY, DIALOG_SECONDARY, DIALOG_DANGER }

    private final Style style;
    private String subtitle;
    private float hoverProgress = 0f;
    private boolean isHovered = false;
    private boolean isFocused = false;

    public StyledButton(String text, Style style) {
        this(text, null, style);
    }

    public StyledButton(String text, String subtitle, Style style) {
        super(text);
        this.subtitle = subtitle;
        this.style = style;

        // Font and foreground are now determined in paintComponent based on style
        setFocusPainted(false);
        setBorderPainted(false);
        setContentAreaFilled(false);
        setCursor(new Cursor(Cursor.HAND_CURSOR));
        setFocusable(true);

        Timer animTimer = new Timer(16, e -> {
            if (isHovered && hoverProgress < 1f) {
                hoverProgress = Math.min(1f, hoverProgress + 0.12f);
                repaint();
            } else if (!isHovered && hoverProgress > 0f) {
                hoverProgress = Math.max(0f, hoverProgress - 0.12f);
                repaint();
            }
        });
        animTimer.start();

        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) { isHovered = true; }
            @Override
            public void mouseExited(MouseEvent e) { isHovered = false; }
        });

        addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent e) {
                isFocused = true;
                repaint();
            }
            @Override
            public void focusLost(FocusEvent e) {
                isFocused = false;
                repaint();
            }
        });
    }

    public void setSubtitle(String subtitle) {
        this.subtitle = subtitle;
        repaint();
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

        int w = getWidth();
        int h = getHeight();
        int arc = 10;

        if (style == Style.MENU) {
            paintMenuButton(g2, w, h);
        } else {
            paintDefaultButton(g2, w, h);
        }

        g2.dispose();
    }

    private void paintDefaultButton(Graphics2D g2, int w, int h) {
        int arc = 10;
        Color bgStart, bgEnd, borderColor;
        Color currentForegroundColor;
        Font currentFont;

        switch (style) {
            case PRIMARY -> {
                bgStart = interpolate(GameTheme.Colors.PLAYER_GREEN.darker(), GameTheme.Colors.PLAYER_GREEN, hoverProgress);
                bgEnd = interpolate(GameTheme.Colors.PLAYER_GREEN, GameTheme.Colors.PLAYER_GREEN.brighter(), hoverProgress);
                borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
                currentFont = FontManager.getBodyMedium();
                currentForegroundColor = isEnabled() ? GameTheme.Colors.TEXT_LIGHT : GameTheme.Colors.TEXT_MUTED;
            }
            case DANGER -> {
                bgStart = interpolate(GameTheme.Colors.PLAYER_RED.darker(), GameTheme.Colors.PLAYER_RED, hoverProgress);
                bgEnd = interpolate(GameTheme.Colors.PLAYER_RED, GameTheme.Colors.PLAYER_RED.brighter(), hoverProgress);
                borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
                currentFont = FontManager.getBodyMedium();
                currentForegroundColor = isEnabled() ? GameTheme.Colors.TEXT_LIGHT : GameTheme.Colors.TEXT_MUTED;
            }
            case DIALOG_PRIMARY -> {
                Color baseColor = GameTheme.Colors.PLAYER_GREEN;
                bgStart = isHovered ? baseColor.brighter() : baseColor;
                bgEnd = bgStart; // Solid color
                borderColor = baseColor.brighter();
                currentFont = FontManager.getBodySmall(Font.BOLD);
                currentForegroundColor = GameTheme.Colors.TEXT_LIGHT;
            }
            case DIALOG_DANGER -> {
                Color baseColor = GameTheme.Colors.PLAYER_RED;
                bgStart = isHovered ? baseColor.brighter() : baseColor;
                bgEnd = bgStart; // Solid color
                borderColor = baseColor.brighter();
                currentFont = FontManager.getBodySmall(Font.BOLD);
                currentForegroundColor = GameTheme.Colors.TEXT_LIGHT;
            }
            case DIALOG_SECONDARY -> {
                Color baseColor = GameTheme.Colors.STONE_DARK;
                bgStart = isHovered ? GameTheme.Colors.STONE_MEDIUM : baseColor;
                bgEnd = bgStart; // Solid color
                borderColor = GameTheme.Colors.STONE_MEDIUM;
                currentFont = FontManager.getBodySmall(Font.BOLD);
                currentForegroundColor = GameTheme.Colors.TEXT_LIGHT;
            }
            default -> { // SECONDARY
                bgStart = interpolate(GameTheme.Colors.STONE_DARK, GameTheme.Colors.STONE_DARK.darker(), hoverProgress);
                bgEnd = interpolate(GameTheme.Colors.STONE_MEDIUM, GameTheme.Colors.STONE_MEDIUM.brighter(), hoverProgress);
                borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
                currentFont = FontManager.getBodyMedium();
                currentForegroundColor = isEnabled() ? GameTheme.Colors.TEXT_LIGHT : GameTheme.Colors.TEXT_MUTED;
            }
        }

        if (!isEnabled()) {
            bgStart = GameTheme.Colors.STONE_MEDIUM.darker();
            bgEnd = GameTheme.Colors.STONE_MEDIUM;
            borderColor = GameTheme.Colors.STONE_DARK;
            currentForegroundColor = GameTheme.Colors.TEXT_MUTED;
        }

        // Shadow
        g2.setColor(ThemeManager.getInstance().getShadow());
        g2.fill(new RoundRectangle2D.Float(3, 4, w - 6, h - 6, arc, arc));

        // Background
        g2.setPaint(new GradientPaint(0, 0, bgStart, 0, h, bgEnd));
        g2.fill(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc, arc));

        // Border
        g2.setColor(borderColor);
        g2.setStroke(new BasicStroke(2f));
        g2.draw(new RoundRectangle2D.Float(1, 1, w - 3, h - 3, arc, arc));

        // Focus indicator
        if (isFocused && isEnabled()) {
            Color focusColor = GameTheme.Colors.ACCENT_GOLD;
            g2.setColor(new Color(focusColor.getRed(), focusColor.getGreen(), focusColor.getBlue(), 80));
            g2.setStroke(new BasicStroke(4f));
            g2.draw(new RoundRectangle2D.Float(-1, -1, w + 1, h + 1, arc + 4, arc + 4));
            g2.setColor(new Color(focusColor.getRed(), focusColor.getGreen(), focusColor.getBlue(), 200));
            g2.setStroke(new BasicStroke(2f));
            g2.draw(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc + 2, arc + 2));
        }

        // Text
        g2.setFont(currentFont);
        FontMetrics fm = g2.getFontMetrics();
        int textX = (w - fm.stringWidth(getText())) / 2;
        int textY = (h + fm.getAscent() - fm.getDescent()) / 2;

        g2.setColor(currentForegroundColor);
        g2.drawString(getText(), textX, textY);
    }

    private void paintMenuButton(Graphics2D g2, int w, int h) {
        int arc = 10;

        // shadow
        g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.shadow(), (int) (70 + 30 * hoverProgress)));
        g2.fill(new RoundRectangle2D.Float(4, 5, w - 8, h - 7, arc, arc));

        // background
        Color bgStart = interpolate(GameTheme.Colors.stoneDark(), GameTheme.Colors.stoneDark().darker(), hoverProgress);
        Color bgEnd = interpolate(GameTheme.Colors.stoneMedium(), GameTheme.Colors.stoneMedium().darker(), hoverProgress);
        GradientPaint bgGradient = new GradientPaint(0, 0, bgStart, 0, h, bgEnd);
        g2.setPaint(bgGradient);
        g2.fill(new RoundRectangle2D.Float(2, 2, w - 4, h - 4, arc, arc));

        // golden border
        Color borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
        g2.setColor(borderColor);
        g2.setStroke(new BasicStroke(2f + hoverProgress * 0.5f));
        g2.draw(new RoundRectangle2D.Float(2, 2, w - 5, h - 5, arc, arc));

        // Focus indicator
        if (isFocused) {
            g2.setColor(new Color(GameTheme.Colors.PRIMARY_GOLD_LIGHT.getRed(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getGreen(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getBlue(), 80));
            g2.setStroke(new BasicStroke(4f));
            g2.draw(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc + 4, arc + 4));
            g2.setColor(new Color(GameTheme.Colors.PRIMARY_GOLD_LIGHT.getRed(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getGreen(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getBlue(), 200));
            g2.setStroke(new BasicStroke(2f));
            g2.draw(new RoundRectangle2D.Float(1, 1, w - 3, h - 3, arc + 2, arc + 2));
        }

        // glow
        g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), (int) (15 + 20 * hoverProgress)));
        g2.fill(new RoundRectangle2D.Float(4, 4, w - 8, (h - 8) / 3f, arc - 2, arc - 2));

        // text
        g2.setFont(FontManager.getBodyMedium(Font.BOLD));
        FontMetrics fm = g2.getFontMetrics();
        String text = getText();
        int textWidth = fm.stringWidth(text);
        int textX = (w - textWidth) / 2;
        int textY = h / 2 - 3;

        g2.setColor(GameTheme.Colors.shadow());
        g2.drawString(text, textX + 2, textY + 2);

        Color textColor = interpolate(GameTheme.Colors.textLight(), GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress * 0.4f);
        g2.setColor(textColor);
        g2.drawString(text, textX, textY);

        // subtitle
        if (subtitle != null && !subtitle.isEmpty()) {
            g2.setFont(FontManager.getBodySmall());
            fm = g2.getFontMetrics();
            int subWidth = fm.stringWidth(subtitle);
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textMuted(), (int) (140 + 40 * hoverProgress)));
            g2.drawString(subtitle, (w - subWidth) / 2, textY + 18);
        }
    }

    private Color interpolate(Color c1, Color c2, float t) {
        return new Color(
                (int) (c1.getRed() + (c2.getRed() - c1.getRed()) * t),
                (int) (c1.getGreen() + (c2.getGreen() - c1.getGreen()) * t),
                (int) (c1.getBlue() + (c2.getBlue() - c1.getBlue()) * t),
                (int) (c1.getAlpha() + (c2.getAlpha() - c1.getAlpha()) * t)
        );
    }
}
