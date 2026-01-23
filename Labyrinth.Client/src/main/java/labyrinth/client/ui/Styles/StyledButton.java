package labyrinth.client.ui.Styles;

// --------------------------------------------------------------------------------
// Styled Button
// --------------------------------------------------------------------------------

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;

public class StyledButton extends JButton {
    public enum Style { PRIMARY, SECONDARY, DANGER }

    private final Style style;
    private float hoverProgress = 0f;
    private boolean isHovered = false;
    private boolean isFocused = false;

    public StyledButton(String text, Style style) {
        super(text);
        this.style = style;

        setFont(FontManager.getBodyMedium());
        setForeground(GameTheme.Colors.TEXT_LIGHT);
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

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        int w = getWidth();
        int h = getHeight();
        int arc = 10;

        Color bgStart, bgEnd, borderColor;
        switch (style) {
            case PRIMARY -> {
                bgStart = interpolate(GameTheme.Colors.PLAYER_GREEN.darker(), GameTheme.Colors.PLAYER_GREEN, hoverProgress);
                bgEnd = interpolate(GameTheme.Colors.PLAYER_GREEN, GameTheme.Colors.PLAYER_GREEN.brighter(), hoverProgress);
                borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
            }
            case DANGER -> {
                bgStart = interpolate(GameTheme.Colors.PLAYER_RED.darker(), GameTheme.Colors.PLAYER_RED, hoverProgress);
                bgEnd = interpolate(GameTheme.Colors.PLAYER_RED, GameTheme.Colors.PLAYER_RED.brighter(), hoverProgress);
                borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
            }
            default -> {
                bgStart = interpolate(GameTheme.Colors.STONE_DARK, GameTheme.Colors.STONE_DARK.darker(), hoverProgress);
                bgEnd = interpolate(GameTheme.Colors.STONE_MEDIUM, GameTheme.Colors.STONE_MEDIUM.brighter(), hoverProgress);
                borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
            }
        }

        if (!isEnabled()) {
            bgStart = GameTheme.Colors.STONE_MEDIUM.darker();
            bgEnd = GameTheme.Colors.STONE_MEDIUM;
            borderColor = GameTheme.Colors.STONE_DARK;
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

        // Focus indicator - glowing outline
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
        g2.setFont(getFont());
        FontMetrics fm = g2.getFontMetrics();
        int textX = (w - fm.stringWidth(getText())) / 2;
        int textY = (h + fm.getAscent() - fm.getDescent()) / 2;

        g2.setColor(isEnabled() ? getForeground() : GameTheme.Colors.TEXT_MUTED);
        g2.drawString(getText(), textX, textY);

        g2.dispose();
    }

    private Color interpolate(Color c1, Color c2, float t) {
        return new Color(
                (int) (c1.getRed() + (c2.getRed() - c1.getRed()) * t),
                (int) (c1.getGreen() + (c2.getGreen() - c1.getGreen()) * t),
                (int) (c1.getBlue() + (c2.getBlue() - c1.getBlue()) * t)
        );
    }
}
