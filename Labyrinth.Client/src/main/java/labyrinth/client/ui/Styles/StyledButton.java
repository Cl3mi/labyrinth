package labyrinth.client.ui.Styles;

// --------------------------------------------------------------------------------
// Styled Button
// --------------------------------------------------------------------------------

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;

public class StyledButton extends JButton {
    public enum Style { PRIMARY, SECONDARY, DANGER }

    private final Style style;
    private float hoverProgress = 0f;
    private boolean isHovered = false;

    public StyledButton(String text, Style style) {
        super(text);
        this.style = style;

        setFont(ColorsFonts.buttonFont);
        setForeground(ColorsFonts.TEXT_LIGHT);
        setFocusPainted(false);
        setBorderPainted(false);
        setContentAreaFilled(false);
        setCursor(new Cursor(Cursor.HAND_CURSOR));

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
                bgStart = interpolate(new Color(60, 100, 60), new Color(80, 130, 80), hoverProgress);
                bgEnd = interpolate(new Color(40, 70, 40), new Color(60, 100, 60), hoverProgress);
                borderColor = interpolate(new Color(100, 160, 100), new Color(130, 200, 130), hoverProgress);
            }
            case DANGER -> {
                bgStart = interpolate(new Color(120, 50, 50), new Color(150, 70, 70), hoverProgress);
                bgEnd = interpolate(new Color(80, 30, 30), new Color(100, 50, 50), hoverProgress);
                borderColor = interpolate(new Color(180, 80, 80), new Color(220, 100, 100), hoverProgress);
            }
            default -> {
                bgStart = interpolate(ColorsFonts.STONE_DARK, new Color(65, 55, 45), hoverProgress);
                bgEnd = interpolate(ColorsFonts.STONE_MEDIUM, new Color(90, 75, 60), hoverProgress);
                borderColor = interpolate(ColorsFonts.PRIMARY_GOLD_DARK, ColorsFonts.PRIMARY_GOLD_LIGHT, hoverProgress);
            }
        }

        if (!isEnabled()) {
            bgStart = new Color(60, 60, 60);
            bgEnd = new Color(50, 50, 50);
            borderColor = new Color(80, 80, 80);
        }

        // Shadow
        g2.setColor(new Color(0, 0, 0, 60));
        g2.fill(new RoundRectangle2D.Float(3, 4, w - 6, h - 6, arc, arc));

        // Background
        g2.setPaint(new GradientPaint(0, 0, bgStart, 0, h, bgEnd));
        g2.fill(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc, arc));

        // Border
        g2.setColor(borderColor);
        g2.setStroke(new BasicStroke(2f));
        g2.draw(new RoundRectangle2D.Float(1, 1, w - 3, h - 3, arc, arc));

        // Text
        g2.setFont(getFont());
        FontMetrics fm = g2.getFontMetrics();
        int textX = (w - fm.stringWidth(getText())) / 2;
        int textY = (h + fm.getAscent() - fm.getDescent()) / 2;

        g2.setColor(isEnabled() ? getForeground() : new Color(120, 120, 120));
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


