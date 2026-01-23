package labyrinth.client.ui.Styles;

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;

/**
 * Custom themed tooltip manager for the Labyrinth game.
 * Provides medieval-styled tooltips that match the game's visual theme.
 */
public class StyledTooltipManager {

    private static final int SHOW_DELAY_MS = 500;
    private static final int HIDE_DELAY_MS = 100;
    private static final int PADDING = 12;
    private static final int ARC = 10;

    private static JWindow currentTooltip;
    private static Timer showTimer;
    private static Timer hideTimer;
    private static JComponent currentComponent;

    /**
     * Set a simple text tooltip for a component.
     */
    public static void setTooltip(JComponent component, String text) {
        setTooltip(component, null, text);
    }

    /**
     * Set a tooltip with title and description for a component.
     */
    public static void setTooltip(JComponent component, String title, String description) {
        // Remove any existing tooltip listener
        for (var listener : component.getMouseListeners()) {
            if (listener instanceof TooltipMouseListener) {
                component.removeMouseListener(listener);
            }
        }

        // Add new tooltip listener
        component.addMouseListener(new TooltipMouseListener(title, description));
    }

    /**
     * Get the tooltip text for a component (used by context menu).
     */
    public static String getTooltipText(JComponent component) {
        for (var listener : component.getMouseListeners()) {
            if (listener instanceof TooltipMouseListener tml) {
                return tml.getFullText();
            }
        }
        return null;
    }

    private static class TooltipMouseListener extends MouseAdapter {
        private final String title;
        private final String description;

        TooltipMouseListener(String title, String description) {
            this.title = title;
            this.description = description;
        }

        String getFullText() {
            if (title != null && !title.isEmpty()) {
                return title + ": " + description;
            }
            return description;
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            JComponent source = (JComponent) e.getSource();
            scheduleShow(source, title, description, e.getLocationOnScreen());
        }

        @Override
        public void mouseExited(MouseEvent e) {
            scheduleHide();
        }

        @Override
        public void mousePressed(MouseEvent e) {
            hideTooltip();
        }
    }

    private static void scheduleShow(JComponent component, String title, String description, Point mouseLocation) {
        cancelTimers();
        currentComponent = component;

        showTimer = new Timer(SHOW_DELAY_MS, e -> {
            showTooltip(component, title, description, mouseLocation);
        });
        showTimer.setRepeats(false);
        showTimer.start();
    }

    private static void scheduleHide() {
        cancelTimers();

        hideTimer = new Timer(HIDE_DELAY_MS, e -> {
            hideTooltip();
        });
        hideTimer.setRepeats(false);
        hideTimer.start();
    }

    private static void cancelTimers() {
        if (showTimer != null) {
            showTimer.stop();
            showTimer = null;
        }
        if (hideTimer != null) {
            hideTimer.stop();
            hideTimer = null;
        }
    }

    private static boolean supportsTranslucency() {
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice gd = ge.getDefaultScreenDevice();
        return gd.isWindowTranslucencySupported(GraphicsDevice.WindowTranslucency.TRANSLUCENT);
    }

    private static void showTooltip(JComponent component, String title, String description, Point mouseLocation) {
        hideTooltip();

        if (component == null || !component.isShowing()) {
            return;
        }

        // Create tooltip window
        Window owner = SwingUtilities.getWindowAncestor(component);
        currentTooltip = new JWindow(owner);
        currentTooltip.setBackground(new Color(0, 0, 0, 0));

        // Create tooltip content panel
        TooltipPanel panel = new TooltipPanel(title, description);
        currentTooltip.add(panel);
        currentTooltip.pack();

        // Position tooltip near mouse but ensure it stays on screen
        Point location = calculateTooltipPosition(component, currentTooltip.getSize(), mouseLocation);
        currentTooltip.setLocation(location);

        // Show with fade-in effect if translucency is supported, otherwise show immediately
        if (supportsTranslucency()) {
            currentTooltip.setOpacity(0f);
            currentTooltip.setVisible(true);

            Timer fadeIn = new Timer(16, null);
            fadeIn.addActionListener(e -> {
                if (currentTooltip == null) {
                    fadeIn.stop();
                    return;
                }
                float opacity = currentTooltip.getOpacity();
                if (opacity < 1f) {
                    currentTooltip.setOpacity(Math.min(1f, opacity + 0.15f));
                } else {
                    fadeIn.stop();
                }
            });
            fadeIn.start();
        } else {
            // No translucency support - show immediately without fade
            currentTooltip.setVisible(true);
        }
    }

    private static void hideTooltip() {
        if (currentTooltip != null) {
            currentTooltip.dispose();
            currentTooltip = null;
        }
        currentComponent = null;
    }

    private static Point calculateTooltipPosition(JComponent component, Dimension tooltipSize, Point mouseLocation) {
        // Get screen bounds
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        Rectangle screenBounds = ge.getMaximumWindowBounds();

        // Default position: below and slightly to the right of mouse
        int x = mouseLocation.x + 15;
        int y = mouseLocation.y + 20;

        // Adjust if tooltip would go off right edge
        if (x + tooltipSize.width > screenBounds.x + screenBounds.width) {
            x = mouseLocation.x - tooltipSize.width - 5;
        }

        // Adjust if tooltip would go off bottom edge
        if (y + tooltipSize.height > screenBounds.y + screenBounds.height) {
            y = mouseLocation.y - tooltipSize.height - 5;
        }

        // Ensure tooltip doesn't go off left or top edge
        x = Math.max(screenBounds.x, x);
        y = Math.max(screenBounds.y, y);

        return new Point(x, y);
    }

    /**
     * Force show a tooltip immediately (used by context menu "Kontext" action).
     */
    public static void showTooltipNow(JComponent component) {
        String text = getTooltipText(component);
        if (text != null) {
            Point location = component.getLocationOnScreen();
            location.x += component.getWidth() / 2;
            location.y += component.getHeight();
            showTooltip(component, null, text, location);
        }
    }

    /**
     * Custom panel for rendering the tooltip with medieval theme.
     */
    private static class TooltipPanel extends JPanel {
        private final String title;
        private final String description;

        TooltipPanel(String title, String description) {
            this.title = title;
            this.description = description;
            setOpaque(false);
            calculateSize();
        }

        private void calculateSize() {
            FontMetrics titleFm = getFontMetrics(FontManager.getBodySmall(Font.BOLD));
            FontMetrics descFm = getFontMetrics(FontManager.getBodySmall());

            int width = PADDING * 2;
            int height = PADDING * 2;

            if (title != null && !title.isEmpty()) {
                width = Math.max(width, titleFm.stringWidth(title) + PADDING * 2);
                height += titleFm.getHeight() + 4;
            }

            if (description != null && !description.isEmpty()) {
                // Handle multi-line descriptions
                String[] lines = description.split("\n");
                for (String line : lines) {
                    width = Math.max(width, descFm.stringWidth(line) + PADDING * 2);
                    height += descFm.getHeight();
                }
            }

            // Limit max width
            width = Math.min(width, 350);

            setPreferredSize(new Dimension(width, height));
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int w = getWidth();
            int h = getHeight();

            // Shadow
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.shadow(), 80));
            g2.fill(new RoundRectangle2D.Float(3, 3, w - 3, h - 3, ARC, ARC));

            // Background
            Color bgColor = ThemeManager.getInstance().getCardBackground();
            g2.setColor(bgColor);
            g2.fill(new RoundRectangle2D.Float(0, 0, w - 3, h - 3, ARC, ARC));

            // Border
            g2.setColor(GameTheme.Colors.ACCENT_GOLD);
            g2.setStroke(new BasicStroke(1.5f));
            g2.draw(new RoundRectangle2D.Float(1, 1, w - 5, h - 5, ARC, ARC));

            // Inner highlight
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 20));
            g2.drawLine(PADDING, 3, w - PADDING - 3, 3);

            // Draw text
            int y = PADDING;

            if (title != null && !title.isEmpty()) {
                g2.setFont(FontManager.getBodySmall(Font.BOLD));
                g2.setColor(GameTheme.Colors.ACCENT_GOLD);
                FontMetrics fm = g2.getFontMetrics();
                g2.drawString(title, PADDING, y + fm.getAscent());
                y += fm.getHeight() + 4;
            }

            if (description != null && !description.isEmpty()) {
                g2.setFont(FontManager.getBodySmall());
                g2.setColor(ThemeManager.getInstance().getTextPrimary());
                FontMetrics fm = g2.getFontMetrics();

                String[] lines = description.split("\n");
                for (String line : lines) {
                    g2.drawString(line, PADDING, y + fm.getAscent());
                    y += fm.getHeight();
                }
            }

            g2.dispose();
        }
    }
}
