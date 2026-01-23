package labyrinth.client.ui.Styles;

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;

/**
 * Custom themed context menu for the Labyrinth game.
 * Provides medieval-styled right-click menus with "Kontext" and "Abbrechen" options.
 */
public class StyledContextMenu extends JPopupMenu {

    private final JComponent targetComponent;
    private final String tooltipText;

    /**
     * Create a context menu for a component.
     *
     * @param targetComponent The component this menu is attached to
     * @param tooltipText     The tooltip text to show when "Kontext" is clicked
     */
    public StyledContextMenu(JComponent targetComponent, String tooltipText) {
        this.targetComponent = targetComponent;
        this.tooltipText = tooltipText;

        setOpaque(false);
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        // Add menu items
        add(createMenuItem("Kontext", "ℹ", this::showContext));
        add(createMenuItem("Abbrechen", "✕", this::cancel));

        // Register for theme changes
        ThemeManager.getInstance().addThemeChangeListener(this::repaint);
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        int w = getWidth();
        int h = getHeight();
        int arc = 10;

        // Shadow
        g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.shadow(), 80));
        g2.fill(new RoundRectangle2D.Float(4, 4, w - 4, h - 4, arc, arc));

        // Background
        g2.setColor(ThemeManager.getInstance().getSurfacePrimary());
        g2.fill(new RoundRectangle2D.Float(0, 0, w - 4, h - 4, arc, arc));

        // Border
        g2.setColor(GameTheme.Colors.ACCENT_GOLD);
        g2.setStroke(new BasicStroke(1.5f));
        g2.draw(new RoundRectangle2D.Float(1, 1, w - 6, h - 6, arc, arc));

        // Inner highlight
        g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 15));
        g2.drawLine(10, 3, w - 14, 3);

        g2.dispose();
    }

    private JMenuItem createMenuItem(String text, String icon, Runnable action) {
        JMenuItem item = new JMenuItem(icon + "  " + text) {
            private boolean isHovered = false;

            {
                setOpaque(false);
                setFont(FontManager.getBodyMedium(Font.PLAIN));
                setForeground(ThemeManager.getInstance().getTextPrimary());
                setBorder(new EmptyBorder(8, 12, 8, 12));
                setCursor(new Cursor(Cursor.HAND_CURSOR));

                addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseEntered(MouseEvent e) {
                        isHovered = true;
                        repaint();
                    }

                    @Override
                    public void mouseExited(MouseEvent e) {
                        isHovered = false;
                        repaint();
                    }
                });
            }

            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

                int w = getWidth();
                int h = getHeight();

                if (isHovered) {
                    // Hover background
                    g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 40));
                    g2.fillRoundRect(4, 2, w - 8, h - 4, 6, 6);
                }

                // Draw text
                g2.setFont(getFont());
                FontMetrics fm = g2.getFontMetrics();

                // Text color changes on hover
                if (isHovered) {
                    g2.setColor(GameTheme.Colors.ACCENT_GOLD);
                } else {
                    g2.setColor(ThemeManager.getInstance().getTextPrimary());
                }

                int textX = 12;
                int textY = (h + fm.getAscent() - fm.getDescent()) / 2;
                g2.drawString(getText(), textX, textY);

                g2.dispose();
            }
        };

        item.addActionListener(e -> action.run());
        return item;
    }

    private void showContext() {
        setVisible(false);
        if (tooltipText != null && !tooltipText.isEmpty()) {
            StyledTooltipManager.showTooltipNow(targetComponent);
        }
    }

    private void cancel() {
        setVisible(false);
    }

    /**
     * Attach a context menu to a component.
     * The tooltip text is automatically retrieved from StyledTooltipManager if available.
     *
     * @param component The component to attach the menu to
     */
    public static void attachTo(JComponent component) {
        String tooltip = StyledTooltipManager.getTooltipText(component);
        attachTo(component, tooltip);
    }

    /**
     * Attach a context menu to a component with a specific tooltip text.
     *
     * @param component   The component to attach the menu to
     * @param tooltipText The tooltip text to show when "Kontext" is clicked
     */
    public static void attachTo(JComponent component, String tooltipText) {
        component.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showContextMenu(e, component, tooltipText);
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showContextMenu(e, component, tooltipText);
                }
            }
        });
    }

    private static void showContextMenu(MouseEvent e, JComponent component, String tooltipText) {
        StyledContextMenu menu = new StyledContextMenu(component, tooltipText);
        menu.show(e.getComponent(), e.getX(), e.getY());
    }

    /**
     * Attach context menu to a JList for list items.
     *
     * @param list            The JList to attach to
     * @param tooltipProvider Function to get tooltip text for each item
     */
    public static void attachToList(JList<?> list, java.util.function.Function<Object, String> tooltipProvider) {
        list.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showListContextMenu(e, list, tooltipProvider);
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showListContextMenu(e, list, tooltipProvider);
                }
            }
        });
    }

    private static void showListContextMenu(MouseEvent e, JList<?> list, java.util.function.Function<Object, String> tooltipProvider) {
        int index = list.locationToIndex(e.getPoint());
        if (index >= 0) {
            list.setSelectedIndex(index);
            Object item = list.getModel().getElementAt(index);
            String tooltip = tooltipProvider.apply(item);

            // Create a temporary component for the tooltip
            JComponent tempComponent = new JPanel();
            tempComponent.setBounds(list.getCellBounds(index, index));

            StyledContextMenu menu = new StyledContextMenu(list, tooltip);
            menu.show(e.getComponent(), e.getX(), e.getY());
        }
    }
}
