package labyrinth.client.ui.Styles;

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;

import javax.swing.*;
import java.awt.*;

public class StyledComboBox<E> extends JComboBox<E> {

    public StyledComboBox() {
        super();
        setupStyle();
    }

    private void setupStyle() {
        setFont(FontManager.getBodySmall());
        setBackground(GameTheme.Colors.STONE_MEDIUM);
        setForeground(GameTheme.Colors.TEXT_LIGHT);
        setPreferredSize(new Dimension(150, 30));

        setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value,
                                                          int index, boolean isSelected, boolean cellHasFocus) {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (isEnabled()) {
                    setBackground(isSelected ? GameTheme.Colors.PRIMARY_GOLD_DARK : GameTheme.Colors.STONE_MEDIUM);
                    setForeground(GameTheme.Colors.TEXT_LIGHT);
                } else {
                    setBackground(GameTheme.Colors.STONE_DARK);
                    setForeground(GameTheme.Colors.TEXT_MUTED);
                }
                setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
                return this;
            }
        });
    }

    @Override
    protected void paintComponent(Graphics g) {
        if (!isEnabled()) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            g2.setColor(GameTheme.Colors.STONE_DARK);
            g2.fillRoundRect(0, 0, getWidth(), getHeight(), 8, 8);

            g2.setColor(GameTheme.Colors.TEXT_MUTED);
            g2.setFont(getFont());
            FontMetrics fm = g2.getFontMetrics();
            String text = getSelectedItem() != null ? getSelectedItem().toString() : "";
            int textX = 10;
            int textY = (getHeight() - fm.getHeight()) / 2 + fm.getAscent();
            g2.drawString(text, textX, textY);

            // Draw disabled arrow
            int arrowX = getWidth() - 20;
            int arrowY = getHeight() / 2 - 2;
            g2.setColor(GameTheme.Colors.TEXT_MUTED.darker()); // Use a darker muted color for the arrow
            g2.fillPolygon(new int[]{arrowX, arrowX + 10, arrowX + 5}, new int[]{arrowY, arrowY, arrowY + 5}, 3);

            g2.dispose();
        } else {
            super.paintComponent(g);
        }
    }
}
