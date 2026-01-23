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
                setBackground(isSelected ? GameTheme.Colors.PRIMARY_GOLD_DARK : GameTheme.Colors.STONE_MEDIUM);
                setForeground(GameTheme.Colors.TEXT_LIGHT);
                setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
                return this;
            }
        });
    }
}
