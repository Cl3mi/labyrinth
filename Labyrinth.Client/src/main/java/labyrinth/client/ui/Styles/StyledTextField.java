package labyrinth.client.ui.Styles;

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import java.awt.*;

public class StyledTextField extends JTextField {

    public StyledTextField(String text) {
        super(text);
        setupStyle();
    }

    public StyledTextField() {
        super();
        setupStyle();
    }

    private void setupStyle() {
        setFont(FontManager.getBodySmall());
        setBackground(GameTheme.Colors.STONE_MEDIUM);
        setForeground(GameTheme.Colors.TEXT_LIGHT);
        setCaretColor(GameTheme.Colors.TEXT_LIGHT);
        Border line = new LineBorder(GameTheme.Colors.CARD_BORDER, 1);
        Border empty = new EmptyBorder(5, 8, 5, 8);
        setBorder(new CompoundBorder(line, empty));
        setPreferredSize(new Dimension(150, 30));
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Fill background
        g2.setColor(getBackground());
        g2.fillRect(0, 0, getWidth(), getHeight());

        super.paintComponent(g2);
        g2.dispose();
    }
}
