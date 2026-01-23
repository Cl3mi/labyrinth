package labyrinth.client.ui.Styles;

// --------------------------------------------------------------------------------
// Styled Player Card Renderer
// --------------------------------------------------------------------------------

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.geom.RoundRectangle2D;

public class StyledPlayerCardRenderer extends JPanel implements ListCellRenderer<String> {
    private final JLabel nameLabel;
    private final JLabel statusLabel;
    private final JLabel badgeLabel;
    private final JPanel avatarPanel;

    // Brighter player colors for better visibility in dark theme
    //    private static final Color[] PLAYER_COLORS = {
    //            new Color(220, 90, 90),    // Bright red
    //            new Color(90, 200, 90),    // Bright green
    //            new Color(100, 160, 230),  // Bright blue
    //            new Color(240, 210, 80)    // Bright yellow
    //    };
    // use theme player colors (can adjust brightness per rendering)
    private static Color playerColor(int index) {
        return GameTheme.Colors.getPlayerColor(index).brighter();
    }

    public StyledPlayerCardRenderer() {
        setLayout(new BorderLayout(12, 0));
        setOpaque(false);
        setBorder(new EmptyBorder(8, 12, 8, 12));

        avatarPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setColor(getBackground());
                g2.fillOval(2, 2, 46, 46);
                g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 60));
                g2.setStroke(new BasicStroke(2));
                g2.drawOval(2, 2, 46, 46);
                g2.dispose();
            }
        };
        avatarPanel.setPreferredSize(new Dimension(50, 50));
        avatarPanel.setOpaque(false);

        JPanel infoPanel = new JPanel();
        infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));
        infoPanel.setOpaque(false);

        nameLabel = new JLabel();
        nameLabel.setFont(FontManager.getBodyMedium(Font.BOLD));
        nameLabel.setForeground(GameTheme.Colors.TEXT_LIGHT);

        statusLabel = new JLabel();
        statusLabel.setFont(FontManager.getBodySmall(Font.PLAIN));

        infoPanel.add(nameLabel);
        infoPanel.add(Box.createVerticalStrut(3));
        infoPanel.add(statusLabel);

        badgeLabel = new JLabel();
        badgeLabel.setFont(FontManager.getBodySmall(Font.BOLD));
        badgeLabel.setVerticalAlignment(SwingConstants.TOP);

        add(avatarPanel, BorderLayout.WEST);
        add(infoPanel, BorderLayout.CENTER);
        add(badgeLabel, BorderLayout.EAST);
    }

    @Override
    public Component getListCellRendererComponent(JList<? extends String> list, String value,
                                                  int index, boolean isSelected, boolean cellHasFocus) {
        boolean isOffline = value != null && value.contains("[OFFLINE]");
        boolean isAdmin = value != null && value.contains("(Admin)");
        boolean isYou = value != null && value.contains("(Du)");

        String cleanName = value;
        if (cleanName != null) {
            cleanName = cleanName.replace("[OFFLINE] ", "")
                    .replace("(Admin) ", "")
                    .replace(" (Du)", "").trim();
        }

        avatarPanel.setBackground(isOffline ? GameTheme.Colors.STONE_MEDIUM : playerColor(index));

        nameLabel.setText(cleanName);
        nameLabel.setForeground(isOffline ? GameTheme.Colors.TEXT_MUTED : GameTheme.Colors.TEXT_LIGHT);

        statusLabel.setText(isOffline ? "- Offline" : "- Bereit");
        statusLabel.setForeground(isOffline ? GameTheme.Colors.PLAYER_RED : GameTheme.Colors.PLAYER_GREEN);

        StringBuilder badges = new StringBuilder("<html><div style='text-align:right'>");
        if (isAdmin) badges.append("<span style='color:#FFD700'>[Admin]</span><br>");
        if (isYou) badges.append("<span style='color:#90EE90'>[Du]</span>");
        badges.append("</div></html>");
        badgeLabel.setText(badges.toString());

        return this;
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Slightly brighter background for better visibility
        g2.setColor(ThemeEffects.withAlpha(ThemeManager.getInstance().getCardBackground(), 200));
        g2.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 10, 10));

        // Brighter border
        g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.cardBorder(), 150));
        g2.setStroke(new BasicStroke(1.5f));
        g2.draw(new RoundRectangle2D.Float(0, 0, getWidth() - 1, getHeight() - 1, 10, 10));

        g2.dispose();
        super.paintComponent(g);
    }
}
