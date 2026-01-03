package labyrinth.client.ui;

import labyrinth.client.ui.theme.FontManager;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

public class ServerBrowserPanel extends JPanel {

    private Runnable onBackToMenu;
    private Image backgroundImage;

    private final JButton leaveLobbyButton;
    private final JButton connectButton;

    public ServerBrowserPanel() {
        loadBackgroundImage();

        setOpaque(false);
        setLayout(new BorderLayout());
        setBorder(new EmptyBorder(10, 10, 10, 10));

        JPanel header = new JPanel(new BorderLayout());
        header.setOpaque(false);


        JPanel footer = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        footer.setOpaque(false);

        connectButton = new JButton("Verbinden");
        connectButton.setFont(FontManager.getLargeUI());
        connectButton.setEnabled(false);
        connectButton.addActionListener(e -> onStartGameClicked());

        leaveLobbyButton = new JButton("Lobby verlassen");
        leaveLobbyButton.setFont(FontManager.getMediumUIBold());
        leaveLobbyButton.setVisible(false); // Initially hidden
        leaveLobbyButton.addActionListener(e -> onLeaveServerBrowser());

        footer.add(leaveLobbyButton);
        footer.add(connectButton);
        add(footer, BorderLayout.SOUTH);
    }

    public void setOnBackToMenu(Runnable callback) {
        this.onBackToMenu = callback;
    }

    public void onShow() {

    }

    private void onLeaveServerBrowser() {
        //TODO
    }

    private void onStartGameClicked() {
        //TODO
    }

    @Override
    protected void paintComponent(Graphics g) {
        if (backgroundImage != null) {
            g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
        } else {
            super.paintComponent(g);
        }
    }

    private void loadBackgroundImage() {
        try {
            var url = getClass().getResource("/images/ui/background.jpg");
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
            } else {
                System.err.println("Lobby background not found: /images/ui/background.jpg");
            }
        } catch (Exception e) {
            System.err.println("Error loading lobby background: " + e.getMessage());
        }
    }
}
