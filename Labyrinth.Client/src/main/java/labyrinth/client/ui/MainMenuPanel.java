package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import labyrinth.client.ui.Styles.StyledButton;
import labyrinth.client.ui.Styles.StyledContextMenu;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.util.ImageAssetManager;
import lombok.Getter;
import lombok.Setter;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Main menu panel for the Labyrinth game.
 * Displays the game logo and primary navigation buttons.
 */
public class MainMenuPanel extends ThemedPanel {

    private static final Logger log = LoggerFactory.getLogger(MainMenuPanel.class);

    @Setter
    private Runnable onMultiplayerClicked;
    @Setter
    private Runnable onOptionsClicked;
    @Setter
    private Runnable onExitClicked;

    @Getter
    private String multiplayerUsername = "Player";

    private Image logoImage;
    private final List<StyledButton> navigationButtons = new ArrayList<>();

    public MainMenuPanel() {
        setDrawCornerDecorations(true);
        loadResources();
        initMusic();
        setupUI();
        setupKeyboardNavigation();
    }

    /**
     * Shows a dialog to enter the multiplayer username.
     *
     * @param onUsernameEntered callback invoked with the entered username
     */
    public void showMultiplayerUsernameDialog(Consumer<String> onUsernameEntered) {
        DialogFactory.showInputAsync(
                this,
                "Gib deinen Spielernamen ein",
                "Spielername:",
                multiplayerUsername,
                "Dein Anzeigename im Multiplayer-Spiel",
                username -> {
                    multiplayerUsername = username;
                    if (onUsernameEntered != null) {
                        onUsernameEntered.accept(username);
                    }
                }
        );
    }

    @Override
    public void setVisible(boolean aFlag) {
        super.setVisible(aFlag);
        if (aFlag) {
            initMusic();
        }
    }

    private void loadResources() {
        logoImage = ImageAssetManager.getInstance().getLogoImage();
        if (logoImage == null) {
            log.error("Logo not found: /images/ui/logo.png");
        }
    }

    private void initMusic() {
        java.util.prefs.Preferences prefs =
                java.util.prefs.Preferences.userNodeForPackage(OptionsPanel.class);

        float volume = prefs.getInt("musicVolume", 50) / 100f;

        AudioPlayer.getInstance().setMusicVolume(volume);
        AudioPlayer.getInstance().playMenuMusic();
    }

    public void setMusicVolume(int volume) {
        float vol = Math.max(0, Math.min(100, volume)) / 100.0f;
        AudioPlayer.getInstance().setMusicVolume(vol);
    }

    public void stopMusic() {
        AudioPlayer.getInstance().stopMusic();
    }

    private void setupUI() {
        setLayout(new GridBagLayout());

        JPanel contentPanel = new JPanel();
        contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));
        contentPanel.setOpaque(false);
        contentPanel.setBorder(new EmptyBorder(20, 40, 40, 40));

        contentPanel.add(Box.createVerticalGlue());

        JPanel logoPanel = createLogoPanel();
        logoPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(logoPanel);

        contentPanel.add(Box.createVerticalStrut(15));
        contentPanel.add(Box.createVerticalStrut(30));

        JPanel buttonContainer = createButtonPanel();
        buttonContainer.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(buttonContainer);

        contentPanel.add(Box.createVerticalGlue());

        JPanel footerPanel = createFooterPanel();
        footerPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(footerPanel);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        add(contentPanel, gbc);
    }

    private JPanel createLogoPanel() {
        JPanel panel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
                g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);

                int centerX = getWidth() / 2;
                int centerY = getHeight() / 2;

                if (logoImage != null) {
                    int originalWidth = logoImage.getWidth(null);
                    int originalHeight = logoImage.getHeight(null);

                    if (originalWidth > 0 && originalHeight > 0) {
                        int panelWidth = getWidth();
                        int panelHeight = getHeight();

                        int targetWidth = Math.min((int)(panelWidth * 0.8), 600);
                        int targetHeight = Math.min((int)(panelHeight * 0.9), 200);

                        double scaleX = (double) targetWidth / originalWidth;
                        double scaleY = (double) targetHeight / originalHeight;
                        double scale = Math.min(scaleX, scaleY);

                        int scaledWidth = (int) (originalWidth * scale);
                        int scaledHeight = (int) (originalHeight * scale);

                        int x = centerX - scaledWidth / 2;
                        int y = centerY - scaledHeight / 2;

                        g2.drawImage(logoImage, x, y, scaledWidth, scaledHeight, null);
                    }
                } else {
                    drawTextLogo(g2, centerX);
                }

                g2.dispose();
            }

            private void drawTextLogo(Graphics2D g2, int centerX) {
                String title = "Das Verrückte";
                String title2 = "LABYRINTH";

                g2.setFont(FontManager.getHeadingLarge());
                FontMetrics fm = g2.getFontMetrics();
                int titleWidth = fm.stringWidth(title);

                g2.setColor(GameTheme.Colors.shadow());
                g2.drawString(title, centerX - titleWidth / 2 + 3, 43);
                g2.setColor(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
                g2.drawString(title, centerX - titleWidth / 2, 40);

                fm = g2.getFontMetrics();
                int title2Width = fm.stringWidth(title2);

                for (int i = 8; i > 0; i--) {
                    float alpha = 0.04f * i;
                    g2.setColor(new Color(
                            GameTheme.Colors.PRIMARY_GOLD_LIGHT.getRed(),
                            GameTheme.Colors.PRIMARY_GOLD_LIGHT.getGreen(),
                            GameTheme.Colors.PRIMARY_GOLD_LIGHT.getBlue(),
                            (int) (alpha * 255)));
                    g2.drawString(title2, centerX - title2Width / 2 - i / 2, 105);
                    g2.drawString(title2, centerX - title2Width / 2 + i / 2, 105);
                }

                g2.setColor(GameTheme.Colors.shadow());
                g2.drawString(title2, centerX - title2Width / 2 + 4, 109);

                GradientPaint goldGradient = new GradientPaint(
                        centerX - title2Width / 2, 60, GameTheme.Colors.PRIMARY_GOLD_LIGHT,
                        centerX + title2Width / 2, 115, GameTheme.Colors.PRIMARY_GOLD_DARK
                );
                g2.setPaint(goldGradient);
                g2.drawString(title2, centerX - title2Width / 2, 105);
            }
        };
        panel.setOpaque(false);

        panel.setPreferredSize(new Dimension(600, 200));
        panel.setMinimumSize(new Dimension(300, 100));
        return panel;
    }

    private JPanel createButtonPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setOpaque(false);

        navigationButtons.clear();

        StyledButton multiplayerBtn = new StyledButton(
                "Spiel starten",
                "Spiele alleine gegen KI oder online mit Freunden",
                StyledButton.Style.MENU);
        multiplayerBtn.setPreferredSize(new Dimension(320, 70));
        multiplayerBtn.setMinimumSize(new Dimension(200, 50));
        multiplayerBtn.setMaximumSize(new Dimension(450, 90));
        multiplayerBtn.addActionListener(e -> {
            if (onMultiplayerClicked != null) onMultiplayerClicked.run();
        });
        multiplayerBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        StyledTooltipManager.setTooltip(multiplayerBtn, "Spiel starten",
                "Spiele alleine gegen KI oder online mit Freunden");
        StyledContextMenu.attachTo(multiplayerBtn);
        panel.add(multiplayerBtn);
        navigationButtons.add(multiplayerBtn);

        panel.add(Box.createVerticalStrut(12));

        StyledButton optionsBtn = new StyledButton(
                "Einstellungen",
                "Grafik, Audio & mehr",
                StyledButton.Style.MENU);
        optionsBtn.setPreferredSize(new Dimension(320, 70));
        optionsBtn.setMinimumSize(new Dimension(200, 50));
        optionsBtn.setMaximumSize(new Dimension(450, 90));
        optionsBtn.addActionListener(e -> {
            if (onOptionsClicked != null) onOptionsClicked.run();
        });
        optionsBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        StyledTooltipManager.setTooltip(optionsBtn, "Einstellungen",
                "Grafik, Audio und Spieloptionen anpassen");
        StyledContextMenu.attachTo(optionsBtn);
        panel.add(optionsBtn);
        navigationButtons.add(optionsBtn);

        panel.add(Box.createVerticalStrut(20));

        StyledButton exitBtn = new StyledButton("Beenden", StyledButton.Style.DANGER);
        exitBtn.setPreferredSize(new Dimension(130, 38));
        exitBtn.setMaximumSize(new Dimension(150, 45));
        exitBtn.setMinimumSize(new Dimension(100, 32));
        exitBtn.addActionListener(e -> {
            if (onExitClicked != null) {
                onExitClicked.run();
            } else {
                System.exit(0);
            }
        });
        exitBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        StyledTooltipManager.setTooltip(exitBtn, "Beenden",
                "Spiel verlassen und Anwendung schließen");
        StyledContextMenu.attachTo(exitBtn);
        panel.add(exitBtn);
        navigationButtons.add(exitBtn);

        return panel;
    }

    private void setupKeyboardNavigation() {
        KeyboardNavigationHelper.setupVerticalNavigation(this, navigationButtons);
        setFocusCycleRoot(true);
        setFocusTraversalPolicy(KeyboardNavigationHelper.createFocusPolicy(navigationButtons));

        // Request focus on first button when panel gains focus
        addFocusListener(new java.awt.event.FocusAdapter() {
            @Override
            public void focusGained(java.awt.event.FocusEvent e) {
                if (!navigationButtons.isEmpty()) {
                    navigationButtons.get(0).requestFocusInWindow();
                }
            }
        });
    }

    @Override
    public void addNotify() {
        super.addNotify();
        // Focus the first button when panel becomes visible
        SwingUtilities.invokeLater(() -> {
            if (!navigationButtons.isEmpty()) {
                navigationButtons.get(0).requestFocusInWindow();
            }
        });
    }

    private JPanel createFooterPanel() {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel.setOpaque(false);

        JLabel versionLabel = new JLabel("Version 1.0  |  © 2025 Gruppe 1");
        versionLabel.setFont(FontManager.getBodySmall());
        versionLabel.setForeground(ThemeEffects.withAlpha(GameTheme.Colors.TEXT_MUTED, 150));
        panel.add(versionLabel);

        return panel;
    }

    public void setMultiplayerUsername(String username) {
        if (username != null && !username.isBlank()) {
            this.multiplayerUsername = username;
        }
    }
}
