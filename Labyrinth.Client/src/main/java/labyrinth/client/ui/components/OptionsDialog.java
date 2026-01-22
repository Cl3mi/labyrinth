package labyrinth.client.ui.components;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.audio.SoundEffects;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.client.ui.Styles.StyledDialog;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class OptionsDialog extends JDialog {

    private final SoundEffects soundEffects;
    private final AudioPlayer backgroundMusic;
    private final ToastManagerWrapper toastManager; // Wrapper or callback
    private final Runnable onExitGame;

    // Interface for minimal toast needing
    public interface ToastManagerWrapper {
        void showSuccess(String id, String title, String msg);
    }

    public OptionsDialog(Window owner, SoundEffects soundEffects, AudioPlayer backgroundMusic,
            ToastManagerWrapper toastManager, Runnable onExitGame) {
        super(owner, "Optionen", ModalityType.APPLICATION_MODAL);
        this.soundEffects = soundEffects;
        this.backgroundMusic = backgroundMusic;
        this.toastManager = toastManager;
        this.onExitGame = onExitGame;

        initUI();
    }

    private void initUI() {
        setUndecorated(true);
        setBackground(new Color(0, 0, 0, 0));

        // Farben
        Color CARD_BG = new Color(35, 32, 28, 240);
        Color CARD_BORDER = new Color(100, 85, 60);
        Color PRIMARY_GOLD = new Color(218, 165, 32);
        Color PRIMARY_GOLD_LIGHT = new Color(255, 215, 0);
        Color TEXT_LIGHT = new Color(255, 248, 230);
        Color STONE_DARK = new Color(45, 42, 38);

        JPanel mainPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setColor(CARD_BG);
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 20, 20);
                g2.setColor(CARD_BORDER);
                g2.setStroke(new BasicStroke(3));
                g2.drawRoundRect(1, 1, getWidth() - 3, getHeight() - 3, 20, 20);
                g2.setColor(new Color(255, 255, 255, 15));
                g2.fillRoundRect(2, 2, getWidth() - 4, 50, 18, 18);
                g2.dispose();
                super.paintComponent(g);
            }
        };
        mainPanel.setOpaque(false);
        mainPanel.setLayout(new BorderLayout(0, 15));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(25, 30, 25, 30));

        JLabel titleLabel = new JLabel("⚙ Optionen");
        titleLabel.setFont(new Font("Serif", Font.BOLD, 24));
        titleLabel.setForeground(PRIMARY_GOLD_LIGHT);
        titleLabel.setHorizontalAlignment(SwingConstants.CENTER);
        mainPanel.add(titleLabel, BorderLayout.NORTH);

        JPanel contentPanel = new JPanel();
        contentPanel.setOpaque(false);
        contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));

        java.util.prefs.Preferences prefs = java.util.prefs.Preferences.userNodeForPackage(getClass());
        int currentMusicVolume = prefs.getInt("musicVolume", 50);
        int currentSfxVolume = prefs.getInt("sfxVolume", 70);
        boolean darkTheme = ThemeManager.getInstance().isDarkMode();

        // Audio
        JPanel audioSection = createOptionsSection("🔊 Audio", CARD_BG, CARD_BORDER, PRIMARY_GOLD_LIGHT);
        JPanel audioContent = new JPanel(new GridBagLayout());
        audioContent.setOpaque(false);
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 10, 8, 10);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.3;
        JLabel musicLabel = new JLabel("Musik:");
        musicLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        musicLabel.setForeground(TEXT_LIGHT);
        audioContent.add(musicLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 0.5;
        JSlider musicSlider = new JSlider(0, 100, currentMusicVolume);
        musicSlider.setOpaque(false);
        musicSlider.setPreferredSize(new Dimension(180, 30));
        audioContent.add(musicSlider, gbc);

        gbc.gridx = 2;
        gbc.weightx = 0.2;
        JLabel musicValueLabel = new JLabel(currentMusicVolume + "%");
        musicValueLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        musicValueLabel.setForeground(PRIMARY_GOLD);
        audioContent.add(musicValueLabel, gbc);

        musicSlider.addChangeListener(e -> {
            int vol = musicSlider.getValue();
            musicValueLabel.setText(vol + "%");
            backgroundMusic.setMusicVolume(vol / 100.0f);
        });

        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel sfxLabel = new JLabel("Effekte:");
        sfxLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        sfxLabel.setForeground(TEXT_LIGHT);
        audioContent.add(sfxLabel, gbc);

        gbc.gridx = 1;
        JSlider sfxSlider = new JSlider(0, 100, currentSfxVolume);
        sfxSlider.setOpaque(false);
        sfxSlider.setPreferredSize(new Dimension(180, 30));
        audioContent.add(sfxSlider, gbc);

        gbc.gridx = 2;
        JLabel sfxValueLabel = new JLabel(currentSfxVolume + "%");
        sfxValueLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        sfxValueLabel.setForeground(PRIMARY_GOLD);
        audioContent.add(sfxValueLabel, gbc);

        sfxSlider.addChangeListener(e -> {
            int vol = sfxSlider.getValue();
            sfxValueLabel.setText(vol + "%");
            soundEffects.setVolume(vol / 100.0f);
        });

        audioSection.add(audioContent, BorderLayout.CENTER);
        contentPanel.add(audioSection);
        contentPanel.add(Box.createVerticalStrut(15));

        // Theme
        JPanel themeSection = createOptionsSection("🎨 Darstellung", CARD_BG, CARD_BORDER, PRIMARY_GOLD_LIGHT);
        JPanel themeContent = new JPanel(new FlowLayout(FlowLayout.LEFT, 15, 10));
        themeContent.setOpaque(false);

        JLabel themeLabel = new JLabel("Theme:");
        themeLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        themeLabel.setForeground(TEXT_LIGHT);
        themeContent.add(themeLabel);

        JToggleButton themeToggle = new JToggleButton() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                int w = getWidth(), h = getHeight();
                g2.setColor(isSelected() ? new Color(60, 60, 80) : new Color(200, 180, 140));
                g2.fillRoundRect(0, 0, w, h, h, h);
                g2.setColor(CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, w - 2, h - 2, h - 2, h - 2);
                int knobSize = h - 6;
                int knobX = isSelected() ? w - knobSize - 3 : 3;
                g2.setColor(isSelected() ? new Color(100, 100, 140) : new Color(255, 220, 120));
                g2.fillOval(knobX, 3, knobSize, knobSize);
                g2.setFont(new Font("SansSerif", Font.PLAIN, 12));
                g2.setColor(TEXT_LIGHT);
                g2.drawString(isSelected() ? "🌙" : "☀", knobX + 4, h - 7);
                g2.dispose();
            }
        };
        themeToggle.setSelected(darkTheme);
        themeToggle.setPreferredSize(new Dimension(60, 28));
        themeToggle.setOpaque(false);
        themeToggle.setBorderPainted(false);
        themeToggle.setFocusPainted(false);
        themeToggle.setCursor(new Cursor(Cursor.HAND_CURSOR));
        themeContent.add(themeToggle);

        JLabel themeStatusLabel = new JLabel(darkTheme ? "Dunkel" : "Hell");
        themeStatusLabel.setFont(new Font("SansSerif", Font.PLAIN, 14));
        themeStatusLabel.setForeground(TEXT_LIGHT);
        themeContent.add(themeStatusLabel);

        themeToggle.addActionListener(e -> {
            themeStatusLabel.setText(themeToggle.isSelected() ? "Dunkel" : "Hell");
            ThemeManager.getInstance().setDarkMode(themeToggle.isSelected());
        });

        themeSection.add(themeContent, BorderLayout.CENTER);
        contentPanel.add(themeSection);

        mainPanel.add(contentPanel, BorderLayout.CENTER);

        // Buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 15, 0));
        buttonPanel.setOpaque(false);

        JButton exitButton = createStyledDialogButton("🚪 Spiel beenden", new Color(120, 50, 50),
                new Color(180, 80, 80));
        exitButton.addActionListener(e -> {
            boolean confirmed = StyledDialog.showConfirm(this, "Spiel beenden?", "Fortschritt geht verloren.");
            if (confirmed) {
                dispose();
                if (onExitGame != null)
                    onExitGame.run();
            }
        });
        buttonPanel.add(exitButton);

        JButton saveButton = createStyledDialogButton("💾 Speichern", new Color(60, 100, 60), new Color(100, 160, 100));
        saveButton.addActionListener(e -> {
            prefs.putInt("musicVolume", musicSlider.getValue());
            prefs.putInt("sfxVolume", sfxSlider.getValue());
            prefs.putBoolean("darkTheme", themeToggle.isSelected());
            dispose();
            if (toastManager != null)
                toastManager.showSuccess("SETTINGS", "Gespeichert", "Einstellungen gespeichert");
        });
        buttonPanel.add(saveButton);

        JButton closeButton = createStyledDialogButton("✕ Schließen", STONE_DARK, new Color(90, 80, 70));
        closeButton.addActionListener(e -> dispose());
        buttonPanel.add(closeButton);

        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        setContentPane(mainPanel);
        setSize(580, 400);
        setLocationRelativeTo(getOwner());
    }

    private JPanel createOptionsSection(String title, Color bgColor, Color borderColor, Color titleColor) {
        JPanel section = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setColor(new Color(bgColor.getRed(), bgColor.getGreen(), bgColor.getBlue(), 150));
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 12, 12);
                g2.setColor(borderColor);
                g2.setStroke(new BasicStroke(1.5f));
                g2.drawRoundRect(0, 0, getWidth() - 1, getHeight() - 1, 12, 12);
                g2.dispose();
                super.paintComponent(g);
            }
        };
        section.setOpaque(false);
        section.setLayout(new BorderLayout(0, 8));
        section.setBorder(BorderFactory.createEmptyBorder(12, 15, 12, 15));
        JLabel titleLabel = new JLabel(title);
        titleLabel.setFont(new Font("Serif", Font.BOLD, 16));
        titleLabel.setForeground(titleColor);
        section.add(titleLabel, BorderLayout.NORTH);
        return section;
    }

    private JButton createStyledDialogButton(String text, Color bgColor, Color borderColor) {
        JButton btn = new JButton(text) {
            private boolean isHovered = false;
            {
                addMouseListener(new MouseAdapter() {
                    public void mouseEntered(MouseEvent e) {
                        isHovered = true;
                        repaint();
                    }

                    public void mouseExited(MouseEvent e) {
                        isHovered = false;
                        repaint();
                    }
                });
            }

            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setColor(isHovered ? borderColor : bgColor);
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 8, 8);
                g2.setColor(borderColor);
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, getWidth() - 2, getHeight() - 2, 8, 8);
                g2.setFont(new Font("SansSerif", Font.BOLD, 12));
                g2.setColor(new Color(255, 248, 230));
                FontMetrics fm = g2.getFontMetrics();
                g2.drawString(getText(), (getWidth() - fm.stringWidth(getText())) / 2,
                        (getHeight() + fm.getAscent() - fm.getDescent()) / 2);
                g2.dispose();
            }
        };
        btn.setPreferredSize(new Dimension(170, 38));
        btn.setOpaque(false);
        btn.setContentAreaFilled(false);
        btn.setBorderPainted(false);
        btn.setFocusPainted(false);
        btn.setCursor(new Cursor(Cursor.HAND_CURSOR));
        return btn;
    }
}
