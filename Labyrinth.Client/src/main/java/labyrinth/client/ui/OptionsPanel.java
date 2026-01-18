package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.ui.Styles.StyledDialog;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

/**
 * Options-Panel für das Labyrinth-Spiel.
 *
 * Einstellungen:
 * - Musik-Lautstärke
 * - Sound-Effekte-Lautstärke
 * - Server-Verbindungs-URL
 * - Theme (Dark/Light)
 */
public class OptionsPanel extends JPanel {

    // Callbacks
    private Runnable onBackToMenu;
    private Runnable onSettingsChanged;
    private Consumer<Integer> onMusicVolumeChanged;
    private Consumer<Integer> onSfxVolumeChanged;
    private Consumer<int[]> onWindowSizeChanged;

    // Hintergrund
    private Image backgroundImage;

    // Settings Components
    private JSlider musicVolumeSlider;
    private JSlider sfxVolumeSlider;
    private JTextField serverUrlField;
    private JToggleButton themeToggle;
    private JLabel musicValueLabel;
    private JLabel sfxValueLabel;

    // Current Settings
    private int musicVolume = 10;
    private int sfxVolume = 70;
    private String serverUrl = DEFAULT_SERVER_URL;
    private boolean darkTheme = true;
    private int windowSizeIndex = 1; // 0=1280x720, 1=1400x900, 2=1600x900, 3=1920x1080, 4=Maximiert

    // Window size options
    private static final String[] WINDOW_SIZE_OPTIONS = {
            "1280 x 720",
            "1400 x 900",
            "1600 x 900",
            "1920 x 1080",
            "Maximiert"
    };
    private static final int[][] WINDOW_SIZES = {
            {1280, 720},
            {1400, 900},
            {1600, 900},
            {1920, 1080},
            {-1, -1} // -1 = maximiert
    };

    // Preferences für persistente Speicherung
    private static final Preferences PREFS = Preferences.userNodeForPackage(OptionsPanel.class);
    private static final String PREF_MUSIC_VOLUME = "musicVolume";
    private static final String PREF_SFX_VOLUME = "sfxVolume";
    private static final String PREF_SERVER_URL = "serverUrl";
    private static final String PREF_DARK_THEME = "darkTheme";
    private static final String PREF_WINDOW_SIZE = "windowSize";

    // Default Server URL - zentrale Konfiguration
    public static final String DEFAULT_SERVER_URL = "https://mgmt.dvl.spalx.dev";

    public OptionsPanel() {

        onMusicVolumeChanged = v -> AudioPlayer.getInstance().setMusicVolume(v / 100f);
        onSfxVolumeChanged   = v -> AudioPlayer.getInstance().setSfxVolume(v / 100f);

        FontManager.initFonts();
        loadSettings();
        loadBackgroundImage();
        setupUI();

        // Theme-Änderungen überwachen
        ThemeManager.getInstance().addThemeChangeListener(() -> {
            loadBackgroundImage();
            repaint();
        });
    }

    private boolean isFontAvailable(String fontName) {
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        for (String family : ge.getAvailableFontFamilyNames()) {
            if (family.equalsIgnoreCase(fontName)) return true;
        }
        return false;
    }

    private void loadBackgroundImage() {
        try {
            String imagePath = ThemeManager.getInstance().getBackgroundImagePath();
            var url = getClass().getResource(imagePath);
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
                System.out.println("[OptionsPanel] Loaded background: " + imagePath);
            }
        } catch (Exception e) {
            System.err.println("Error loading background: " + e.getMessage());
        }
    }

    private void loadSettings() {
        musicVolume = PREFS.getInt(PREF_MUSIC_VOLUME, 50);
        sfxVolume = PREFS.getInt(PREF_SFX_VOLUME, 70);
        serverUrl = PREFS.get(PREF_SERVER_URL, "ws://localhost:8082/game");
        darkTheme = ThemeManager.getInstance().isDarkMode(); // Use ThemeManager as source of truth
        windowSizeIndex = PREFS.getInt(PREF_WINDOW_SIZE, 1); // Default: 1400x900
    }

    private void saveSettings() {
        PREFS.putInt(PREF_MUSIC_VOLUME, musicVolume);
        PREFS.putInt(PREF_SFX_VOLUME, sfxVolume);
        PREFS.put(PREF_SERVER_URL, serverUrl);
        PREFS.putBoolean(PREF_DARK_THEME, darkTheme);
        PREFS.putInt(PREF_WINDOW_SIZE, windowSizeIndex);

        // Fenstergröße sofort anwenden
        if (onWindowSizeChanged != null) {
            onWindowSizeChanged.accept(getWindowSize());
        }

        if (onSettingsChanged != null) {
            onSettingsChanged.run();
        }
    }

    private void setupUI() {
        setOpaque(false);
        setLayout(new BorderLayout(0, 20));
        setBorder(new EmptyBorder(30, 50, 30, 50));

        // Header
        add(createHeader(), BorderLayout.NORTH);

        // Center - Settings Cards
        add(createCenterPanel(), BorderLayout.CENTER);

        // Footer - Buttons
        add(createFooter(), BorderLayout.SOUTH);
    }

    private JPanel createHeader() {
        JPanel header = new JPanel(new BorderLayout());
        header.setOpaque(false);

        // Zurück-Button
        StyledButton backButton = new StyledButton("Zurück", StyledButton.Style.SECONDARY);
        backButton.setPreferredSize(new Dimension(140, 40));
        backButton.addActionListener(e -> {
            if (onBackToMenu != null) onBackToMenu.run();
        });

        JPanel leftPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        leftPanel.setOpaque(false);
        leftPanel.add(backButton);
        header.add(leftPanel, BorderLayout.WEST);

        // Titel
        JLabel titleLabel = new JLabel("Einstellungen");
        titleLabel.setFont(FontManager.titleFont);
        titleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        titleLabel.setHorizontalAlignment(SwingConstants.CENTER);
        header.add(titleLabel, BorderLayout.CENTER);

        // Platzhalter rechts
        JPanel rightPanel = new JPanel();
        rightPanel.setOpaque(false);
        rightPanel.setPreferredSize(new Dimension(140, 40));
        header.add(rightPanel, BorderLayout.EAST);

        return header;
    }

    private JPanel createCenterPanel() {
        JPanel center = new JPanel();
        center.setLayout(new BoxLayout(center, BoxLayout.Y_AXIS));
        center.setOpaque(false);

        // Audio Settings Card
        JPanel audioCard = createSettingsCard("Audio", createAudioSettings());
        audioCard.setAlignmentX(Component.CENTER_ALIGNMENT);
        center.add(audioCard);

        center.add(Box.createVerticalStrut(20));

        // Connection Settings Card
        JPanel connectionCard = createSettingsCard("Verbindung", createConnectionSettings());
        connectionCard.setAlignmentX(Component.CENTER_ALIGNMENT);
        center.add(connectionCard);

        center.add(Box.createVerticalStrut(20));

        // Appearance Settings Card
        JPanel appearanceCard = createSettingsCard("Darstellung", createAppearanceSettings());
        appearanceCard.setAlignmentX(Component.CENTER_ALIGNMENT);
        center.add(appearanceCard);

        // Wrapper für Zentrierung
        JPanel wrapper = new JPanel(new GridBagLayout());
        wrapper.setOpaque(false);
        wrapper.add(center);

        return wrapper;
    }

    private JPanel createSettingsCard(String title, JPanel content) {
        JPanel card = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Card Background
                g2.setColor(GameTheme.Colors.CARD_BG);
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 15, 15));

                // Border
                g2.setColor(GameTheme.Colors.CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 2, getHeight() - 2, 15, 15));

                // Top highlight
                g2.setColor(new Color(255, 255, 255, 10));
                g2.fill(new RoundRectangle2D.Float(2, 2, getWidth() - 4, 35, 13, 13));

                g2.dispose();
                super.paintComponent(g);
            }
        };
        card.setOpaque(false);
        card.setLayout(new BorderLayout(0, 15));
        card.setBorder(new EmptyBorder(20, 30, 25, 30));

        // Größere Karten für bessere Lesbarkeit
        int cardHeight = content.getPreferredSize().height + 90;
        card.setPreferredSize(new Dimension(550, cardHeight));
        card.setMaximumSize(new Dimension(550, cardHeight));

        // Titel
        JLabel titleLabel = new JLabel(title);
        titleLabel.setFont(FontManager.titleFont);
        titleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        card.add(titleLabel, BorderLayout.NORTH);

        card.add(content, BorderLayout.CENTER);

        return card;
    }

    private JPanel createAudioSettings() {
        JPanel panel = new JPanel(new GridBagLayout());
        panel.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 5, 8, 5);

        // Musik-Lautstärke
        gbc.gridx = 0; gbc.gridy = 0; gbc.weightx = 0.3;
        panel.add(createStyledLabel("Musik:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.5;
        musicVolumeSlider = createStyledSlider(0, 100, musicVolume);

        // Label VOR Listener erzeugen (sonst NPE / UI aktualisiert nicht korrekt)
        gbc.gridx = 2; gbc.weightx = 0.2;
        musicValueLabel = createStyledLabel(musicVolume + "%");
        musicValueLabel.setHorizontalAlignment(SwingConstants.RIGHT);

        // Slider ins Layout
        gbc.gridx = 1; gbc.weightx = 0.5;
        panel.add(musicVolumeSlider, gbc);

        // Label ins Layout
        gbc.gridx = 2; gbc.weightx = 0.2;
        panel.add(musicValueLabel, gbc);

        // Listener erst nachdem Label existiert
        musicVolumeSlider.addChangeListener(e -> {
            musicVolume = musicVolumeSlider.getValue();
            musicValueLabel.setText(musicVolume + "%");
            // Echtzeit-Update der Musik-Lautstärke
            if (onMusicVolumeChanged != null) {
                onMusicVolumeChanged.accept(musicVolume);
            }
        });

        // Sound-Effekte-Lautstärke
        gbc.gridx = 0; gbc.gridy = 1; gbc.weightx = 0.3;
        panel.add(createStyledLabel("Effekte:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.5;
        sfxVolumeSlider = createStyledSlider(0, 100, sfxVolume);

        // Label VOR Listener erzeugen
        gbc.gridx = 2; gbc.weightx = 0.2;
        sfxValueLabel = createStyledLabel(sfxVolume + "%");
        sfxValueLabel.setHorizontalAlignment(SwingConstants.RIGHT);

        // Slider ins Layout
        gbc.gridx = 1; gbc.weightx = 0.5;
        panel.add(sfxVolumeSlider, gbc);

        // Label ins Layout
        gbc.gridx = 2; gbc.weightx = 0.2;
        panel.add(sfxValueLabel, gbc);

        // Listener erst nachdem Label existiert
        sfxVolumeSlider.addChangeListener(e -> {
            sfxVolume = sfxVolumeSlider.getValue();
            sfxValueLabel.setText(sfxVolume + "%");
            // Echtzeit-Update der Effekt-Lautstärke
            if (onSfxVolumeChanged != null) {
                onSfxVolumeChanged.accept(sfxVolume);
            }
        });

        panel.setPreferredSize(new Dimension(480, 100));
        return panel;
    }


    private JPanel createConnectionSettings() {
        JPanel panel = new JPanel(new GridBagLayout());
        panel.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 5, 8, 5);

        // Server URL
        gbc.gridx = 0; gbc.gridy = 0; gbc.weightx = 0.25;
        panel.add(createStyledLabel("Server:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.75; gbc.gridwidth = 2;
        serverUrlField = createStyledTextField(serverUrl);
        serverUrlField.addActionListener(e -> serverUrl = serverUrlField.getText().trim());
        serverUrlField.addFocusListener(new java.awt.event.FocusAdapter() {
            @Override
            public void focusLost(java.awt.event.FocusEvent e) {
                serverUrl = serverUrlField.getText().trim();
            }
        });
        panel.add(serverUrlField, gbc);

        // Hinweis
        gbc.gridx = 0; gbc.gridy = 1; gbc.gridwidth = 3;
        gbc.insets = new Insets(5, 5, 0, 5);
        JLabel hintLabel = new JLabel("Format: ws://hostname:port/path");
        hintLabel.setFont(new Font("SansSerif", Font.ITALIC, 11));
        hintLabel.setForeground(GameTheme.Colors.TEXT_MUTED);
        panel.add(hintLabel, gbc);

        panel.setPreferredSize(new Dimension(480, 90));
        return panel;
    }

    private JPanel createAppearanceSettings() {
        JPanel panel = new JPanel(new GridBagLayout());
        panel.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 5, 8, 5);

        // Theme Toggle
        gbc.gridx = 0; gbc.gridy = 0; gbc.weightx = 0.3;
        panel.add(createStyledLabel("Theme:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.7;
        JPanel themePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 0));
        themePanel.setOpaque(false);

        themeToggle = createThemeToggle();
        themePanel.add(themeToggle);

        JLabel themeStatusLabel = new JLabel(darkTheme ? "Dunkel" : "Hell");
        themeStatusLabel.setFont(FontManager.labelFont);
        themeStatusLabel.setForeground(GameTheme.Colors.TEXT_LIGHT);
        themePanel.add(themeStatusLabel);

        themeToggle.addActionListener(e -> {
            darkTheme = themeToggle.isSelected();
            themeStatusLabel.setText(darkTheme ? "Dunkel" : "Hell");
            // Update ThemeManager and notify all listeners
            ThemeManager.getInstance().setDarkMode(darkTheme);
        });

        panel.add(themePanel, gbc);

        // Window Size
        gbc.gridx = 0; gbc.gridy = 1; gbc.weightx = 0.3; gbc.gridwidth = 1;
        gbc.insets = new Insets(8, 5, 8, 5);
        panel.add(createStyledLabel("Fenstergröße:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.7;
        JComboBox<String> windowSizeCombo = createStyledComboBox();
        for (String option : WINDOW_SIZE_OPTIONS) {
            windowSizeCombo.addItem(option);
        }
        windowSizeCombo.setSelectedIndex(windowSizeIndex);
        windowSizeCombo.addActionListener(e -> {
            windowSizeIndex = windowSizeCombo.getSelectedIndex();
        });
        panel.add(windowSizeCombo, gbc);

        // Hinweis
        gbc.gridx = 0; gbc.gridy = 2; gbc.gridwidth = 2;
        gbc.insets = new Insets(5, 5, 0, 5);
        JLabel hintLabel = new JLabel("Änderungen werden nach Neustart wirksam");
        hintLabel.setFont(new Font("SansSerif", Font.ITALIC, 11));
        hintLabel.setForeground(GameTheme.Colors.TEXT_MUTED);
        panel.add(hintLabel, gbc);

        panel.setPreferredSize(new Dimension(480, 130));
        return panel;
    }

    private JPanel createFooter() {
        JPanel footer = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 10));
        footer.setOpaque(false);

        StyledButton resetButton = new StyledButton("Zurücksetzen", StyledButton.Style.DANGER);
        resetButton.setPreferredSize(new Dimension(150, 45));
        resetButton.addActionListener(e -> resetToDefaults());
        footer.add(resetButton);

        StyledButton saveButton = new StyledButton("Speichern", StyledButton.Style.PRIMARY);
        saveButton.setPreferredSize(new Dimension(150, 45));
        saveButton.addActionListener(e -> {
            saveSettings();
            showSaveConfirmation();
        });
        footer.add(saveButton);

        return footer;
    }

    // --------------------------------------------------------------------------------
    // Styled Components
    // --------------------------------------------------------------------------------

    private JLabel createStyledLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(FontManager.labelFont);
        label.setForeground(GameTheme.Colors.TEXT_LIGHT);
        return label;
    }

    private JSlider createStyledSlider(int min, int max, int value) {
        JSlider slider = new JSlider(min, max, value) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                int trackY = getHeight() / 2;
                int trackHeight = 6;

                // Track background
                g2.setColor(GameTheme.Colors.STONE_DARK);
                g2.fillRoundRect(8, trackY - trackHeight/2, getWidth() - 16, trackHeight, 3, 3);

                // Filled portion
                int fillWidth = (int) ((getValue() - getMinimum()) / (double) (getMaximum() - getMinimum()) * (getWidth() - 16));
                g2.setColor(GameTheme.Colors.PRIMARY_GOLD);
                g2.fillRoundRect(8, trackY - trackHeight/2, fillWidth, trackHeight, 3, 3);

                // Thumb
                int thumbX = 8 + fillWidth - 8;
                g2.setColor(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
                g2.fillOval(thumbX, trackY - 8, 16, 16);
                g2.setColor(GameTheme.Colors.PRIMARY_GOLD_DARK);
                g2.setStroke(new BasicStroke(2));
                g2.drawOval(thumbX, trackY - 8, 16, 16);

                g2.dispose();
            }
        };
        slider.setOpaque(false);
        slider.setPreferredSize(new Dimension(200, 30));
        return slider;
    }

    private JTextField createStyledTextField(String text) {
        JTextField field = new JTextField(text);
        field.setFont(new Font("Monospaced", Font.PLAIN, 13));
        field.setBackground(GameTheme.Colors.STONE_DARK);
        field.setForeground(GameTheme.Colors.TEXT_LIGHT);
        field.setCaretColor(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        field.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(GameTheme.Colors.CARD_BORDER, 1),
                BorderFactory.createEmptyBorder(8, 10, 8, 10)
        ));
        field.setPreferredSize(new Dimension(300, 35));
        return field;
    }

    private JComboBox<String> createStyledComboBox() {
        JComboBox<String> combo = new JComboBox<>();
        combo.setFont(new Font("SansSerif", Font.PLAIN, 13));
        combo.setBackground(GameTheme.Colors.STONE_DARK);
        combo.setForeground(GameTheme.Colors.TEXT_LIGHT);
        combo.setPreferredSize(new Dimension(150, 30));

        // Custom renderer for dropdown items
        combo.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value,
                                                          int index, boolean isSelected, boolean cellHasFocus) {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                setBackground(isSelected ? GameTheme.Colors.PRIMARY_GOLD_DARK : GameTheme.Colors.STONE_DARK);
                setForeground(GameTheme.Colors.TEXT_LIGHT);
                setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
                return this;
            }
        });

        return combo;
    }

    private JToggleButton createThemeToggle() {
        JToggleButton toggle = new JToggleButton() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                int w = getWidth();
                int h = getHeight();

                // Background
                if (isSelected()) {
                    g2.setColor(new Color(60, 60, 80));
                } else {
                    g2.setColor(new Color(200, 180, 140));
                }
                g2.fillRoundRect(0, 0, w, h, h, h);

                // Border
                g2.setColor(GameTheme.Colors.CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, w - 2, h - 2, h - 2, h - 2);

                // Knob
                int knobSize = h - 6;
                int knobX = isSelected() ? w - knobSize - 3 : 3;
                g2.setColor(isSelected() ? new Color(100, 100, 140) : new Color(255, 220, 120));
                g2.fillOval(knobX, 3, knobSize, knobSize);

                // Icon
                g2.setFont(new Font("SansSerif", Font.PLAIN, 12));
                g2.setColor(GameTheme.Colors.TEXT_LIGHT);
                if (isSelected()) {
                    g2.drawString("🌙", knobX + 4, h - 7);
                } else {
                    g2.drawString("☀", knobX + 4, h - 7);
                }

                g2.dispose();
            }
        };
        toggle.setSelected(darkTheme);
        toggle.setPreferredSize(new Dimension(60, 28));
        toggle.setOpaque(false);
        toggle.setBorderPainted(false);
        toggle.setFocusPainted(false);
        toggle.setCursor(new Cursor(Cursor.HAND_CURSOR));
        return toggle;
    }

    private void resetToDefaults() {
        boolean confirmed = StyledDialog.showConfirm(this,
                "Zurücksetzen bestätigen",
                "Alle Einstellungen auf Standardwerte zurücksetzen?");

        if (confirmed) {
            musicVolume = 50;
            sfxVolume = 70;
            serverUrl = DEFAULT_SERVER_URL;
            darkTheme = true;

            musicVolumeSlider.setValue(musicVolume);
            sfxVolumeSlider.setValue(sfxVolume);
            serverUrlField.setText(serverUrl);
            themeToggle.setSelected(darkTheme);

            musicValueLabel.setText(musicVolume + "%");
            sfxValueLabel.setText(sfxVolume + "%");
        }
    }

    private void showSaveConfirmation() {
        // Kurze Toast-Nachricht
        JDialog toast = new JDialog((Frame) SwingUtilities.getWindowAncestor(this));
        toast.setUndecorated(true);
        toast.setBackground(new Color(0, 0, 0, 0));

        JPanel toastPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setColor(new Color(60, 120, 60, 230));
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 10, 10));
                g2.dispose();
                super.paintComponent(g);
            }
        };
        toastPanel.setOpaque(false);
        toastPanel.setBorder(new EmptyBorder(12, 20, 12, 20));

        JLabel toastLabel = new JLabel("[OK] Einstellungen gespeichert");
        toastLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        toastLabel.setForeground(Color.WHITE);
        toastPanel.add(toastLabel);

        toast.setContentPane(toastPanel);
        toast.pack();
        toast.setLocationRelativeTo(this);

        toast.setVisible(true);

        // Nach 2 Sekunden ausblenden
        Timer timer = new Timer(2000, e -> toast.dispose());
        timer.setRepeats(false);
        timer.start();
    }

    // --------------------------------------------------------------------------------
    // Public API
    // --------------------------------------------------------------------------------

    public void setOnBackToMenu(Runnable callback) {
        this.onBackToMenu = callback;
    }

    public void setOnSettingsChanged(Runnable callback) {
        this.onSettingsChanged = callback;
    }

    public void setOnMusicVolumeChanged(java.util.function.Consumer<Integer> callback) {
        this.onMusicVolumeChanged = callback;
    }

    public void setOnSfxVolumeChanged(java.util.function.Consumer<Integer> callback) {
        this.onSfxVolumeChanged = callback;
    }

    public void setOnWindowSizeChanged(java.util.function.Consumer<int[]> callback) {
        this.onWindowSizeChanged = callback;
    }

    public int getMusicVolume() {
        return musicVolume;
    }

    public int getSfxVolume() {
        return sfxVolume;
    }

    public String getServerUrl() {
        return serverUrl;
    }

    public boolean isDarkTheme() {
        return darkTheme;
    }

    public int getWindowSizeIndex() {
        return windowSizeIndex;
    }

    public int[] getWindowSize() {
        if (windowSizeIndex >= 0 && windowSizeIndex < WINDOW_SIZES.length) {
            return WINDOW_SIZES[windowSizeIndex];
        }
        return WINDOW_SIZES[1]; // Default 1400x900
    }

    /**
     * Statische Methode zum Laden der Fenstergröße beim App-Start
     */
    public static int[] loadWindowSizeFromPreferences() {
        Preferences prefs = Preferences.userNodeForPackage(OptionsPanel.class);
        int index = prefs.getInt(PREF_WINDOW_SIZE, 1);
        if (index >= 0 && index < WINDOW_SIZES.length) {
            return WINDOW_SIZES[index];
        }
        return WINDOW_SIZES[1]; // Default 1400x900
    }

    /**
     * Statische Methode zum Laden der Server-URL beim App-Start.
     * Gibt die gespeicherte URL oder die Default-URL zurück.
     */
    public static String loadServerUrlFromPreferences() {
        Preferences prefs = Preferences.userNodeForPackage(OptionsPanel.class);
        return prefs.get(PREF_SERVER_URL, DEFAULT_SERVER_URL);
    }

    // --------------------------------------------------------------------------------
    // Hintergrund zeichnen
    // --------------------------------------------------------------------------------

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);

        int w = getWidth();
        int h = getHeight();

        if (backgroundImage != null) {
            g2.drawImage(backgroundImage, 0, 0, w, h, this);
        } else {
            // Use dynamic colors based on current theme
            GradientPaint gradient = new GradientPaint(0, 0, GameTheme.Colors.stoneDark(), 0, h, GameTheme.Colors.backgroundSecondary());
            g2.setPaint(gradient);
            g2.fillRect(0, 0, w, h);
        }

        // Overlay
        g2.setColor(ThemeManager.getInstance().isDarkMode()
            ? new Color(0, 0, 0, 80)
            : new Color(0, 0, 0, 30));
        g2.fillRect(0, 0, w, h);

        // Vignette
        drawVignette(g2, w, h);

        // Decorative corners
        drawDecorativeCorners(g2, w, h);

        g2.dispose();
        super.paintComponent(g);
    }

    private void drawVignette(Graphics2D g2, int w, int h) {
        float radius = Math.max(w, h) * 0.8f;
        RadialGradientPaint vignette = new RadialGradientPaint(
                w / 2f, h / 2f, radius,
                new float[]{0.3f, 0.7f, 1.0f},
                new Color[]{new Color(0, 0, 0, 0), new Color(0, 0, 0, 50), new Color(0, 0, 0, 130)}
        );
        g2.setPaint(vignette);
        g2.fillRect(0, 0, w, h);
    }

    private void drawDecorativeCorners(Graphics2D g2, int w, int h) {
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(new Color(218, 165, 32, 50));
        g2.setStroke(new BasicStroke(2f));

        int size = 50;
        g2.drawLine(25, 25, 25 + size, 25);
        g2.drawLine(25, 25, 25, 25 + size);
        g2.fillOval(22, 22, 6, 6);

        g2.drawLine(w - 25, 25, w - 25 - size, 25);
        g2.drawLine(w - 25, 25, w - 25, 25 + size);
        g2.fillOval(w - 28, 22, 6, 6);

        g2.drawLine(25, h - 25, 25 + size, h - 25);
        g2.drawLine(25, h - 25, 25, h - 25 - size);
        g2.fillOval(22, h - 28, 6, 6);

        g2.drawLine(w - 25, h - 25, w - 25 - size, h - 25);
        g2.drawLine(w - 25, h - 25, w - 25, h - 25 - size);
        g2.fillOval(w - 28, h - 28, 6, 6);
    }

    // --------------------------------------------------------------------------------
    // Styled Button (same as other panels)
    // --------------------------------------------------------------------------------

    private class StyledButton extends JButton {
        enum Style { PRIMARY, SECONDARY, DANGER }

        private final Style style;
        private float hoverProgress = 0f;
        private boolean isHovered = false;

        public StyledButton(String text, Style style) {
            super(text);
            this.style = style;

            setFont(FontManager.buttonFont);
            setForeground(GameTheme.Colors.TEXT_LIGHT);
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
                    bgStart = interpolate(GameTheme.Colors.STONE_DARK, new Color(65, 55, 45), hoverProgress);
                    bgEnd = interpolate(GameTheme.Colors.STONE_MEDIUM, new Color(90, 75, 60), hoverProgress);
                    borderColor = interpolate(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
                }
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

            g2.setColor(getForeground());
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
}