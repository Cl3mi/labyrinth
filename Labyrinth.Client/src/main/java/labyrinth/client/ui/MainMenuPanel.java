package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;

/**
 * Hauptmenü-Panel für das Labyrinth-Spiel.
 *
 * Features:
 * - Zentriertes Layout mit Logo oben
 * - Große, stilvolle Buttons: Einzelspieler, Mehrspieler, Einstellungen
 * - Atmosphärischer Hintergrund mit Labyrinth-Textur
 * - Animierte Hover-Effekte
 * - Mystische Farbpalette passend zum "Verrückten Labyrinth"
 */
public class MainMenuPanel extends JPanel {

    // Callbacks für Button-Aktionen
    private Runnable onSingleplayerClicked;
    private Runnable onMultiplayerClicked;
    private Runnable onOptionsClicked;
    private Runnable onExitClicked;

    // Singleplayer Default-Einstellungen
    private int singleplayerBoardSize = 7;
    private int singleplayerTreasures = 4;
    private int singleplayerTurnTime = 30;
    private int singleplayerGameDuration = 30;
    private String singleplayerUsername = "Player";

    // Multiplayer Username
    private String multiplayerUsername = "Player";

    /**
     * Zeigt den Singleplayer-Einstellungsdialog und startet das Spiel.
     */
    private void showSingleplayerSettingsDialog() {
        JDialog dialog = new JDialog((Frame) SwingUtilities.getWindowAncestor(this), "Einzelspieler - Einstellungen", true);
        dialog.setSize(400, 420);
        dialog.setLocationRelativeTo(this);
        dialog.setResizable(false);

        JPanel mainPanel = new JPanel(new BorderLayout(10, 10));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
        mainPanel.setBackground(new Color(45, 42, 38));

        // Titel
        JLabel titleLabel = new JLabel("Spiel-Einstellungen", SwingConstants.CENTER);
        titleLabel.setFont(new Font("Serif", Font.BOLD, 20));
        titleLabel.setForeground(PRIMARY_GOLD_LIGHT);
        mainPanel.add(titleLabel, BorderLayout.NORTH);

        // Settings Panel
        JPanel settingsPanel = new JPanel(new GridBagLayout());
        settingsPanel.setOpaque(false);
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 10, 10, 10);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;

        // Username
        gbc.gridx = 0;
        gbc.gridy = 0;
        settingsPanel.add(createDialogLabel("Spielername:"), gbc);

        gbc.gridx = 1;
        JTextField usernameField = new JTextField(singleplayerUsername, 15);
        usernameField.setFont(new Font("Arial", Font.PLAIN, 14));
        settingsPanel.add(usernameField, gbc);

        // Spielfeldgröße
        gbc.gridx = 0;
        gbc.gridy = 1;
        settingsPanel.add(createDialogLabel("Spielfeldgröße:"), gbc);

        gbc.gridx = 1;
        JComboBox<String> boardSizeCombo = new JComboBox<>();
        for (int i = 5; i <= 15; i++) {
            boardSizeCombo.addItem(i + "x" + i);
        }
        boardSizeCombo.setSelectedItem(singleplayerBoardSize + "x" + singleplayerBoardSize);
        settingsPanel.add(boardSizeCombo, gbc);

        // Schätze pro Spieler
        gbc.gridx = 0;
        gbc.gridy = 2;
        settingsPanel.add(createDialogLabel("Schätze:"), gbc);

        gbc.gridx = 1;
        SpinnerModel treasureModel = new SpinnerNumberModel(singleplayerTreasures, 1, 24, 1);
        JSpinner treasureSpinner = new JSpinner(treasureModel);
        settingsPanel.add(treasureSpinner, gbc);

        // Runden-Zeit
        gbc.gridx = 0;
        gbc.gridy = 3;
        settingsPanel.add(createDialogLabel("Runden-Zeit:"), gbc);

        gbc.gridx = 1;
        JComboBox<String> turnTimeCombo = new JComboBox<>(new String[]{
                "15 Sekunden", "30 Sekunden", "45 Sekunden",
                "60 Sekunden", "90 Sekunden", "120 Sekunden", "Unbegrenzt"
        });
        turnTimeCombo.setSelectedItem(singleplayerTurnTime + " Sekunden");
        settingsPanel.add(turnTimeCombo, gbc);

        // Spiel-Dauer
        gbc.gridx = 0;
        gbc.gridy = 4;
        settingsPanel.add(createDialogLabel("Spiel-Dauer:"), gbc);

        gbc.gridx = 1;
        JComboBox<String> durationCombo = new JComboBox<>(new String[]{
                "10 Minuten", "15 Minuten", "30 Minuten",
                "45 Minuten", "60 Minuten", "90 Minuten", "Unbegrenzt"
        });
        durationCombo.setSelectedItem(singleplayerGameDuration + " Minuten");
        settingsPanel.add(durationCombo, gbc);

        mainPanel.add(settingsPanel, BorderLayout.CENTER);

        // Buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
        buttonPanel.setOpaque(false);

        JButton cancelButton = new JButton("Abbrechen");
        cancelButton.setPreferredSize(new Dimension(120, 35));
        cancelButton.addActionListener(e -> dialog.dispose());

        JButton startButton = new JButton("Spiel starten");
        startButton.setPreferredSize(new Dimension(120, 35));
        startButton.setBackground(new Color(80, 120, 80));
        startButton.setForeground(Color.WHITE);
        startButton.addActionListener(e -> {
            // Username validieren und speichern
            String enteredUsername = usernameField.getText().trim();
            if (enteredUsername.isEmpty()) {
                JOptionPane.showMessageDialog(dialog,
                    "Bitte geben Sie einen Spielernamen ein.",
                    "Spielername erforderlich",
                    JOptionPane.WARNING_MESSAGE);
                usernameField.requestFocus();
                return;
            }
            singleplayerUsername = enteredUsername;

            // Einstellungen speichern
            String selectedSize = (String) boardSizeCombo.getSelectedItem();
            if (selectedSize != null) {
                singleplayerBoardSize = Integer.parseInt(selectedSize.split("x")[0]);
            }
            singleplayerTreasures = (Integer) treasureSpinner.getValue();

            String selectedTurnTime = (String) turnTimeCombo.getSelectedItem();
            if (selectedTurnTime != null) {
                if (selectedTurnTime.equals("Unbegrenzt")) {
                    singleplayerTurnTime = 9999;
                } else {
                    singleplayerTurnTime = Integer.parseInt(selectedTurnTime.split(" ")[0]);
                }
            }

            String selectedDuration = (String) durationCombo.getSelectedItem();
            if (selectedDuration != null) {
                if (selectedDuration.equals("Unbegrenzt")) {
                    singleplayerGameDuration = 9999;
                } else {
                    singleplayerGameDuration = Integer.parseInt(selectedDuration.split(" ")[0]);
                }
            }

            dialog.dispose();

            // Callback aufrufen um Spiel zu starten
            if (onSingleplayerClicked != null) {
                onSingleplayerClicked.run();
            }
        });

        buttonPanel.add(cancelButton);
        buttonPanel.add(startButton);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        dialog.setContentPane(mainPanel);
        dialog.setVisible(true);
    }

    /**
     * Zeigt einen einfachen Dialog zur Eingabe des Spielernamens für Multiplayer.
     */
    private void showMultiplayerUsernameDialog() {
        JDialog dialog = new JDialog((Frame) SwingUtilities.getWindowAncestor(this), "Mehrspieler - Spielername", true);
        dialog.setSize(400, 200);
        dialog.setLocationRelativeTo(this);
        dialog.setResizable(false);

        JPanel mainPanel = new JPanel(new BorderLayout(10, 15));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(20, 25, 20, 25));
        mainPanel.setBackground(new Color(45, 42, 38));

        // Titel
        JLabel titleLabel = new JLabel("Gib deinen Spielernamen ein", SwingConstants.CENTER);
        titleLabel.setFont(new Font("Serif", Font.BOLD, 18));
        titleLabel.setForeground(PRIMARY_GOLD_LIGHT);
        mainPanel.add(titleLabel, BorderLayout.NORTH);

        // Username Eingabe - zentriertes Panel
        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 5, 10, 5);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.3;
        JLabel nameLabel = createDialogLabel("Spielername:");
        inputPanel.add(nameLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 0.7;
        JTextField usernameField = new JTextField(multiplayerUsername, 15);
        usernameField.setFont(new Font("Arial", Font.PLAIN, 14));
        usernameField.setBackground(new Color(70, 65, 58));
        usernameField.setForeground(new Color(255, 248, 230));
        usernameField.setCaretColor(new Color(255, 248, 230));
        usernameField.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new Color(100, 85, 60), 1),
                BorderFactory.createEmptyBorder(8, 10, 8, 10)
        ));
        usernameField.setPreferredSize(new Dimension(200, 35));
        inputPanel.add(usernameField, gbc);

        mainPanel.add(inputPanel, BorderLayout.CENTER);

        // Buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
        buttonPanel.setOpaque(false);

        JButton cancelButton = new JButton("Abbrechen");
        cancelButton.setPreferredSize(new Dimension(110, 38));
        cancelButton.setFont(new Font("SansSerif", Font.BOLD, 13));
        cancelButton.addActionListener(e -> dialog.dispose());

        JButton joinButton = new JButton("Beitreten");
        joinButton.setPreferredSize(new Dimension(110, 38));
        joinButton.setFont(new Font("SansSerif", Font.BOLD, 13));
        joinButton.setBackground(new Color(80, 120, 80));
        joinButton.setForeground(Color.WHITE);
        joinButton.addActionListener(e -> {
            String enteredUsername = usernameField.getText().trim();
            if (enteredUsername.isEmpty()) {
                JOptionPane.showMessageDialog(dialog,
                        "Bitte geben Sie einen Spielernamen ein.",
                        "Spielername erforderlich",
                        JOptionPane.WARNING_MESSAGE);
                usernameField.requestFocus();
                return;
            }
            multiplayerUsername = enteredUsername;

            dialog.dispose();

            // Callback aufrufen um zur Lobby zu wechseln
            if (onMultiplayerClicked != null) {
                onMultiplayerClicked.run();
            }
        });

        buttonPanel.add(cancelButton);
        buttonPanel.add(joinButton);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        // Enter-Taste soll auch "Beitreten" auslösen
        usernameField.addActionListener(e -> joinButton.doClick());

        // Fokus auf das Textfeld setzen
        dialog.addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowOpened(java.awt.event.WindowEvent e) {
                usernameField.requestFocusInWindow();
                usernameField.selectAll();
            }
        });

        dialog.setContentPane(mainPanel);
        dialog.setVisible(true);
    }

    private JLabel createDialogLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(new Font("Arial", Font.BOLD, 14));
        label.setForeground(new Color(220, 210, 190));
        return label;
    }

    // Hintergrund & Musik
    private Image backgroundImage;
    private Image logoImage;
    // Farben - Mystische Labyrinth-Palette
    private static final Color PRIMARY_GOLD = new Color(218, 165, 32);
    private static final Color PRIMARY_GOLD_LIGHT = new Color(255, 215, 0);
    private static final Color PRIMARY_GOLD_DARK = new Color(184, 134, 11);
    private static final Color STONE_DARK = new Color(45, 42, 38);
    private static final Color STONE_MEDIUM = new Color(82, 75, 66);
    private static final Color TEXT_LIGHT = new Color(255, 248, 230);
    private static final Color SHADOW_COLOR = new Color(0, 0, 0, 120);

    // Fonts
    private Font titleFont;
    private Font buttonFont;
    private Font subtitleFont;

    public MainMenuPanel() {
        initFonts();
        loadResources();
        initMusic();
        setupUI();
    }

    @Override
    public void setVisible(boolean aFlag) {
        super.setVisible(aFlag);
        if (aFlag) {
            initMusic();
        }
    }

    // --------------------------------------------------------------------------------
    // Initialisierung
    // --------------------------------------------------------------------------------

    private void initFonts() {
        try {
            titleFont = new Font("Serif", Font.BOLD, 48);
            buttonFont = new Font("Serif", Font.BOLD, 20);
            subtitleFont = new Font("Serif", Font.ITALIC, 14);

            if (isFontAvailable("Cinzel")) {
                titleFont = new Font("Cinzel", Font.BOLD, 48);
                buttonFont = new Font("Cinzel", Font.BOLD, 20);
            }
            if (isFontAvailable("Lora")) {
                subtitleFont = new Font("Lora", Font.ITALIC, 14);
            }
        } catch (Exception e) {
            titleFont = new Font("Serif", Font.BOLD, 48);
            buttonFont = new Font("Serif", Font.BOLD, 20);
            subtitleFont = new Font("Serif", Font.ITALIC, 14);
        }
    }

    private boolean isFontAvailable(String fontName) {
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        String[] fontFamilies = ge.getAvailableFontFamilyNames();
        for (String family : fontFamilies) {
            if (family.equalsIgnoreCase(fontName)) {
                return true;
            }
        }
        return false;
    }

    private void loadResources() {
        // Hintergrundbild laden
        try {
            var url = getClass().getResource("/images/ui/background.png");
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
            } else {
                System.err.println("Background not found: /images/ui/background.png");
            }
        } catch (Exception e) {
            System.err.println("Error loading background: " + e.getMessage());
        }

        // Logo laden
        try {
            var logoUrl = getClass().getResource("/images/ui/logo.png");
            if (logoUrl != null) {
                logoImage = new ImageIcon(logoUrl).getImage();
            } else {
                System.err.println("Logo not found: /images/ui/logo.png");
            }
        } catch (Exception e) {
            System.err.println("Error loading logo: " + e.getMessage());
        }
    }

    private void initMusic() {
        java.util.prefs.Preferences prefs =
                java.util.prefs.Preferences.userNodeForPackage(OptionsPanel.class);

        float volume = prefs.getInt("musicVolume", 50) / 100f;

        AudioPlayer.getInstance().setMusicVolume(volume);
        AudioPlayer.getInstance().playMenuMusic();
    }


    /**
     * Setzt die Musik-Lautstärke (0-100).
     */
    public void setMusicVolume(int volume) {
        float vol = Math.max(0, Math.min(100, volume)) / 100.0f;
        AudioPlayer.getInstance().setMusicVolume(vol);
    }

    public void stopMusic() {
        AudioPlayer.getInstance().stopMusic();
    }

    private void setupUI() {
        setOpaque(false);
        setLayout(new GridBagLayout());

        JPanel contentPanel = new JPanel();
        contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));
        contentPanel.setOpaque(false);
        contentPanel.setBorder(new EmptyBorder(20, 40, 40, 40));

        // Vertikaler Abstand oben (flexibel)
        contentPanel.add(Box.createVerticalGlue());

        // Logo-Bereich
        JPanel logoPanel = createLogoPanel();
        logoPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(logoPanel);

        contentPanel.add(Box.createVerticalStrut(15));

        // Untertitel
        JLabel subtitleLabel = new JLabel("Das mystische Abenteuer beginnt... DiBSE 2025");
        subtitleLabel.setFont(new Font("SansSerif", Font.BOLD, 24));
        subtitleLabel.setForeground(new Color(255, 153, 0, 255));
        subtitleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(subtitleLabel);

        contentPanel.add(Box.createVerticalStrut(30));

        // Button-Bereich
        JPanel buttonContainer = createButtonPanel();
        buttonContainer.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(buttonContainer);

        // Vertikaler Abstand unten (flexibel)
        contentPanel.add(Box.createVerticalGlue());

        // Footer
        JPanel footerPanel = createFooterPanel();
        footerPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(footerPanel);

        // GridBagConstraints für zentrierte Platzierung mit Skalierung
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        add(contentPanel, gbc);
    }

    // --------------------------------------------------------------------------------
    // UI-Komponenten erstellen
    // --------------------------------------------------------------------------------

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
                        // Skaliere basierend auf Panel-Größe (responsive)
                        int panelWidth = getWidth();
                        int panelHeight = getHeight();

                        // Logo soll ca. 80% der Panel-Breite nutzen, aber max 600px
                        int targetWidth = Math.min((int)(panelWidth * 0.8), 600);
                        int targetHeight = Math.min((int)(panelHeight * 0.9), 200);

                        double scaleX = (double) targetWidth / originalWidth;
                        double scaleY = (double) targetHeight / originalHeight;
                        double scale = Math.min(scaleX, scaleY);

                        int scaledWidth = (int) (originalWidth * scale);
                        int scaledHeight = (int) (originalHeight * scale);

                        int x = centerX - scaledWidth / 2;
                        int y = centerY - scaledHeight / 2;

                        // Logo zeichnen (nur einmal)
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

                g2.setFont(titleFont.deriveFont(32f));
                FontMetrics fm = g2.getFontMetrics();
                int titleWidth = fm.stringWidth(title);

                g2.setColor(SHADOW_COLOR);
                g2.drawString(title, centerX - titleWidth / 2 + 3, 43);
                g2.setColor(PRIMARY_GOLD_LIGHT);
                g2.drawString(title, centerX - titleWidth / 2, 40);

                g2.setFont(titleFont.deriveFont(52f));
                fm = g2.getFontMetrics();
                int title2Width = fm.stringWidth(title2);

                for (int i = 8; i > 0; i--) {
                    float alpha = 0.04f * i;
                    g2.setColor(new Color(255, 215, 0, (int) (alpha * 255)));
                    g2.drawString(title2, centerX - title2Width / 2 - i / 2, 105);
                    g2.drawString(title2, centerX - title2Width / 2 + i / 2, 105);
                }

                g2.setColor(SHADOW_COLOR);
                g2.drawString(title2, centerX - title2Width / 2 + 4, 109);

                GradientPaint goldGradient = new GradientPaint(
                        centerX - title2Width / 2, 60, PRIMARY_GOLD_LIGHT,
                        centerX + title2Width / 2, 115, PRIMARY_GOLD_DARK
                );
                g2.setPaint(goldGradient);
                g2.drawString(title2, centerX - title2Width / 2, 105);
            }
        };
        panel.setOpaque(false);
        // Keine feste Größe - skaliert mit dem Fenster
        panel.setPreferredSize(new Dimension(600, 200));
        panel.setMinimumSize(new Dimension(300, 100));
        return panel;
    }

    private JPanel createButtonPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setOpaque(false);

        // Einzelspieler Button
        MenuButton singleplayerBtn = new MenuButton("Einzelspieler", "Starte ein Solo-Abenteuer");
        singleplayerBtn.addActionListener(e -> showSingleplayerSettingsDialog());
        singleplayerBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        panel.add(singleplayerBtn);

        panel.add(Box.createVerticalStrut(12));

        // Mehrspieler Button
        MenuButton multiplayerBtn = new MenuButton("Mehrspieler", "Spiele online mit Freunden");
        multiplayerBtn.addActionListener(e -> showMultiplayerUsernameDialog());
        multiplayerBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        panel.add(multiplayerBtn);

        panel.add(Box.createVerticalStrut(12));

        // Einstellungen Button
        MenuButton optionsBtn = new MenuButton("Einstellungen", "Grafik, Audio & mehr");
        optionsBtn.addActionListener(e -> {
            if (onOptionsClicked != null) onOptionsClicked.run();
        });
        optionsBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        panel.add(optionsBtn);

        panel.add(Box.createVerticalStrut(20));

        // Beenden Button
        ExitButton exitBtn = new ExitButton("Beenden");
        exitBtn.addActionListener(e -> {
            if (onExitClicked != null) {
                onExitClicked.run();
            } else {
                System.exit(0);
            }
        });
        exitBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        panel.add(exitBtn);

        return panel;
    }

    private JPanel createFooterPanel() {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel.setOpaque(false);

        JLabel versionLabel = new JLabel("Version 1.0  |  © 2025 Gruppe 1");
        versionLabel.setFont(new Font("SansSerif", Font.PLAIN, 11));
        versionLabel.setForeground(new Color(200, 190, 170, 150));
        panel.add(versionLabel);

        return panel;
    }

    // --------------------------------------------------------------------------------
    // Callback Setter
    // --------------------------------------------------------------------------------

    public void setOnSingleplayerClicked(Runnable callback) {
        this.onSingleplayerClicked = callback;
    }

    public void setOnMultiplayerClicked(Runnable callback) {
        this.onMultiplayerClicked = callback;
    }

    public void setOnOptionsClicked(Runnable callback) {
        this.onOptionsClicked = callback;
    }

    public void setOnExitClicked(Runnable callback) {
        this.onExitClicked = callback;
    }

    // Singleplayer Einstellungen Getter
    public int getSingleplayerBoardSize() {
        return singleplayerBoardSize;
    }

    public int getSingleplayerTreasures() {
        return singleplayerTreasures;
    }

    public int getSingleplayerTurnTime() {
        return singleplayerTurnTime;
    }

    public int getSingleplayerGameDuration() {
        return singleplayerGameDuration;
    }

    public String getSingleplayerUsername() {
        return singleplayerUsername;
    }

    public void setSingleplayerUsername(String username) {
        if (username != null && !username.isBlank()) {
            this.singleplayerUsername = username;
        }
    }

    public String getMultiplayerUsername() {
        return multiplayerUsername;
    }

    public void setMultiplayerUsername(String username) {
        if (username != null && !username.isBlank()) {
            this.multiplayerUsername = username;
        }
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
            GradientPaint gradient = new GradientPaint(
                    0, 0, STONE_DARK,
                    0, h, new Color(75, 45, 90)
            );
            g2.setPaint(gradient);
            g2.fillRect(0, 0, w, h);
        }

        // Dunkler Overlay
        g2.setColor(new Color(0, 0, 0, 60));
        g2.fillRect(0, 0, w, h);

        // Vignette
        drawVignette(g2, w, h);

        // Dekorative Ecken
        drawDecorativeCorners(g2, w, h);

        g2.dispose();
        super.paintComponent(g);
    }

    private void drawVignette(Graphics2D g2, int w, int h) {
        int centerX = w / 2;
        int centerY = h / 2;
        float radius = Math.max(w, h) * 0.8f;

        RadialGradientPaint vignette = new RadialGradientPaint(
                centerX, centerY, radius,
                new float[]{0.3f, 0.7f, 1.0f},
                new Color[]{
                        new Color(0, 0, 0, 0),
                        new Color(0, 0, 0, 50),
                        new Color(0, 0, 0, 130)
                }
        );
        g2.setPaint(vignette);
        g2.fillRect(0, 0, w, h);
    }

    private void drawDecorativeCorners(Graphics2D g2, int w, int h) {
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(new Color(218, 165, 32, 50));
        g2.setStroke(new BasicStroke(2f));

        int size = 50;

        // Oben links
        g2.drawLine(25, 25, 25 + size, 25);
        g2.drawLine(25, 25, 25, 25 + size);
        g2.fillOval(22, 22, 6, 6);

        // Oben rechts
        g2.drawLine(w - 25, 25, w - 25 - size, 25);
        g2.drawLine(w - 25, 25, w - 25, 25 + size);
        g2.fillOval(w - 28, 22, 6, 6);

        // Unten links
        g2.drawLine(25, h - 25, 25 + size, h - 25);
        g2.drawLine(25, h - 25, 25, h - 25 - size);
        g2.fillOval(22, h - 28, 6, 6);

        // Unten rechts
        g2.drawLine(w - 25, h - 25, w - 25 - size, h - 25);
        g2.drawLine(w - 25, h - 25, w - 25, h - 25 - size);
        g2.fillOval(w - 28, h - 28, 6, 6);
    }

    // --------------------------------------------------------------------------------
    // Custom Menu Button
    // --------------------------------------------------------------------------------

    private class MenuButton extends JButton {
        private final String subtitle;
        private float hoverProgress = 0f;
        private final Timer animationTimer;
        private boolean isHovered = false;

        public MenuButton(String text, String subtitle) {
            super(text);
            this.subtitle = subtitle;

            setFont(buttonFont);
            setForeground(TEXT_LIGHT);
            setFocusPainted(false);
            setBorderPainted(false);
            setContentAreaFilled(false);
            setCursor(new Cursor(Cursor.HAND_CURSOR));

            // Flexible Größe - skaliert mit dem Layout
            setPreferredSize(new Dimension(320, 70));
            setMinimumSize(new Dimension(200, 50));
            setMaximumSize(new Dimension(450, 90));

            animationTimer = new Timer(16, e -> {
                if (isHovered && hoverProgress < 1f) {
                    hoverProgress = Math.min(1f, hoverProgress + 0.12f);
                    repaint();
                } else if (!isHovered && hoverProgress > 0f) {
                    hoverProgress = Math.max(0f, hoverProgress - 0.12f);
                    repaint();
                }
            });
            animationTimer.start();

            addMouseListener(new MouseAdapter() {
                @Override
                public void mouseEntered(MouseEvent e) {
                    isHovered = true;
                }

                @Override
                public void mouseExited(MouseEvent e) {
                    isHovered = false;
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
            int arc = 10;

            // Schatten
            g2.setColor(new Color(0, 0, 0, (int) (70 + 30 * hoverProgress)));
            g2.fill(new RoundRectangle2D.Float(4, 5, w - 8, h - 7, arc, arc));

            // Hintergrund
            Color bgStart = interpolateColor(STONE_DARK, new Color(65, 50, 35), hoverProgress);
            Color bgEnd = interpolateColor(STONE_MEDIUM, new Color(95, 70, 45), hoverProgress);
            GradientPaint bgGradient = new GradientPaint(0, 0, bgStart, 0, h, bgEnd);
            g2.setPaint(bgGradient);
            g2.fill(new RoundRectangle2D.Float(2, 2, w - 4, h - 4, arc, arc));

            // Goldener Rand
            Color borderColor = interpolateColor(PRIMARY_GOLD_DARK, PRIMARY_GOLD_LIGHT, hoverProgress);
            g2.setColor(borderColor);
            g2.setStroke(new BasicStroke(2f + hoverProgress * 0.5f));
            g2.draw(new RoundRectangle2D.Float(2, 2, w - 5, h - 5, arc, arc));

            // Glanz
            g2.setColor(new Color(255, 255, 255, (int) (15 + 20 * hoverProgress)));
            g2.fill(new RoundRectangle2D.Float(4, 4, w - 8, (h - 8) / 3f, arc - 2, arc - 2));

            // Text
            g2.setFont(buttonFont);
            FontMetrics fm = g2.getFontMetrics();
            String text = getText();
            int textWidth = fm.stringWidth(text);
            int textX = (w - textWidth) / 2;
            int textY = h / 2 - 3;

            g2.setColor(SHADOW_COLOR);
            g2.drawString(text, textX + 2, textY + 2);

            Color textColor = interpolateColor(TEXT_LIGHT, PRIMARY_GOLD_LIGHT, hoverProgress * 0.4f);
            g2.setColor(textColor);
            g2.drawString(text, textX, textY);

            // Untertitel
            if (subtitle != null && !subtitle.isEmpty()) {
                g2.setFont(subtitleFont.deriveFont(11f));
                fm = g2.getFontMetrics();
                int subWidth = fm.stringWidth(subtitle);
                g2.setColor(new Color(200, 190, 170, (int) (140 + 40 * hoverProgress)));
                g2.drawString(subtitle, (w - subWidth) / 2, textY + 18);
            }

            g2.dispose();
        }

        private Color interpolateColor(Color c1, Color c2, float t) {
            int r = (int) (c1.getRed() + (c2.getRed() - c1.getRed()) * t);
            int gr = (int) (c1.getGreen() + (c2.getGreen() - c1.getGreen()) * t);
            int b = (int) (c1.getBlue() + (c2.getBlue() - c1.getBlue()) * t);
            int a = (int) (c1.getAlpha() + (c2.getAlpha() - c1.getAlpha()) * t);
            return new Color(r, gr, b, a);
        }
    }

    // --------------------------------------------------------------------------------
    // Exit Button
    // --------------------------------------------------------------------------------

    private class ExitButton extends JButton {
        private boolean isHovered = false;

        public ExitButton(String text) {
            super(text);
            setFont(new Font("SansSerif", Font.BOLD, 13));
            setForeground(new Color(200, 100, 100));
            setFocusPainted(false);
            setBorderPainted(false);
            setContentAreaFilled(false);
            setCursor(new Cursor(Cursor.HAND_CURSOR));
            setPreferredSize(new Dimension(130, 38));
            setMaximumSize(new Dimension(150, 45));
            setMinimumSize(new Dimension(100, 32));

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
                // Hover: Helleres Rot mit Hintergrund
                g2.setColor(new Color(140, 50, 50, 180));
                g2.fillRoundRect(0, 0, w, h, 8, 8);
                g2.setColor(new Color(255, 17, 17));
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, w - 2, h - 2, 8, 8);
                g2.setColor(new Color(255, 200, 200));
            } else {
                // Normal: Dunkelrot mit Rahmen
                g2.setColor(new Color(80, 40, 40, 120));
                g2.fillRoundRect(0, 0, w, h, 8, 8);
                g2.setColor(new Color(150, 80, 80));
                g2.setStroke(new BasicStroke(1.5f));
                g2.drawRoundRect(1, 1, w - 2, h - 2, 8, 8);
                g2.setColor(new Color(200, 130, 130));
            }

            g2.setFont(getFont());
            FontMetrics fm = g2.getFontMetrics();
            String text = getText();
            int textX = (w - fm.stringWidth(text)) / 2;
            int textY = (h + fm.getAscent() - fm.getDescent()) / 2;
            g2.drawString(text, textX, textY);

            g2.dispose();
        }

    }

    private Font loadFont(String resourcePath, float size) {
        try (var is = getClass().getResourceAsStream(resourcePath)) {
            if (is == null) throw new IllegalArgumentException("Font nicht gefunden: " + resourcePath);
            Font base = Font.createFont(Font.TRUETYPE_FONT, is);
            return base.deriveFont(size);
        } catch (Exception e) {
            e.printStackTrace();
            return new Font("SansSerif", Font.PLAIN, Math.round(size)); // Fallback
        }
    }
}