package labyrinth.client.ui;

import labyrinth.client.messaging.GameClient;
import labyrinth.client.ui.Styles.StyledButton;
import labyrinth.client.ui.Styles.StyledContextMenu;
import labyrinth.client.ui.Styles.StyledDialog;
import labyrinth.client.ui.Styles.StyledPlayerCardRenderer;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.contracts.models.BoardSize;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
import lombok.Setter;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.geom.RoundRectangle2D;

/**
 * Multiplayer-Lobby-Panel für das Labyrinth-Spiel.
 * Redesigned mit mystischem Labyrinth-Thema passend zum MainMenuPanel.
 */
public class MultiplayerLobbyPanel extends JPanel {

    @Setter
    private GameClient client;

    @Setter
    private String localPlayerId;

    @Setter
    private String localUsername;

    // UI-Komponenten
    private JLabel connectionLabel;
    private JLabel lobbyTitleLabel;
    private DefaultListModel<String> playerListModel;
    private JList<String> playerList;
    private JPanel settingsPanel;
    private StyledButton startButton;
    private StyledButton cancelReconnectButton;
    private StyledButton backButton;

    // Hintergrund
    private Image backgroundImage;

    // Letzter Lobby-State
    private volatile LobbyStateEventPayload lastLobbyState;

    // Game configuration state
    private int configBoardSize = 7;
    private int configTreasuresToWin = 4;
    private int configBonusCount = 4;
    private int configTurnTimeSeconds = 30;
    private int configGameDurationMinutes = 30;
    private String configUsername = "Player";

    // UI Components für Settings
    private JTextField usernameField;
    private JComboBox<String> boardSizeCombo;
    private JSpinner treasureSpinner;
    private JSpinner bonusSpinner;
    private JComboBox<String> turnTimeCombo;
    private JComboBox<String> durationCombo;

    // Callbacks
    private Runnable onBackToMenu;



    public MultiplayerLobbyPanel(String localPlayerId) {
        this.localPlayerId = localPlayerId;

        FontManager.initFonts();
        loadBackgroundImage();
        setupUI();

        // Theme-Änderungen überwachen
        ThemeManager.getInstance().addThemeChangeListener(() -> {
            loadBackgroundImage();
            repaint();
        });
    }





    private void loadBackgroundImage() {
        try {
            String imagePath = ThemeManager.getInstance().getBackgroundImagePath();
            var url = getClass().getResource(imagePath);
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
                System.out.println("[MultiplayerLobbyPanel] Loaded background: " + imagePath);
            }
        } catch (Exception e) {
            System.err.println("Error loading background: " + e.getMessage());
        }
    }

    private void setupUI() {
        setOpaque(false);
        setLayout(new BorderLayout(0, 15));
        setBorder(new EmptyBorder(20, 30, 20, 30));

        // Header
        add(createHeader(), BorderLayout.NORTH);

        // Center - Settings und Spielerliste
        add(createCenterPanel(), BorderLayout.CENTER);

        // Footer - Buttons
        add(createFooter(), BorderLayout.SOUTH);
    }

    private JPanel createHeader() {
        JPanel header = new JPanel(new BorderLayout(20, 0));
        header.setOpaque(false);

        // Zurück-Button
        backButton = new StyledButton("Zurück", StyledButton.Style.SECONDARY);
        backButton.setPreferredSize(new Dimension(140, 40));
        backButton.addActionListener(e -> {
            boolean confirmed = StyledDialog.showConfirm(this,
                    "Lobby verlassen?",
                    "Möchtest du die Lobby wirklich verlassen?");
            if (confirmed && onBackToMenu != null) {
                onBackToMenu.run();
            }
        });
        StyledTooltipManager.setTooltip(backButton, "Zurück", "Zurück zum Hauptmenü");
        StyledContextMenu.attachTo(backButton);

        JPanel leftPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        leftPanel.setOpaque(false);
        leftPanel.add(backButton);
        header.add(leftPanel, BorderLayout.WEST);

        // Titel und Status
        JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setOpaque(false);

        lobbyTitleLabel = new JLabel("Multiplayer Lobby");
        lobbyTitleLabel.setFont(FontManager.titleFont);
        lobbyTitleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        lobbyTitleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        connectionLabel = new JLabel("Verbindung wird aufgebaut...");
        connectionLabel.setFont(FontManager.labelFont);
        connectionLabel.setForeground(new Color(200, 160, 60));
        connectionLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        centerPanel.add(Box.createVerticalStrut(5));
        centerPanel.add(lobbyTitleLabel);
        centerPanel.add(Box.createVerticalStrut(5));
        centerPanel.add(connectionLabel);

        header.add(centerPanel, BorderLayout.CENTER);

        // Platzhalter rechts
        JPanel rightPanel = new JPanel();
        rightPanel.setOpaque(false);
        rightPanel.setPreferredSize(new Dimension(140, 40));
        header.add(rightPanel, BorderLayout.EAST);

        return header;
    }

    private JPanel createCenterPanel() {
        JPanel center = new JPanel(new GridBagLayout());
        center.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(10, 10, 10, 10);

        // Settings Card (links)
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.4;
        gbc.weighty = 1.0;
        settingsPanel = createSettingsCard();
        center.add(settingsPanel, gbc);

        // Player List Card (rechts)
        gbc.gridx = 1;
        gbc.weightx = 0.6;
        center.add(createPlayerListCard(), gbc);

        return center;
    }

    private JPanel createSettingsCard() {
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

                // Inner glow
                g2.setColor(new Color(255, 255, 255, 10));
                g2.fill(new RoundRectangle2D.Float(2, 2, getWidth() - 4, 40, 13, 13));

                g2.dispose();
                super.paintComponent(g);
            }
        };
        card.setOpaque(false);
        card.setLayout(new BorderLayout(0, 15));
        card.setBorder(new EmptyBorder(20, 25, 20, 25));

        // Titel
        JLabel titleLabel = new JLabel("⚙ Spiel-Einstellungen");
        titleLabel.setFont(new Font("Serif", Font.BOLD, 18));
        titleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        card.add(titleLabel, BorderLayout.NORTH);

        // Settings Grid
        JPanel settingsGrid = new JPanel(new GridBagLayout());
        settingsGrid.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 5, 8, 5);
        gbc.anchor = GridBagConstraints.WEST;

        // Spielername (nur Anzeige - wird im Hauptmenü festgelegt)
        gbc.gridx = 0; gbc.gridy = 0; gbc.weightx = 0.4;
        settingsGrid.add(createStyledLabel("Spielername:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.6;
        usernameField = createStyledTextField(configUsername);
        usernameField.setEditable(false);
        StyledTooltipManager.setTooltip(usernameField, "Spielername", "Dein Anzeigename im Spiel (wird im Hauptmenü festgelegt)");
        settingsGrid.add(usernameField, gbc);

        // Spielfeldgröße
        gbc.gridx = 0; gbc.gridy = 1; gbc.weightx = 0.4;
        settingsGrid.add(createStyledLabel("Spielfeldgröße:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.6;
        boardSizeCombo = createStyledComboBox();
        for (int i = 3; i <= 11; i += 2) {
            boardSizeCombo.addItem(i + " × " + i);
        }
        boardSizeCombo.setSelectedItem("7 × 7");
        boardSizeCombo.addActionListener(e -> {
            String selected = (String) boardSizeCombo.getSelectedItem();
            if (selected != null) {
                configBoardSize = Integer.parseInt(selected.split(" ")[0]);
            }
        });
        StyledTooltipManager.setTooltip(boardSizeCombo, "Spielfeldgröße", "Größe des Spielfelds (Standardwert: 7×7)");
        settingsGrid.add(boardSizeCombo, gbc);

        // Schätze
        gbc.gridx = 0; gbc.gridy = 2;
        settingsGrid.add(createStyledLabel("Schätze pro Spieler:"), gbc);

        gbc.gridx = 1;
        treasureSpinner = createStyledSpinner(configTreasuresToWin, 1, 24);
        treasureSpinner.addChangeListener(e -> configTreasuresToWin = (Integer) treasureSpinner.getValue());
        StyledTooltipManager.setTooltip(treasureSpinner, "Schätze", "Anzahl der Schätze, die jeder Spieler sammeln muss");
        settingsGrid.add(treasureSpinner, gbc);

        // Bonus-Anzahl
        gbc.gridx = 0; gbc.gridy = 3;
        settingsGrid.add(createStyledLabel("Bonus-Anzahl:"), gbc);

        gbc.gridx = 1;
        bonusSpinner = createStyledSpinner(configBonusCount, 0, 12);
        bonusSpinner.addChangeListener(e -> configBonusCount = (Integer) bonusSpinner.getValue());
        settingsGrid.add(bonusSpinner, gbc);

        // Runden-Zeit
        gbc.gridx = 0; gbc.gridy = 4;
        settingsGrid.add(createStyledLabel("Runden-Zeit:"), gbc);

        gbc.gridx = 1;
        turnTimeCombo = createStyledComboBox();
        turnTimeCombo.addItem("15 Sekunden");
        turnTimeCombo.addItem("30 Sekunden");
        turnTimeCombo.addItem("45 Sekunden");
        turnTimeCombo.addItem("60 Sekunden");
        turnTimeCombo.addItem("90 Sekunden");
        turnTimeCombo.addItem("120 Sekunden");
        turnTimeCombo.setSelectedItem("30 Sekunden");
        turnTimeCombo.addActionListener(e -> {
            String selected = (String) turnTimeCombo.getSelectedItem();
            if (selected != null) {
                configTurnTimeSeconds = Integer.parseInt(selected.split(" ")[0]);
            }
        });
        StyledTooltipManager.setTooltip(turnTimeCombo, "Runden-Zeit", "Zeitlimit pro Spielzug");
        settingsGrid.add(turnTimeCombo, gbc);

        // Spiel-Dauer
        gbc.gridx = 0; gbc.gridy = 5;
        settingsGrid.add(createStyledLabel("Spiel-Dauer:"), gbc);

        gbc.gridx = 1;
        durationCombo = createStyledComboBox();
        durationCombo.addItem("10 Minuten");
        durationCombo.addItem("15 Minuten");
        durationCombo.addItem("30 Minuten");
        durationCombo.addItem("45 Minuten");
        durationCombo.addItem("60 Minuten");
        durationCombo.addItem("90 Minuten");
        durationCombo.setSelectedItem("30 Minuten");
        durationCombo.addActionListener(e -> {
            String selected = (String) durationCombo.getSelectedItem();
            if (selected != null) {
                configGameDurationMinutes = Integer.parseInt(selected.split(" ")[0]);
            }
        });
        StyledTooltipManager.setTooltip(durationCombo, "Spiel-Dauer", "Maximale Gesamtdauer des Spiels");
        settingsGrid.add(durationCombo, gbc);

        // Hinweis
        gbc.gridx = 0; gbc.gridy = 6; gbc.gridwidth = 2;
        gbc.insets = new Insets(15, 5, 5, 5);
        JLabel hintLabel = new JLabel("Nur der Admin kann Einstellungen ändern");
        hintLabel.setFont(new Font("SansSerif", Font.ITALIC, 11));
        hintLabel.setForeground(GameTheme.Colors.TEXT_MUTED);
        settingsGrid.add(hintLabel, gbc);

        card.add(settingsGrid, BorderLayout.CENTER);

        return card;
    }

    private JPanel createPlayerListCard() {
        JPanel card = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                g2.setColor(GameTheme.Colors.CARD_BG);
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 15, 15));

                g2.setColor(GameTheme.Colors.CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 2, getHeight() - 2, 15, 15));

                g2.setColor(new Color(255, 255, 255, 10));
                g2.fill(new RoundRectangle2D.Float(2, 2, getWidth() - 4, 40, 13, 13));

                g2.dispose();
                super.paintComponent(g);
            }
        };
        card.setOpaque(false);
        card.setLayout(new BorderLayout(0, 15));
        card.setBorder(new EmptyBorder(20, 25, 20, 25));

        // Titel
        JLabel titleLabel = new JLabel("👥 Spieler in der Lobby");
        titleLabel.setFont(new Font("Serif", Font.BOLD, 18));
        titleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        card.add(titleLabel, BorderLayout.NORTH);

        // Player List
        playerListModel = new DefaultListModel<>();
        playerList = new JList<>(playerListModel);
        playerList.setCellRenderer(new StyledPlayerCardRenderer());
        playerList.setOpaque(false);
        playerList.setBackground(new Color(0, 0, 0, 0));
        playerList.setFixedCellHeight(70);
        playerList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        // Add right-click context menu to player list
        StyledContextMenu.attachToList(playerList, item -> {
            String playerName = item != null ? item.toString() : "Unbekannter Spieler";
            return "Spieler: " + playerName;
        });

        JScrollPane scrollPane = new JScrollPane(playerList);
        scrollPane.setOpaque(false);
        scrollPane.getViewport().setOpaque(false);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        card.add(scrollPane, BorderLayout.CENTER);

        return card;
    }

    private JPanel createFooter() {
        JPanel footer = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 10));
        footer.setOpaque(false);

        cancelReconnectButton = new StyledButton("Wiederverbindung abbrechen", StyledButton.Style.DANGER);
        cancelReconnectButton.setPreferredSize(new Dimension(220, 45));
        cancelReconnectButton.setVisible(false);
        cancelReconnectButton.addActionListener(e -> onCancelReconnect());
        StyledTooltipManager.setTooltip(cancelReconnectButton, "Abbrechen", "Wiederverbindungsversuch abbrechen");
        StyledContextMenu.attachTo(cancelReconnectButton);
        footer.add(cancelReconnectButton);

        startButton = new StyledButton("Spiel starten", StyledButton.Style.PRIMARY);
        startButton.setPreferredSize(new Dimension(200, 50));
        startButton.setEnabled(false);
        startButton.addActionListener(e -> onStartGameClicked());
        StyledTooltipManager.setTooltip(startButton, "Spiel starten", "Startet das Spiel mit den aktuellen Spielern");
        StyledContextMenu.attachTo(startButton);
        footer.add(startButton);

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

    private JComboBox<String> createStyledComboBox() {
        JComboBox<String> combo = new JComboBox<>();
        combo.setFont(new Font("SansSerif", Font.PLAIN, 13));
        combo.setBackground(GameTheme.Colors.STONE_MEDIUM);
        combo.setForeground(GameTheme.Colors.TEXT_LIGHT);
        combo.setPreferredSize(new Dimension(150, 30));
        return combo;
    }

    private JSpinner createStyledSpinner(int value, int min, int max) {
        SpinnerModel model = new SpinnerNumberModel(value, min, max, 1);
        JSpinner spinner = new JSpinner(model);
        spinner.setFont(new Font("SansSerif", Font.PLAIN, 13));
        spinner.setPreferredSize(new Dimension(150, 30));
        return spinner;
    }

    private JTextField createStyledTextField(String defaultText) {
        JTextField textField = new JTextField(defaultText, 15);
        textField.setFont(new Font("SansSerif", Font.PLAIN, 13));
        textField.setBackground(GameTheme.Colors.STONE_MEDIUM);
        textField.setForeground(GameTheme.Colors.TEXT_LIGHT);
        textField.setCaretColor(GameTheme.Colors.TEXT_LIGHT);
        textField.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(GameTheme.Colors.CARD_BORDER, 1),
                BorderFactory.createEmptyBorder(5, 8, 5, 8)
        ));
        textField.setPreferredSize(new Dimension(150, 30));
        return textField;
    }

    // --------------------------------------------------------------------------------
    // Public API
    // --------------------------------------------------------------------------------

    public void setOnBackToMenu(Runnable callback) {
        this.onBackToMenu = callback;
    }

    public void setConnected(boolean connected) {
        SwingUtilities.invokeLater(() -> {
            if (connected) {
                connectionLabel.setText("✓ Verbunden mit Server");
                connectionLabel.setForeground(new Color(100, 200, 100));
            } else {
                connectionLabel.setText("✗ Nicht verbunden");
                connectionLabel.setForeground(new Color(200, 100, 100));
                startButton.setEnabled(false);
            }
        });
    }

    public void setStatusText(String text, Color color) {
        SwingUtilities.invokeLater(() -> {
            connectionLabel.setText(text != null ? text : "");
            if (color != null) {
                connectionLabel.setForeground(color);
            }
        });
    }

    public void setReconnecting(boolean reconnecting) {
        SwingUtilities.invokeLater(() -> {
            cancelReconnectButton.setVisible(reconnecting);
            startButton.setVisible(!reconnecting);
        });
    }

    /**
     * Gibt den im Settings-Panel eingegebenen Username zurück.
     */
    public String getMultiplayerUsername() {
        return configUsername;
    }

    /**
     * Setzt den Username im Settings-Panel.
     */
    public void setMultiplayerUsername(String username) {
        if (username != null && !username.isBlank()) {
            this.configUsername = username;
            if (usernameField != null) {
                usernameField.setText(username);
            }
        }
    }


    private void onCancelReconnect() {
        boolean confirmed = StyledDialog.showConfirm(this,
                "Abbrechen bestätigen",
                "Wiederverbindung abbrechen und zum Menü zurückkehren?");
        if (confirmed && onBackToMenu != null) {
            onBackToMenu.run();
        }
    }

    public void updateLobby(LobbyStateEventPayload lobby) {
        this.lastLobbyState = lobby;

        SwingUtilities.invokeLater(() -> {
            playerListModel.clear();

            if (lobby == null || lobby.getPlayers() == null) {
                startButton.setEnabled(false);
                return;
            }

            PlayerInfo[] players = lobby.getPlayers();
            boolean localFound = false;
            boolean localIsAdmin = false;

            for (PlayerInfo p : players) {
                if (p == null) continue;

                StringBuilder sb = new StringBuilder();

                if (Boolean.FALSE.equals(p.getIsConnected())) {
                    sb.append("[OFFLINE] ");
                }
                if (Boolean.TRUE.equals(p.getIsAdmin())) {
                    sb.append("(Admin) ");
                }

                String name = p.getName() != null ? p.getName() : "<unbekannt>";
                sb.append(name);

                boolean idMatch = localPlayerId != null && p.getId() != null && localPlayerId.equals(p.getId());
                boolean nameMatch = localUsername != null && p.getName() != null && localUsername.equals(p.getName());

                if (idMatch || (!localFound && nameMatch)) {
                    sb.append(" (Du)");
                    localFound = true;
                    localIsAdmin = Boolean.TRUE.equals(p.getIsAdmin());
                }

                playerListModel.addElement(sb.toString());
            }

            // Enable/disable settings
            enableSettingsPanel(localFound && localIsAdmin);
            startButton.setEnabled(localFound && localIsAdmin);
        });
    }

    private void enableSettingsPanel(boolean enabled) {
        if (usernameField != null) usernameField.setEnabled(enabled);
        if (boardSizeCombo != null) boardSizeCombo.setEnabled(enabled);
        if (treasureSpinner != null) treasureSpinner.setEnabled(enabled);
        if (turnTimeCombo != null) turnTimeCombo.setEnabled(enabled);
        if (durationCombo != null) durationCombo.setEnabled(enabled);
    }

    private void onStartGameClicked() {
        int playerCount = lastLobbyState != null && lastLobbyState.getPlayers() != null
                ? lastLobbyState.getPlayers().length : 0;
        String message = playerCount > 1
                ? "Spiel mit " + playerCount + " Spielern starten?"
                : "Spiel starten? (Leere Plätze werden mit KI-Spielern gefüllt)";

        boolean confirmed = StyledDialog.showConfirm(this, "Spiel starten?", message);

        if (!confirmed) {
            return;
        }

        startButton.setEnabled(false);

        BoardSize bs = new BoardSize();
        bs.setRows(configBoardSize);
        bs.setCols(configBoardSize);

        try {
            System.out.println("START clicked -> sending START_GAME with " + configBonusCount + " bonuses");
            client.sendStartGame(bs, configTreasuresToWin, configBonusCount, configGameDurationMinutes * 60, configTurnTimeSeconds);
        } catch (Exception ex) {
            ex.printStackTrace();
            SwingUtilities.invokeLater(() -> {
                StyledDialog.showError(this, "Fehler", "Konnte Spiel nicht starten: " + ex.getMessage());
                updateLobby(lastLobbyState);
            });
        }
    }

    /**
     * Force-enables the start button for returning to lobby after a game.
     * This is needed because the server doesn't send LOBBY_STATE when returning mid-game.
     */
    public void forceEnableStartButton() {
        SwingUtilities.invokeLater(() -> {
            startButton.setEnabled(true);
            enableSettingsPanel(true);
            System.out.println("[MultiplayerLobbyPanel] Start button force-enabled");
        });
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


}