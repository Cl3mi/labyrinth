package labyrinth.client.ui;

import labyrinth.client.messaging.GameClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import labyrinth.client.ui.Styles.StyledButton;
import labyrinth.client.ui.Styles.StyledComboBox;
import labyrinth.client.ui.Styles.StyledContextMenu;
import labyrinth.client.ui.Styles.StyledDialog;
import labyrinth.client.ui.Styles.StyledPlayerCardRenderer;
import labyrinth.client.ui.Styles.StyledTextField;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.contracts.models.BoardSize;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
import lombok.Setter;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.geom.RoundRectangle2D;
import java.util.ArrayList;
import java.util.List;


public class MultiplayerLobbyPanel extends JPanel {

    private static final Logger log = LoggerFactory.getLogger(MultiplayerLobbyPanel.class);

    @Setter
    private GameClient client;

    @Setter
    private String localPlayerId;

    @Setter
    private String localUsername;

    private JLabel connectionLabel;
    private JLabel serverNameLabel;
    private DefaultListModel<String> playerListModel;
    private StyledButton startButton;
    private StyledButton cancelReconnectButton;
    private Image backgroundImage;

    private volatile LobbyStateEventPayload lastLobbyState;


    private int configBoardWidth = 7;
    private int configBoardHeight = 7;
    private int configTreasuresToWin = 6;
    private int configBonusCount = 0;
    private int configTurnTimeSeconds = 30;
    private int configGameDurationMinutes = 60;
    private String configUsername = "Player";

    private StyledTextField usernameField;
    private StyledComboBox<String> boardWidthCombo;
    private StyledComboBox<String> boardHeightCombo;
    private StyledComboBox<String> treasureCombo;
    private StyledComboBox<String> bonusCombo;
    private StyledComboBox<String> durationCombo;

    @Setter
    private Runnable onBackToMenu;

    private StyledButton backButton;
    private final List<Component> focusableComponents = new ArrayList<>();


    public MultiplayerLobbyPanel(String localPlayerId) {
        this.localPlayerId = localPlayerId;

        loadBackgroundImage();
        setupUI();
        setupKeyboardNavigation();

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
                log.info("[MultiplayerLobbyPanel] Loaded background: {}", imagePath);
            }
        } catch (Exception e) {
            log.error("Error loading background: {}", e.getMessage());
        }
    }

    private void setupUI() {
        setOpaque(false);
        setLayout(new BorderLayout(0, 15));
        setBorder(new EmptyBorder(20, 30, 20, 30));

        // header
        add(createHeader(), BorderLayout.NORTH);

        // center
        add(createCenterPanel(), BorderLayout.CENTER);

        // footer
        add(createFooter(), BorderLayout.SOUTH);
    }

    private JPanel createHeader() {
        JPanel header = new JPanel(new BorderLayout(20, 0));
        header.setOpaque(false);

        // back button
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

        // title
        JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setOpaque(false);


        var lobbyTitleLabel = new JLabel("Multiplayer Lobby");
        lobbyTitleLabel.setFont(FontManager.getHeadingMedium());
        lobbyTitleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        lobbyTitleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        serverNameLabel = new JLabel("");
        serverNameLabel.setFont(FontManager.getBodyLarge(Font.BOLD));
        serverNameLabel.setForeground(GameTheme.Colors.TEXT_LIGHT);
        serverNameLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        connectionLabel = new JLabel("Verbindung wird aufgebaut...");
        connectionLabel.setFont(FontManager.getBodyMedium());
        connectionLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        connectionLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        centerPanel.add(Box.createVerticalStrut(5));
        centerPanel.add(lobbyTitleLabel);
        centerPanel.add(Box.createVerticalStrut(3));
        centerPanel.add(serverNameLabel);
        centerPanel.add(Box.createVerticalStrut(5));
        centerPanel.add(connectionLabel);

        header.add(centerPanel, BorderLayout.CENTER);

        // placeholder right
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

        // Settings Card
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.4;
        gbc.weighty = 1.0;
        var settingsPanel = createSettingsCard();
        center.add(settingsPanel, gbc);

        // Player List Card
        gbc.gridx = 1;
        gbc.weightx = 0.6;
        center.add(createPlayerListCard(), gbc);

        return center;
    }

    private JPanel createSettingsCard() {
        var card = new JPanel() {
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
                g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 10));
                g2.fill(new RoundRectangle2D.Float(2, 2, getWidth() - 4, 40, 13, 13));

                g2.dispose();
                super.paintComponent(g);
            }
        };
        card.setOpaque(false);
        card.setLayout(new BorderLayout(0, 15));
        card.setBorder(new EmptyBorder(20, 25, 20, 25));

        // title
        JLabel titleLabel = new JLabel("⚙ Spiel-Einstellungen");
        titleLabel.setFont(FontManager.getBodyLarge(Font.BOLD));
        titleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        card.add(titleLabel, BorderLayout.NORTH);

        // settings
        JPanel settingsGrid = new JPanel(new GridBagLayout());
        settingsGrid.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 5, 8, 5);
        gbc.anchor = GridBagConstraints.WEST;

        // username
        gbc.gridx = 0; gbc.gridy = 0; gbc.weightx = 0.4;
        settingsGrid.add(createStyledLabel("Spielername:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.6;
        usernameField = new StyledTextField(configUsername);
        usernameField.setEditable(false);
        StyledTooltipManager.setTooltip(usernameField, "Spielername", "Dein Anzeigename im Spiel (wird im Hauptmenü festgelegt)");
        settingsGrid.add(usernameField, gbc);

        // game board width
        gbc.gridx = 0; gbc.gridy = 1; gbc.weightx = 0.4;
        settingsGrid.add(createStyledLabel("Spielfeld Breite:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.6;
        boardWidthCombo = new StyledComboBox<>();
        for (int i = 3; i <= 11; i += 2) {
            boardWidthCombo.addItem(String.valueOf(i));
        }
        boardWidthCombo.setSelectedItem("7");
        boardWidthCombo.addActionListener(e -> {
            String selected = (String) boardWidthCombo.getSelectedItem();
            if (selected != null) {
                configBoardWidth = Integer.parseInt(selected);
            }
        });
        StyledTooltipManager.setTooltip(boardWidthCombo, "Breite", "Breite des Spielfelds (nur ungerade Zahlen, 3-11)");
        settingsGrid.add(boardWidthCombo, gbc);

        // game board height
        gbc.gridx = 0; gbc.gridy = 2; gbc.weightx = 0.4;
        settingsGrid.add(createStyledLabel("Spielfeld Höhe:"), gbc);

        gbc.gridx = 1; gbc.weightx = 0.6;
        boardHeightCombo = new StyledComboBox<>();
        for (int i = 3; i <= 11; i += 2) {
            boardHeightCombo.addItem(String.valueOf(i));
        }
        boardHeightCombo.setSelectedItem("7");
        boardHeightCombo.addActionListener(e -> {
            String selected = (String) boardHeightCombo.getSelectedItem();
            if (selected != null) {
                configBoardHeight = Integer.parseInt(selected);
            }
        });
        StyledTooltipManager.setTooltip(boardHeightCombo, "Höhe", "Höhe des Spielfelds (nur ungerade Zahlen, 3-11)");
        settingsGrid.add(boardHeightCombo, gbc);

        // treasures
        gbc.gridx = 0; gbc.gridy = 3;
        settingsGrid.add(createStyledLabel("Schätze pro Spieler:"), gbc);

        gbc.gridx = 1;
        treasureCombo = new StyledComboBox<>();
        for (int i = 1; i <= 6; i++) {
            treasureCombo.addItem(String.valueOf(i));
        }
        treasureCombo.setSelectedItem(String.valueOf(configTreasuresToWin));
        treasureCombo.addActionListener(e -> {
            String selected = (String) treasureCombo.getSelectedItem();
            if (selected != null) {
                configTreasuresToWin = Integer.parseInt(selected);
            }
        });
        StyledTooltipManager.setTooltip(treasureCombo, "Schätze", "Anzahl der Schätze, die jeder Spieler sammeln muss");
        settingsGrid.add(treasureCombo, gbc);

        // bonus count
        gbc.gridx = 0; gbc.gridy = 4;
        settingsGrid.add(createStyledLabel("Bonus-Anzahl:"), gbc);

        gbc.gridx = 1;
        bonusCombo = new StyledComboBox<>();
        for (int i = 0; i <= 20; i++) {
            bonusCombo.addItem(String.valueOf(i));
        }
        bonusCombo.setSelectedItem(String.valueOf(configBonusCount));
        bonusCombo.addActionListener(e -> {
            String selected = (String) bonusCombo.getSelectedItem();
            if (selected != null) {
                configBonusCount = Integer.parseInt(selected);
            }
        });
        StyledTooltipManager.setTooltip(bonusCombo, "Bonus", "Gesamtanzahl der Boni im Spiel");
        settingsGrid.add(bonusCombo, gbc);


        // game duration
        gbc.gridx = 0; gbc.gridy = 5;
        settingsGrid.add(createStyledLabel("Spiel-Dauer:"), gbc);

        gbc.gridx = 1;

        durationCombo = new StyledComboBox<>();
        durationCombo.addItem("10 Minuten");
        durationCombo.addItem("15 Minuten");
        durationCombo.addItem("30 Minuten");
        durationCombo.addItem("45 Minuten");
        durationCombo.addItem("60 Minuten");
        durationCombo.addItem("90 Minuten");
        durationCombo.setSelectedItem("60 Minuten");
        durationCombo.addActionListener(e -> {
            String selected = (String) durationCombo.getSelectedItem();
            if (selected != null) {
                configGameDurationMinutes = Integer.parseInt(selected.split(" ")[0]);
            }
        });
        StyledTooltipManager.setTooltip(durationCombo, "Spiel-Dauer", "Maximale Gesamtdauer des Spiels");
        settingsGrid.add(durationCombo, gbc);

        // hint
        gbc.gridx = 0; gbc.gridy = 6; gbc.gridwidth = 2;
        gbc.insets = new Insets(15, 5, 5, 5);
        JLabel hintLabel = new JLabel("Nur der Admin kann Einstellungen ändern");
        hintLabel.setFont(FontManager.getBodySmall());
        hintLabel.setForeground(GameTheme.Colors.TEXT_MUTED);
        settingsGrid.add(hintLabel, gbc);

        card.add(settingsGrid, BorderLayout.CENTER);

        return card;
    }

    private JPanel createPlayerListCard() {
        var card = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                g2.setColor(GameTheme.Colors.CARD_BG);
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 15, 15));

                g2.setColor(GameTheme.Colors.CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 2, getHeight() - 2, 15, 15));

                g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 10));
                g2.fill(new RoundRectangle2D.Float(2, 2, getWidth() - 4, 40, 13, 13));

                g2.dispose();
                super.paintComponent(g);
            }
        };
        card.setOpaque(false);
        card.setLayout(new BorderLayout(0, 15));
        card.setBorder(new EmptyBorder(20, 25, 20, 25));

        // title
        JLabel titleLabel = new JLabel("👥 Spieler in der Lobby");
        titleLabel.setFont(FontManager.getBodyLarge(Font.BOLD));
        titleLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        card.add(titleLabel, BorderLayout.NORTH);

        // Player List
        playerListModel = new DefaultListModel<>();
        var playerList = new JList<>(playerListModel);
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


    private JLabel createStyledLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(FontManager.getBodyMedium());
        label.setForeground(GameTheme.Colors.TEXT_LIGHT);
        return label;
    }

    public void setConnected(boolean connected) {
        SwingUtilities.invokeLater(() -> {
            if (connected) {
                connectionLabel.setText("[OK] Verbunden mit Server");
                connectionLabel.setForeground(new Color(100, 200, 100));
            } else {
                connectionLabel.setText("[X] Nicht verbunden");
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

    public void setMultiplayerUsername(String username) {
        if (username != null && !username.isBlank()) {
            this.configUsername = username;
            if (usernameField != null) {
                usernameField.setText(username);
            }
        }
    }

    public void setServerName(String serverName) {
        SwingUtilities.invokeLater(() -> {
            if (serverNameLabel != null) {
                if (serverName != null && !serverName.isBlank()) {
                    serverNameLabel.setText("Server: " + serverName);
                } else {
                    serverNameLabel.setText("");
                }
            }
        });
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
                if (Boolean.TRUE.equals(p.getIsAiControlled())) {
                    sb.append("[AI] ");
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
        if (boardWidthCombo != null) boardWidthCombo.setEnabled(enabled);
        if (boardHeightCombo != null) boardHeightCombo.setEnabled(enabled);
        if (treasureCombo != null) treasureCombo.setEnabled(enabled);
        if (bonusCombo != null) bonusCombo.setEnabled(enabled);
        if (durationCombo != null) durationCombo.setEnabled(enabled);
    }

    private void setupKeyboardNavigation() {
        // Build list of focusable components in order
        focusableComponents.clear();
        focusableComponents.add(backButton);
        focusableComponents.add(boardWidthCombo);
        focusableComponents.add(boardHeightCombo);
        focusableComponents.add(treasureCombo);
        focusableComponents.add(bonusCombo);
        focusableComponents.add(durationCombo);
        focusableComponents.add(cancelReconnectButton);
        focusableComponents.add(startButton);

        KeyListener navListener = new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                int keyCode = e.getKeyCode();

                if (keyCode == KeyEvent.VK_ESCAPE) {
                    e.consume();
                    backButton.doClick();
                    return;
                }

                Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                int currentIndex = findFocusedIndex(focused);

                if (keyCode == KeyEvent.VK_DOWN || keyCode == KeyEvent.VK_S) {
                    e.consume();
                    focusNext(currentIndex);
                } else if (keyCode == KeyEvent.VK_UP || keyCode == KeyEvent.VK_W) {
                    e.consume();
                    focusPrev(currentIndex);
                } else if (keyCode == KeyEvent.VK_ENTER || keyCode == KeyEvent.VK_SPACE) {
                    if (focused instanceof AbstractButton button) {
                        e.consume();
                        button.doClick();
                    }
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {}

            @Override
            public void keyReleased(KeyEvent e) {}
        };

        addKeyListener(navListener);
        for (Component comp : focusableComponents) {
            if (comp != null) {
                comp.addKeyListener(navListener);
            }
        }

        setFocusable(true);
        setFocusCycleRoot(true);
        setFocusTraversalPolicy(KeyboardNavigationHelper.createFocusPolicy(focusableComponents));
    }

    private int findFocusedIndex(Component focused) {
        for (int i = 0; i < focusableComponents.size(); i++) {
            Component comp = focusableComponents.get(i);
            if (comp == focused || isDescendant(comp, focused)) {
                return i;
            }
        }
        return -1;
    }

    private void focusNext(int currentIndex) {
        int nextIndex = currentIndex < 0 ? 0 : (currentIndex + 1) % focusableComponents.size();
        // Skip invisible or disabled components
        int attempts = 0;
        while (attempts < focusableComponents.size()) {
            Component next = focusableComponents.get(nextIndex);
            if (next != null && next.isVisible() && next.isEnabled()) {
                next.requestFocusInWindow();
                return;
            }
            nextIndex = (nextIndex + 1) % focusableComponents.size();
            attempts++;
        }
    }

    private void focusPrev(int currentIndex) {
        int prevIndex = currentIndex < 0 ? focusableComponents.size() - 1 : (currentIndex - 1 + focusableComponents.size()) % focusableComponents.size();
        // Skip invisible or disabled components
        int attempts = 0;
        while (attempts < focusableComponents.size()) {
            Component prev = focusableComponents.get(prevIndex);
            if (prev != null && prev.isVisible() && prev.isEnabled()) {
                prev.requestFocusInWindow();
                return;
            }
            prevIndex = (prevIndex - 1 + focusableComponents.size()) % focusableComponents.size();
            attempts++;
        }
    }

    private boolean isDescendant(Component ancestor, Component descendant) {
        if (ancestor == null || descendant == null) return false;
        Component parent = descendant;
        while (parent != null) {
            if (parent == ancestor) return true;
            parent = parent.getParent();
        }
        return false;
    }

    @Override
    public void addNotify() {
        super.addNotify();
        SwingUtilities.invokeLater(() -> {
            if (backButton != null) {
                backButton.requestFocusInWindow();
            }
        });
    }

    private void onStartGameClicked() {
        int playerCount = lastLobbyState != null && lastLobbyState.getPlayers() != null
                ? lastLobbyState.getPlayers().length : 0;

        if (playerCount < 2) {
            StyledDialog.showMessage(this, "Zu wenig Spieler", "Es müssen mindestens zwei Spieler in der Lobby sein, um ein Spiel zu starten.");
            return;
        }

        String message = "Spiel mit " + playerCount + " Spielern starten?";
        boolean confirmed = StyledDialog.showConfirm(this, "Spiel starten?", message);

        if (!confirmed) {
            return;
        }

        startButton.setEnabled(false);

        BoardSize bs = new BoardSize();
        bs.setRows(configBoardHeight);
        bs.setCols(configBoardWidth);

        try {
            log.info("START clicked -> sending START_GAME with {}x{} board, {} bonuses", configBoardWidth, configBoardHeight, configBonusCount);
            client.sendStartGame(bs, configTreasuresToWin * playerCount, configBonusCount, configGameDurationMinutes * 60, configTurnTimeSeconds);
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
            log.info("[MultiplayerLobbyPanel] Start button force-enabled");
        });
    }


    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

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
        g2.setColor(ThemeManager.getInstance().getShadow());
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
                new Color[]{new Color(0, 0, 0, 0), ThemeEffects.withAlpha(GameTheme.Colors.SHADOW, 50), ThemeEffects.withAlpha(GameTheme.Colors.SHADOW, 130)}
        );
        g2.setPaint(vignette);
        g2.fillRect(0, 0, w, h);
    }

    private void drawDecorativeCorners(Graphics2D g2, int w, int h) {
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        Color gold = GameTheme.Colors.PRIMARY_GOLD_LIGHT;
        g2.setColor(new Color(gold.getRed(), gold.getGreen(), gold.getBlue(), 50));
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
