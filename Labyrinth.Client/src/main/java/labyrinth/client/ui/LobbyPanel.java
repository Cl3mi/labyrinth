package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.messaging.GameClient;
import labyrinth.contracts.models.BoardSize;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;
import lombok.Setter;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.util.Objects;

/**
 * Lobby-UI für den Online-Modus.
 * - zeigt Verbindungsstatus
 * - listet Spieler in der Lobby
 * - Start-Button nur für lokalen Admin
 *
 * Robustheit:
 * - primär: lokaler Spieler über localPlayerId
 * - fallback: lokaler Spieler über localUsername (falls PlayerId stale/noch nicht gesetzt)
 * - alle UI-Updates laufen im EDT
 */
public class LobbyPanel extends JPanel {

    private final GameClient client;

    /**
     * MUSS nach CONNECT_ACK vom Client gesetzt werden (ack.getPlayerId()).
     * Kann zu Beginn null sein.
     */
    @Setter
    private String localPlayerId;

    /**
     * Username als Fallback (nicht ideal als Identifier, aber praktisch bis localPlayerId stabil ist).
     */
    @Setter
    private String localUsername;

    // UI-Komponenten
    private final JLabel connectionLabel;
    private final DefaultListModel<String> playerListModel;
    private final JList<String> playerList;
    private final JButton startButton;
    private final JButton cancelReconnectButton;

    // Hintergrund & Musik
    private Image backgroundImage;
    private AudioPlayer backgroundMusic;

    // Letzter Lobby-State (für Retry/Restore)
    private volatile LobbyStateEventPayload lastLobbyState;

    public LobbyPanel(GameClient client, String localPlayerId) {
        this.client = Objects.requireNonNull(client, "client must not be null");
        this.localPlayerId = localPlayerId;

        loadBackgroundImage();
        initMusic();

        setOpaque(false);
        setLayout(new BorderLayout());
        setBorder(new EmptyBorder(10, 10, 10, 10));

        // ===== Header =====
        JPanel header = new JPanel(new BorderLayout());
        header.setOpaque(false);

        connectionLabel = new JLabel("Verbindung wird aufgebaut …");
        connectionLabel.setFont(new Font("Arial", Font.BOLD, 16));
        connectionLabel.setForeground(new Color(170, 120, 0));

        header.add(connectionLabel, BorderLayout.WEST);
        add(header, BorderLayout.NORTH);

        // ===== Center: Spieler-Liste mit erweiterten Cards =====
        playerListModel = new DefaultListModel<>();
        playerList = new JList<>(playerListModel);
        playerList.setFont(new Font("Arial", Font.BOLD, 14));
        playerList.setOpaque(false);
        playerList.setForeground(Color.BLACK);
        playerList.setSelectionBackground(new Color(255, 255, 255, 80));
        playerList.setSelectionForeground(Color.BLACK);
        playerList.setFixedCellHeight(80); // Taller cells for enhanced cards

        // Enhanced custom cell renderer with player cards
        playerList.setCellRenderer(new PlayerCardRenderer());

        JScrollPane scrollPane = new JScrollPane(playerList) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setColor(new Color(255, 255, 255, 180));
                g2.fillRect(0, 0, getWidth(), getHeight());
                g2.dispose();
                super.paintComponent(g);
            }
        };
        scrollPane.getViewport().setOpaque(false);
        scrollPane.setOpaque(false);
        scrollPane.setBorder(BorderFactory.createTitledBorder("Spieler in der Lobby"));

        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.setOpaque(false);

        // Add game settings preview panel
        JPanel settingsPreview = createSettingsPreviewPanel();
        centerPanel.add(settingsPreview, BorderLayout.NORTH);
        centerPanel.add(scrollPane, BorderLayout.CENTER);

        add(centerPanel, BorderLayout.CENTER);

        // ===== Footer: Start-Button + Cancel Reconnect =====
        JPanel footer = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        footer.setOpaque(false);

        startButton = new JButton("Spiel starten");
        startButton.setFont(new Font("Arial", Font.BOLD, 14));
        startButton.setEnabled(false); // initial deaktiviert
        startButton.addActionListener(e -> onStartGameClicked());

        cancelReconnectButton = new JButton("Wiederverbindung abbrechen");
        cancelReconnectButton.setFont(new Font("Arial", Font.BOLD, 12));
        cancelReconnectButton.setVisible(false); // Hidden by default
        cancelReconnectButton.addActionListener(e -> onCancelReconnect());

        footer.add(cancelReconnectButton);
        footer.add(startButton);
        add(footer, BorderLayout.SOUTH);

        if (backgroundMusic != null) {
            backgroundMusic.play();
        }
    }

    // --------------------------------------------------------------------------------
    // Hintergrund & Musik
    // --------------------------------------------------------------------------------

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

    private void initMusic() {
        try {
            backgroundMusic = new AudioPlayer("/sounds/06-Kokiri-Forest.wav");
            backgroundMusic.setVolume(0.0f);
            backgroundMusic.loop();
        } catch (Exception e) {
            System.err.println("Error starting lobby music: " + e.getMessage());
        }
    }

    public void stopMusic() {
        if (backgroundMusic != null) {
            backgroundMusic.stop();
        }
    }

    // --------------------------------------------------------------------------------
    // Public API
    // --------------------------------------------------------------------------------

    public void setConnected(boolean connected) {
        SwingUtilities.invokeLater(() -> {
            if (connected) {
                connectionLabel.setText("Verbunden mit Server");
                connectionLabel.setForeground(new Color(0, 150, 0));
            } else {
                connectionLabel.setText("Nicht verbunden");
                connectionLabel.setForeground(new Color(170, 0, 0));
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

    /**
     * Shows/hides the cancel reconnect button during reconnection attempts.
     */
    public void setReconnecting(boolean reconnecting) {
        SwingUtilities.invokeLater(() -> {
            cancelReconnectButton.setVisible(reconnecting);
            startButton.setVisible(!reconnecting);
        });
    }

    /**
     * Called when user clicks cancel reconnect button.
     */
    private void onCancelReconnect() {
        int choice = JOptionPane.showConfirmDialog(
            this,
            "Wiederverbindung abbrechen und Anwendung beenden?",
            "Abbrechen bestätigen",
            JOptionPane.YES_NO_OPTION
        );

        if (choice == JOptionPane.YES_OPTION) {
            // Signal to exit application
            System.exit(0);
        }
    }

    /**
     * Aktualisiert die Spieler-Liste basierend auf dem LobbyState-Event.
     *
     * Matching-Strategie:
     * 1) Primär: localPlayerId == p.id
     * 2) Fallback: localUsername == p.name (nur wenn lokaler Spieler noch nicht gefunden wurde)
     *
     * Das behebt deinen aktuellen Zustand (stale localPlayerId), ohne dass du UI-seitig blockierst.
     */
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

            // Optionales Debug: (kannst du später entfernen)
            System.out.println("[LobbyPanel] localPlayerId=" + localPlayerId + " localUsername=" + localUsername);

            for (PlayerInfo p : players) {
                if (p == null) continue;

                // Optionales Debug: (kannst du später entfernen)
                System.out.println("[LobbyPanel] p.id=" + p.getId() + " p.name=" + p.getName() + " p.isAdmin=" + p.getIsAdmin());

                StringBuilder sb = new StringBuilder();

                // Show disconnected status
                if (Boolean.FALSE.equals(p.getIsConnected())) {
                    sb.append("[OFFLINE] ");
                }

                if (Boolean.TRUE.equals(p.getIsAdmin())) {
                    sb.append("(Admin) ");
                }

                String name = p.getName() != null ? p.getName() : "<unbekannt>";
                sb.append(name);

                boolean idMatch = localPlayerId != null
                        && p.getId() != null
                        && localPlayerId.equals(p.getId());

                boolean nameMatch = localUsername != null
                        && p.getName() != null
                        && localUsername.equals(p.getName());

                // Primär über ID; Fallback über Name, aber nur solange noch nicht gefunden
                if (idMatch || (!localFound && nameMatch)) {
                    sb.append(" (Du)");
                    localFound = true;
                    localIsAdmin = Boolean.TRUE.equals(p.getIsAdmin());
                }

                playerListModel.addElement(sb.toString());
            }

            startButton.setEnabled(localFound && localIsAdmin);
        });
    }

    // --------------------------------------------------------------------------------
    // StartGame
    // --------------------------------------------------------------------------------

    private void onStartGameClicked() {
        // Schutz gegen Double-Click
        startButton.setEnabled(false);

        BoardSize bs = new BoardSize();
        bs.setRows(7);
        bs.setCols(7);

        int treasuresPerPlayer = 12;
        int totalBonusCount = 0;
        Integer gameDurationSeconds = 3600;

        try {
            System.out.println("START clicked -> sending START_GAME");
            System.out.println("rows=" + bs.getRows() + " cols=" + bs.getCols()
                    + " treasureCardCount=" + treasuresPerPlayer
                    + " totalBonusCount=" + totalBonusCount
                    + " gameDurationSeconds=" + gameDurationSeconds);

            client.sendStartGame(bs, treasuresPerPlayer, totalBonusCount, gameDurationSeconds);
        } catch (Exception ex) {
            ex.printStackTrace();

            SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(
                    this,
                    "Konnte Spiel nicht starten: " + ex.getMessage(),
                    "Fehler",
                    JOptionPane.ERROR_MESSAGE
            ));

            // Button-Status anhand letzter Lobby wiederherstellen
            updateLobby(lastLobbyState);
        }
    }

    // --------------------------------------------------------------------------------
    // Hintergrund zeichnen
    // --------------------------------------------------------------------------------

    @Override
    protected void paintComponent(Graphics g) {
        if (backgroundImage != null) {
            g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
        } else {
            super.paintComponent(g);
        }
    }

    // --------------------------------------------------------------------------------
    // Settings Preview Panel
    // --------------------------------------------------------------------------------

    private JPanel createSettingsPreviewPanel() {
        JPanel panel = new JPanel();
        panel.setOpaque(false);
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        JLabel titleLabel = new JLabel("Spiel-Einstellungen");
        titleLabel.setFont(new Font("Arial", Font.BOLD, 14));
        titleLabel.setForeground(new Color(220, 220, 255));
        titleLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel settingsGrid = new JPanel(new GridLayout(2, 2, 10, 5));
        settingsGrid.setOpaque(false);
        settingsGrid.setMaximumSize(new Dimension(400, 60));
        settingsGrid.setAlignmentX(Component.LEFT_ALIGNMENT);

        addSettingLabel(settingsGrid, "Spielfeld:", "7x7");
        addSettingLabel(settingsGrid, "Schätze:", "12 pro Spieler");
        addSettingLabel(settingsGrid, "Spielzeit:", "60 Minuten");
        addSettingLabel(settingsGrid, "Bonus:", "Keine");

        panel.add(titleLabel);
        panel.add(Box.createVerticalStrut(8));
        panel.add(settingsGrid);
        panel.add(Box.createVerticalStrut(10));

        return panel;
    }

    private void addSettingLabel(JPanel panel, String label, String value) {
        JLabel labelComp = new JLabel(label);
        labelComp.setFont(new Font("Arial", Font.PLAIN, 12));
        labelComp.setForeground(new Color(180, 180, 200));

        JLabel valueComp = new JLabel(value);
        valueComp.setFont(new Font("Arial", Font.BOLD, 12));
        valueComp.setForeground(new Color(220, 220, 255));

        panel.add(labelComp);
        panel.add(valueComp);
    }

    // --------------------------------------------------------------------------------
    // Enhanced Player Card Renderer
    // --------------------------------------------------------------------------------

    private class PlayerCardRenderer extends JPanel implements ListCellRenderer<String> {
        private final JLabel nameLabel;
        private final JLabel statusLabel;
        private final JLabel badgeLabel;
        private final JPanel iconPanel;

        private static final Color[] PLAYER_COLORS = {
            new Color(220, 80, 80),
            new Color(80, 180, 80),
            new Color(80, 140, 220),
            new Color(230, 200, 80)
        };

        public PlayerCardRenderer() {
            setLayout(new BorderLayout(10, 5));
            setBorder(BorderFactory.createEmptyBorder(8, 12, 8, 12));

            // Left: Color indicator / icon
            iconPanel = new JPanel() {
                @Override
                protected void paintComponent(Graphics g) {
                    super.paintComponent(g);
                    Graphics2D g2 = (Graphics2D) g.create();
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                    // Draw colored circle
                    g2.setColor(getBackground());
                    g2.fillOval(5, 5, 50, 50);

                    // Draw border
                    g2.setColor(Color.WHITE);
                    g2.setStroke(new BasicStroke(2));
                    g2.drawOval(5, 5, 50, 50);

                    g2.dispose();
                }
            };
            iconPanel.setPreferredSize(new Dimension(60, 60));
            iconPanel.setOpaque(false);

            // Center: Name and status
            JPanel centerPanel = new JPanel();
            centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
            centerPanel.setOpaque(false);

            nameLabel = new JLabel();
            nameLabel.setFont(new Font("Arial", Font.BOLD, 16));
            nameLabel.setForeground(Color.WHITE);
            nameLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

            statusLabel = new JLabel();
            statusLabel.setFont(new Font("Arial", Font.PLAIN, 12));
            statusLabel.setForeground(new Color(200, 200, 220));
            statusLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

            centerPanel.add(nameLabel);
            centerPanel.add(Box.createVerticalStrut(4));
            centerPanel.add(statusLabel);

            // Right: Badges (admin, you, offline)
            badgeLabel = new JLabel();
            badgeLabel.setFont(new Font("Arial", Font.BOLD, 12));
            badgeLabel.setHorizontalAlignment(SwingConstants.RIGHT);
            badgeLabel.setVerticalAlignment(SwingConstants.TOP);

            add(iconPanel, BorderLayout.WEST);
            add(centerPanel, BorderLayout.CENTER);
            add(badgeLabel, BorderLayout.EAST);
        }

        @Override
        public Component getListCellRendererComponent(JList<? extends String> list,
                                                      String value,
                                                      int index,
                                                      boolean isSelected,
                                                      boolean cellHasFocus) {
            // Parse player info from string
            boolean isOffline = value != null && value.contains("[OFFLINE]");
            boolean isAdmin = value != null && value.contains("(Admin)");
            boolean isYou = value != null && value.contains("(Du)");

            // Extract clean name
            String cleanName = value;
            if (cleanName != null) {
                cleanName = cleanName.replace("[OFFLINE] ", "")
                                    .replace("(Admin) ", "")
                                    .replace(" (Du)", "")
                                    .trim();
            }

            // Set background
            if (isSelected) {
                setBackground(new Color(100, 120, 160, 200));
            } else {
                setBackground(new Color(50, 60, 80, 180));
            }

            // Set player color icon
            Color playerColor = PLAYER_COLORS[index % PLAYER_COLORS.length];
            if (isOffline) {
                playerColor = new Color(100, 100, 100);
            }
            iconPanel.setBackground(playerColor);

            // Set name
            nameLabel.setText(cleanName);
            if (isOffline) {
                nameLabel.setForeground(new Color(150, 150, 150));
            } else {
                nameLabel.setForeground(Color.WHITE);
            }

            // Set status text
            if (isOffline) {
                statusLabel.setText("Nicht verbunden");
                statusLabel.setForeground(new Color(200, 100, 100));
            } else {
                statusLabel.setText("Bereit");
                statusLabel.setForeground(new Color(100, 200, 100));
            }

            // Set badges
            StringBuilder badges = new StringBuilder("<html>");
            if (isAdmin) {
                badges.append("<span style='color: #FFD700;'>★ Admin</span><br>");
            }
            if (isYou) {
                badges.append("<span style='color: #90EE90;'>● Du</span>");
            }
            badges.append("</html>");
            badgeLabel.setText(badges.toString());

            return this;
        }
    }
}
