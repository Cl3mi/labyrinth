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

        // ===== Center: Spieler-Liste =====
        playerListModel = new DefaultListModel<>();
        playerList = new JList<>(playerListModel);
        playerList.setFont(new Font("Arial", Font.BOLD, 14));
        playerList.setOpaque(false);
        playerList.setForeground(Color.BLACK);
        playerList.setSelectionBackground(new Color(255, 255, 255, 80));
        playerList.setSelectionForeground(Color.BLACK);

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
        centerPanel.add(scrollPane, BorderLayout.CENTER);
        add(centerPanel, BorderLayout.CENTER);

        // ===== Footer: Start-Button =====
        JPanel footer = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        footer.setOpaque(false);

        startButton = new JButton("Spiel starten");
        startButton.setFont(new Font("Arial", Font.BOLD, 14));
        startButton.setEnabled(false); // initial deaktiviert
        startButton.addActionListener(e -> onStartGameClicked());

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
}
