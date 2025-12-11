package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.messaging.GameClient;
import labyrinth.contracts.models.BoardSize;
import labyrinth.contracts.models.LobbyStateEventPayload;
import labyrinth.contracts.models.PlayerInfo;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * Lobby-UI für den Online-Modus.
 * Zeigt Verbindungsstatus, Spieler in der Lobby und einen "Spiel starten"-Button.
 * Design: gleicher Hintergrund & Musik wie im BoardPanel.
 */
public class LobbyPanel extends JPanel {

    private final GameClient client;
    private String localPlayerId;

    // UI-Komponenten
    private final JLabel connectionLabel;
    private final DefaultListModel<String> playerListModel;
    private final JList<String> playerList;
    private final JButton startButton;

    // Hintergrund & Musik
    private Image backgroundImage;
    private AudioPlayer backgroundMusic;

    public LobbyPanel(GameClient client, String localPlayerId) {
        this.client = client;
        this.localPlayerId = localPlayerId;

        loadBackgroundImage();
        initMusic();
        setOpaque(false); // wir malen den Hintergrund selbst

        setLayout(new BorderLayout());
        setBorder(new EmptyBorder(10, 10, 10, 10));

        // ===== Header (Statuszeile) =====
        JPanel header = new JPanel(new BorderLayout());
        header.setOpaque(false);

        connectionLabel = new JLabel("Verbindung wird aufgebaut …");
        connectionLabel.setFont(new Font("Arial", Font.BOLD, 16));
        connectionLabel.setForeground(new Color(0, 120, 0));

        header.add(connectionLabel, BorderLayout.WEST);
        add(header, BorderLayout.NORTH);

        // ===== Mitte: halbtransparente Spieler-Liste =====
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
                // halbtransparenter weißer Hintergrund
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

        startButton.addActionListener(e -> onStartGameClicked());

        footer.add(startButton);
        add(footer, BorderLayout.SOUTH);

        // Startbutton initial deaktivieren, bis Lobby-Infos vom Server kommen
        startButton.setEnabled(true);

        if (backgroundMusic != null) {
            backgroundMusic.play();
        }
    }

    public void setLocalPlayerId(String localPlayerId) {
        this.localPlayerId = localPlayerId;
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
            backgroundMusic.setVolume(0.8f);
            backgroundMusic.loop();
        } catch (Exception e) {
            System.err.println("Error starting lobby music: " + e.getMessage());
        }
    }

    /**
     * Sollte beim Wechsel ins BoardPanel aufgerufen werden.
     */
    public void stopMusic() {
        if (backgroundMusic != null) {
            backgroundMusic.stop();
        }
    }

    // --------------------------------------------------------------------------------
    // Öffentliche API für LabyrinthApplication
    // --------------------------------------------------------------------------------

    /** Wird vom Client aufgerufen, wenn die Verbindung steht / verloren geht. */
    public void setConnected(boolean connected) {
        if (connected) {
            connectionLabel.setText("Verbunden mit Server");
            connectionLabel.setForeground(new Color(0, 150, 0));
        } else {
            connectionLabel.setText("Nicht verbunden");
            connectionLabel.setForeground(new Color(170, 0, 0));
        }
    }

    /**
     * Aktualisiert die Spieler-Liste basierend auf dem LobbyState-Event.
     */
    public void updateLobby(LobbyStateEventPayload lobby) {
        playerListModel.clear();
        if (lobby == null || lobby.getPlayers() == null) {
            startButton.setEnabled(true);
            return;
        }

        PlayerInfo[] players = lobby.getPlayers();
        boolean isAdmin = false;

        for (PlayerInfo p : players) {
            StringBuilder sb = new StringBuilder();

            // Admin-Markierung
            if (Boolean.TRUE.equals(p.getIsAdmin())) {
                sb.append("★ ");
            }

            sb.append(p.getName());

            // Lokaler Spieler?
            if (p.getId() != null && p.getId().equals(localPlayerId)) {
                sb.append(" (Du)");
                // Lokal ist Admin?
                isAdmin = Boolean.TRUE.equals(p.getIsAdmin());
            }

            playerListModel.addElement(sb.toString());
        }

        // Start-Button nur aktiv, wenn der lokale Spieler Admin ist
        startButton.setEnabled(isAdmin);
    }

    // --------------------------------------------------------------------------------
    // Start-Button → StartGameCommandPayload über GameClient senden
    // --------------------------------------------------------------------------------

    private void onStartGameClicked() {
        BoardSize bs = new BoardSize();
        bs.setRows(7);
        bs.setCols(7);

        int treasuresPerPlayer = 7;
        int totalBonusCount = 0;        // z.B. keine Boni
        Integer gameDurationSeconds = null; // null = keine Zeitbegrenzung

        try {
            client.sendStartGame(bs, treasuresPerPlayer, totalBonusCount, gameDurationSeconds);
        } catch (Exception ex) {
            ex.printStackTrace();
            JOptionPane.showMessageDialog(
                    this,
                    "Konnte spiel nicht starten: " + ex.getMessage(),
                    "Fehler",
                    JOptionPane.ERROR_MESSAGE
            );
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
