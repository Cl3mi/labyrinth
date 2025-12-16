package labyrinth.client.models;

import labyrinth.client.ui.BoardPanel;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.ui.LobbyPanel;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.prefs.Preferences;

public class LabyrinthApplication {

    private static final URI SERVER_URI = URI.create("ws://localhost:8080/game");
    private volatile boolean loginSent = false;
    private volatile boolean connectAckReceived = false;

    /**
     * Für mehrere lokale Instanzen:
     * Starten mit VM Option z.B.:
     *   -Dlabyrinth.profile=client1
     *   -Dlabyrinth.profile=client2
     */
    private static final String PROFILE = System.getProperty("labyrinth.profile", "default");

    // Token-Datei bleibt optional; wir löschen sie beim Beenden, weil Reconnect serverseitig nicht stabil ist.
    private static final Path TOKEN_FILE =
            Path.of(System.getProperty("user.home"), ".labyrinth", "identifier_" + PROFILE + ".token");

    private GameClient client;
    private JFrame frame;

    private JPanel mainPanel;    // CardLayout: "lobby" / "game"
    private LobbyPanel lobbyPanel;
    private BoardPanel boardPanel;

    private String username;

    /** Reconnect-Token vom Server (identifierToken) */
    private String identifierToken;

    public void start() throws Exception {

        // UI zuerst
        frame = new JFrame("Labyrinth Online (" + PROFILE + ")");
        installWindowCloseHandler();
        frame.setSize(1400, 900);
        frame.setLocationRelativeTo(null);

        mainPanel = new JPanel(new CardLayout());

        // Client erstellen
        client = new GameClient(SERVER_URI);
        registerCallbacks();

        lobbyPanel = new LobbyPanel(client, null);
        mainPanel.add(lobbyPanel, "lobby");

        frame.setContentPane(mainPanel);
        frame.setVisible(true);

        // Login-Flags pro Start resetten
        loginSent = false;
        connectAckReceived = false;

        client.setOnOpenHook(() -> {
            if (loginSent) return;
            loginSent = true;

            String token = loadToken();
            if (token != null) {
                System.out.println("[" + PROFILE + "] onOpen -> RECONNECT");
                client.sendReconnect(token);
            } else {
                username = JOptionPane.showInputDialog(frame, "Bitte Username eingeben (" + PROFILE + "):");
                if (username == null || username.isBlank()) username = "Player";
                lobbyPanel.setLocalUsername(username);

                System.out.println("[" + PROFILE + "] onOpen -> CONNECT username=" + username);
                client.sendConnect(username);
            }

            // Fallback: wenn nach 1.5s kein ACK kam, dann token löschen + normal CONNECT
            new Thread(() -> {
                try { Thread.sleep(1500); } catch (InterruptedException ignored) {}
                if (connectAckReceived) return;

                SwingUtilities.invokeLater(() -> {
                    // Wenn reconnect probiert wurde und nicht geklappt hat: Token löschen und CONNECT
                    String t = loadToken();
                    if (t != null) {
                        System.out.println("[" + PROFILE + "] No CONNECT_ACK after reconnect -> deleting token + CONNECT fallback");
                        deleteToken();
                    }

                    // Nur fallbacken, wenn wir noch keinen username haben (oder reconnect war)
                    if (username == null || username.isBlank()) {
                        username = JOptionPane.showInputDialog(frame, "Reconnect nicht möglich. Bitte Username eingeben (" + PROFILE + "):");
                        if (username == null || username.isBlank()) username = "Player";
                        lobbyPanel.setLocalUsername(username);
                    }

                    // Wichtig: NICHT nochmal senden, wenn ACK inzwischen doch kam
                    if (!connectAckReceived) {
                        System.out.println("[" + PROFILE + "] Fallback -> CONNECT username=" + username);
                        client.sendConnect(username);
                    }
                });
            }, "login-fallback-" + PROFILE).start();
        });

        client.connect();
    }

    // --------------------------------------------------------------------
    // Window close -> DISCONNECT + close socket + Token löschen
    // --------------------------------------------------------------------

    private void installWindowCloseHandler() {
        frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowClosing(java.awt.event.WindowEvent e) {
                shutdownAndExit();
            }
        });
    }

    private void shutdownAndExit() {
        try {
            if (client != null && client.isOpen()) {
                client.sendDisconnect();
                try { Thread.sleep(1000); } catch (InterruptedException ignored) {}
            }
            if (client != null) client.close();
        } finally {
            if (frame != null) frame.dispose();
            System.exit(0);
        }
    }

    // --------------------------------------------------------------------
    // Ansicht umschalten / BoardPanel aufbauen
    // --------------------------------------------------------------------

    private void switchToGameView() {
        if (lobbyPanel != null) lobbyPanel.stopMusic();

        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "game");
    }

    private void ensureBoardPanel(Board board, Player currentPlayer, List<Player> allPlayers) {
        if (boardPanel == null) {
            boardPanel = new BoardPanel(client, board, currentPlayer, allPlayers);
            mainPanel.add(boardPanel, "game");
        } else {
            boardPanel.setBoard(board);
            boardPanel.setPlayers(allPlayers);
            boardPanel.setCurrentPlayer(currentPlayer);
        }

        switchToGameView();
        frame.revalidate();
        frame.repaint();
    }

    // --------------------------------------------------------------------
    // Local player resolution (ohne serverseitige playerId im ACK)
    // --------------------------------------------------------------------

    private Player resolveLocalPlayer(List<Player> players) {
        if (players == null || players.isEmpty()) return null;

        if (username != null && !username.isBlank()) {
            for (Player p : players) {
                if (p != null && username.equals(p.getName())) {
                    return p;
                }
            }
        }

        return players.getFirst();
    }

    // --------------------------------------------------------------------
    // Callbacks für GameClient
    // --------------------------------------------------------------------

    private volatile boolean gameViewShown = false;

    private void registerCallbacks() {

        // ============================================================
        // CONNECT_ACK
        // ============================================================
        client.setOnConnectAck(ack -> {
            connectAckReceived = true;

            System.out.println("[" + PROFILE + "] CONNECT_ACK playerId=" + ack.getPlayerId()
                    + " identifierToken=" + ack.getIdentifierToken());

            // Token & PlayerId persistent speichern (optional, aber korrekt)
            ClientIdentityStore.saveToken(ack.getIdentifierToken());
            ClientIdentityStore.savePlayerId(ack.getPlayerId());

            this.identifierToken = ack.getIdentifierToken();

            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setConnected(true);

                // playerId (NICHT token!) ins LobbyPanel
                lobbyPanel.setLocalPlayerId(ack.getPlayerId());
            });
        });

        // ============================================================
        // LOBBY_STATE
        // ============================================================
        client.setOnLobbyState(lobby ->
                SwingUtilities.invokeLater(() -> lobbyPanel.updateLobby(lobby))
        );

        // ============================================================
        // GAME_STARTED  → initiale Spielansicht
        // ============================================================
        client.setOnGameStarted(started -> {
            System.out.println("[" + PROFILE + "] Received GAME_STARTED");

            Board board = BoardFactory.fromContracts(started.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(started.getPlayers());

            System.out.println("SERVER spareTile = " + (started.getBoard().getSpareTile() == null ? "null" : "present"));

            showGame(board, players);
        });

        // ============================================================
        // GAME_STATE_UPDATE → laufende Updates
        // ============================================================
        client.setOnGameStateUpdate(state -> {
            System.out.println("[" + PROFILE + "] Received GAME_STATE_UPDATE");

            Board board = BoardFactory.fromContracts(state.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(state.getPlayers());

            System.out.println("SERVER spareTile = " + (state.getBoard().getSpareTile() == null ? "null" : "present"));

            showGame(board, players);
        });

        // ============================================================
        // ERROR
        // ============================================================
        client.setOnErrorMessage(msg ->
                SwingUtilities.invokeLater(() ->
                        JOptionPane.showMessageDialog(frame, msg, "Fehler", JOptionPane.ERROR_MESSAGE)
                )
        );
    }

    private void showGame(Board board, List<Player> players) {
        if (board == null || players == null) return;

        Player currentPlayer = resolveLocalPlayer(players);

        SwingUtilities.invokeLater(() -> {
            if (!gameViewShown) {
                gameViewShown = true;
            }

            frame.setTitle("Labyrinth Online (" + PROFILE + ") - Board " +
                    board.getHeight() + "x" + board.getWidth());

            ensureBoardPanel(board, currentPlayer, players);
        });
    }

    // --------------------------------------------------------------------
    // Token persistence (profil-spezifisch)
    // --------------------------------------------------------------------

    private void saveToken(String token) {
        if (token == null || token.isBlank()) return;
        try {
            Files.createDirectories(TOKEN_FILE.getParent());
            Files.writeString(TOKEN_FILE, token.trim(), StandardCharsets.UTF_8);
            System.out.println("[" + PROFILE + "] Saved token to " + TOKEN_FILE.toAbsolutePath());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("unused")
    private String loadToken() {
        try {
            if (!Files.exists(TOKEN_FILE)) return null;
            String t = Files.readString(TOKEN_FILE, StandardCharsets.UTF_8).trim();
            System.out.println("[" + PROFILE + "] Loaded token from " + TOKEN_FILE.toAbsolutePath());
            return t.isBlank() ? null : t;
        } catch (Exception e) {
            return null;
        }
    }

    private void deleteToken() {
        try {
            Files.deleteIfExists(TOKEN_FILE);
            System.out.println("[" + PROFILE + "] Deleted token file " + TOKEN_FILE.toAbsolutePath());
        } catch (Exception ignored) {
        }
    }

    public final class ClientIdentityStore {
        private static final Preferences PREFS = Preferences.userNodeForPackage(ClientIdentityStore.class);
        private static final String KEY_TOKEN = "identifierToken";
        private static final String KEY_PLAYER_ID = "playerId";

        public static String loadToken() { return PREFS.get(KEY_TOKEN, null); }
        public static void saveToken(String token) { PREFS.put(KEY_TOKEN, token); }
        public static void clearToken() { PREFS.remove(KEY_TOKEN); }

        public static String loadPlayerId() { return PREFS.get(KEY_PLAYER_ID, null); }
        public static void savePlayerId(String playerId) { PREFS.put(KEY_PLAYER_ID, playerId); }
    }
}
