package labyrinth.client.models;

import labyrinth.client.ui.BoardPanel;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.messaging.ReconnectionManager;
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

    private static final URI SERVER_URI = URI.create("ws://localhost:8081/game");
    private volatile boolean loginSent = false;
    private volatile boolean connectAckReceived = false;

    /**
     * Für mehrere lokale Instanzen:
     * Starten mit VM Option z.B.:
     *   -Dlabyrinth.profile=client1
     *   -Dlabyrinth.profile=client2
     */
    private static final String PROFILE = System.getProperty("labyrinth.profile", "default");

    // Token wird persistent gespeichert und überlebt Neustarts für automatisches Reconnect
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

    /** Reconnection manager for automatic retry logic */
    private ReconnectionManager reconnectionManager;
    private volatile boolean isShuttingDown = false;

    public void start() throws Exception {

        // UI zuerst
        frame = new JFrame("Labyrinth Online (" + PROFILE + ")");
        installWindowCloseHandler();
        frame.setSize(1400, 900);
        frame.setLocationRelativeTo(null);

        mainPanel = new JPanel(new CardLayout());

        // Client erstellen
        client = new GameClient(SERVER_URI);

        // Create reconnection manager
        reconnectionManager = new ReconnectionManager(client, this);

        registerCallbacks();

        lobbyPanel = new LobbyPanel(client, null);
        mainPanel.add(lobbyPanel, "lobby");

        frame.setContentPane(mainPanel);
        frame.setVisible(true);

        // Login-Flags pro Start resetten
        loginSent = false;
        connectAckReceived = false;

        // Register connection lost handler (auto-reconnect trigger)
        client.setOnConnectionLost(() -> {
            if (isShuttingDown) {
                System.out.println("Connection lost during shutdown - ignoring");
                return;
            }

            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setStatusText("Verbindung unterbrochen - Wiederverbinden...",
                                        new Color(170, 120, 0));
            });

            // Start automatic reconnection
            reconnectionManager.startAutoReconnect();
        });

        // Register status update handler
        client.setOnStatusUpdate(status -> {
            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setStatusText(status, new Color(0, 150, 0));
            });
        });

        client.setOnOpenHook(() -> {
            if (loginSent) return;
            loginSent = true;

            String token = ClientIdentityStore.loadToken();
            String storedUsername = ClientIdentityStore.loadUsername();

            if (token != null) {
                System.out.println("[" + PROFILE + "] onOpen -> RECONNECT with token");
                client.sendReconnect(token);
            } else {
                // Use stored username as default, or "Player" if none
                String defaultUsername = storedUsername != null ? storedUsername : "Player";
                username = JOptionPane.showInputDialog(frame,
                    "Bitte Username eingeben (" + PROFILE + "):",
                    defaultUsername);
                if (username == null || username.isBlank()) {
                    username = defaultUsername;
                }
                lobbyPanel.setLocalUsername(username);

                System.out.println("[" + PROFILE + "] onOpen -> CONNECT username=" + username);
                client.sendConnect(username);
            }

            // Fallback: wenn nach 2s kein ACK kam, dann token löschen + normal CONNECT
            new Thread(() -> {
                try { Thread.sleep(2000); } catch (InterruptedException ignored) {}
                if (connectAckReceived) return;

                SwingUtilities.invokeLater(() -> {
                    // Wenn reconnect probiert wurde und nicht geklappt hat: Token löschen und CONNECT
                    String t = ClientIdentityStore.loadToken();
                    if (t != null) {
                        System.out.println("[" + PROFILE + "] No CONNECT_ACK after reconnect -> token invalid");
                        ClientIdentityStore.clearToken(); // Clear invalid token
                    }

                    // Nur fallbacken, wenn wir noch keinen username haben (oder reconnect war)
                    if (username == null || username.isBlank()) {
                        String defaultUsername = storedUsername != null ? storedUsername : "Player";
                        username = JOptionPane.showInputDialog(frame,
                            "Reconnect nicht möglich. Bitte Username eingeben (" + PROFILE + "):",
                            defaultUsername);
                        if (username == null || username.isBlank()) {
                            username = defaultUsername;
                        }
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
        isShuttingDown = true; // Prevent reconnection attempts during shutdown

        try {
            // Cancel any ongoing reconnection attempts
            if (reconnectionManager != null) {
                reconnectionManager.cancelReconnection();
                reconnectionManager.shutdown();
            }

            // Send clean disconnect to server
            if (client != null && client.isOpen()) {
                client.disconnectCleanly(); // Uses new method with intentionalDisconnect flag
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException ignored) {}
            }

            // Close connection
            if (client != null) {
                client.close();
            }

            // NOTE: We NO LONGER delete token here - it persists across sessions
            // Token is only deleted when server explicitly rejects it (PLAYER_NOT_FOUND)

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

    private void ensureBoardPanel(Board board, Player currentPlayer, List<Player> allPlayers,
                                   java.time.OffsetDateTime gameEndTime,
                                   labyrinth.contracts.models.CurrentTurnInfo turnInfo) {
        if (boardPanel == null) {
            boardPanel = new BoardPanel(client, board, currentPlayer, allPlayers);
            mainPanel.add(boardPanel, "game");
        } else {
            boardPanel.setBoard(board);
            boardPanel.setPlayers(allPlayers);
            boardPanel.setCurrentPlayer(currentPlayer);
        }

        // Set timing information from server
        if (gameEndTime != null) {
            boardPanel.setGameEndTime(gameEndTime);
        }

        if (turnInfo != null) {
            boardPanel.setTurnEndTime(turnInfo.getTurnEndTime());
            boardPanel.setCurrentTurnState(turnInfo.getState());
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

            // Save credentials persistently
            ClientIdentityStore.saveToken(ack.getIdentifierToken());
            ClientIdentityStore.savePlayerId(ack.getPlayerId());
            ClientIdentityStore.saveUsername(username); // NEW: Save username for next session

            this.identifierToken = ack.getIdentifierToken();

            // Reset reconnection manager on successful connection
            if (reconnectionManager != null) {
                reconnectionManager.reset();
            }

            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setConnected(true);

                // playerId (NICHT token!) ins LobbyPanel
                lobbyPanel.setLocalPlayerId(ack.getPlayerId());

                // Update status to show successful connection
                lobbyPanel.setStatusText("Verbunden mit Server", new Color(0, 150, 0));
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

            // Apply turn info from server to board
            BoardFactory.applyTurnInfo(board, players, started.getCurrentTurnInfo());

            System.out.println("SERVER spareTile = " + (started.getBoard().getSpareTile() == null ? "null" : "present"));

            showGame(board, players, started.getGameEndTime(), started.getCurrentTurnInfo());
        });

        // ============================================================
        // GAME_STATE_UPDATE → laufende Updates
        // ============================================================
        client.setOnGameStateUpdate(state -> {
            System.out.println("[" + PROFILE + "] Received GAME_STATE_UPDATE");

            Board board = BoardFactory.fromContracts(state.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(state.getPlayers());

            // Apply turn info from server to board
            BoardFactory.applyTurnInfo(board, players, state.getCurrentTurnInfo());

            System.out.println("SERVER spareTile = " + (state.getBoard().getSpareTile() == null ? "null" : "present"));

            showGame(board, players, state.getGameEndTime(), state.getCurrentTurnInfo());
        });

        // ============================================================
        // ACTION_ERROR - Enhanced with token invalidation handling and toast notifications
        // ============================================================
        client.setOnErrorMessage(msg -> {
            SwingUtilities.invokeLater(() -> {
                // Check if this is a PLAYER_NOT_FOUND error (invalid token)
                if (msg != null && msg.contains("PLAYER_NOT_FOUND")) {
                    System.out.println("[" + PROFILE + "] Token rejected by server - clearing stored credentials");
                    ClientIdentityStore.clearToken();
                    ClientIdentityStore.clearUsername();

                    JOptionPane.showMessageDialog(
                        frame,
                        "Ihre Sitzung ist abgelaufen. Bitte melden Sie sich erneut an.\n\n" + msg,
                        "Sitzung abgelaufen",
                        JOptionPane.WARNING_MESSAGE
                    );

                    // Prompt for new username
                    String newUsername = JOptionPane.showInputDialog(
                        frame,
                        "Bitte Username eingeben (" + PROFILE + "):",
                        "Player"
                    );

                    if (newUsername != null && !newUsername.isBlank()) {
                        username = newUsername;
                        lobbyPanel.setLocalUsername(username);

                        if (client != null && client.isOpen()) {
                            client.sendConnect(username);
                        }
                    }
                } else {
                    // Use toast notifications if in game view, otherwise show dialog
                    if (boardPanel != null && gameViewShown) {
                        handleErrorWithToast(msg);
                    } else {
                        // In lobby - use dialog
                        JOptionPane.showMessageDialog(frame, msg, "Fehler", JOptionPane.ERROR_MESSAGE);
                    }
                }
            });
        });
    }

    /**
     * Handles error messages with structured toast notifications
     */
    private void handleErrorWithToast(String msg) {
        if (msg == null || msg.isBlank()) return;
        if (boardPanel == null) return;

        // Parse error code and message
        // Format: "ERROR_CODE: message details"
        String errorCode;
        String errorMessage;
        String errorTitle;

        int colonIndex = msg.indexOf(':');
        if (colonIndex > 0) {
            errorCode = msg.substring(0, colonIndex).trim();
            errorMessage = msg.substring(colonIndex + 1).trim();

            // Map error codes to structured codes and titles
            switch (errorCode) {
                case "NOT_YOUR_TURN" -> {
                    errorCode = "101";
                    errorTitle = "Nicht an der Reihe";
                }
                case "INVALID_PUSH" -> {
                    errorCode = "102";
                    errorTitle = "Ungültiger Einschub";
                }
                case "INVALID_MOVE" -> {
                    errorCode = "103";
                    errorTitle = "Ungültige Bewegung";
                }
                case "GAME_NOT_STARTED" -> {
                    errorCode = "104";
                    errorTitle = "Spiel nicht gestartet";
                }
                case "TIMEOUT" -> {
                    errorCode = "204";
                    errorTitle = "Zeitüberschreitung";
                }
                default -> {
                    errorCode = "999";
                    errorTitle = "Fehler";
                }
            }
        } else {
            errorCode = "999";
            errorTitle = "Fehler";
            errorMessage = msg;
        }

        boardPanel.showErrorToast(errorCode, errorTitle, errorMessage);

        // Unlock input so player can retry their action after error
        boardPanel.unlockInput();
    }

    private void showGame(Board board, List<Player> players,
                          java.time.OffsetDateTime gameEndTime,
                          labyrinth.contracts.models.CurrentTurnInfo turnInfo) {
        if (board == null || players == null) return;

        Player currentPlayer = resolveLocalPlayer(players);

        SwingUtilities.invokeLater(() -> {
            if (!gameViewShown) {
                gameViewShown = true;
            }

            frame.setTitle("Labyrinth Online (" + PROFILE + ") - Board " +
                    board.getHeight() + "x" + board.getWidth());

            ensureBoardPanel(board, currentPlayer, players, gameEndTime, turnInfo);
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

    // --------------------------------------------------------------------
    // Reconnection Helper Methods
    // --------------------------------------------------------------------

    /**
     * Called by ReconnectionManager to show manual reconnect dialog when auto-retry exhausted.
     */
    public void showManualReconnectDialog() {
        SwingUtilities.invokeLater(() -> {
            int choice = JOptionPane.showConfirmDialog(
                frame,
                "Automatische Wiederverbindung fehlgeschlagen.\n" +
                "Möchten Sie es manuell erneut versuchen?",
                "Verbindung unterbrochen",
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE
            );

            if (choice == JOptionPane.YES_OPTION) {
                // User wants to retry
                String token = ClientIdentityStore.loadToken();
                if (token != null) {
                    lobbyPanel.setStatusText("Manueller Wiederverbindungsversuch...",
                                            new Color(170, 120, 0));
                    reconnectionManager.startAutoReconnect(); // Restart auto-reconnect
                } else {
                    // No token - need fresh connection
                    username = JOptionPane.showInputDialog(
                        frame,
                        "Bitte Username eingeben (" + PROFILE + "):",
                        username != null ? username : "Player"
                    );

                    if (username != null && !username.isBlank()) {
                        lobbyPanel.setLocalUsername(username);
                        if (client.attemptConnect(username)) {
                            lobbyPanel.setStatusText("Verbindungsversuch...",
                                                    new Color(170, 120, 0));
                        }
                    }
                }
            } else {
                // User wants to exit
                shutdownAndExit();
            }
        });
    }

    /**
     * Called by ReconnectionManager to update UI with reconnection status.
     */
    public void updateReconnectionStatus(int attemptNumber, int maxAttempts, int delaySeconds) {
        SwingUtilities.invokeLater(() -> {
            String status = String.format(
                "Wiederverbinden... Versuch %d/%d (nächster Versuch in %ds)",
                attemptNumber, maxAttempts, delaySeconds
            );
            lobbyPanel.setStatusText(status, new Color(170, 120, 0));
        });
    }

    // --------------------------------------------------------------------
    // Client Identity Store (Preferences-based persistence)
    // --------------------------------------------------------------------

    public final class ClientIdentityStore {
        private static final Preferences PREFS = Preferences.userNodeForPackage(ClientIdentityStore.class);
        private static final String KEY_TOKEN = "identifierToken";
        private static final String KEY_PLAYER_ID = "playerId";
        private static final String KEY_USERNAME = "username";

        public static String loadToken() { return PREFS.get(KEY_TOKEN, null); }
        public static void saveToken(String token) {
            if (token != null && !token.isBlank()) {
                PREFS.put(KEY_TOKEN, token);
            }
        }
        public static void clearToken() { PREFS.remove(KEY_TOKEN); }

        public static String loadPlayerId() { return PREFS.get(KEY_PLAYER_ID, null); }
        public static void savePlayerId(String playerId) {
            if (playerId != null && !playerId.isBlank()) {
                PREFS.put(KEY_PLAYER_ID, playerId);
            }
        }

        public static String loadUsername() { return PREFS.get(KEY_USERNAME, null); }
        public static void saveUsername(String username) {
            if (username != null && !username.isBlank()) {
                PREFS.put(KEY_USERNAME, username);
            }
        }
        public static void clearUsername() { PREFS.remove(KEY_USERNAME); }
    }
}
