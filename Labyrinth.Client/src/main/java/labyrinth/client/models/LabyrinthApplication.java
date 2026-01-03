package labyrinth.client.models;

import labyrinth.client.ui.BoardPanel;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.messaging.ReconnectionManager;
import labyrinth.client.ui.GameOverPanel;
import labyrinth.client.ui.MainMenuPanel;
import labyrinth.client.ui.MultiplayerLobbyPanel;
import labyrinth.client.ui.OptionsPanel;

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

    private static final String PROFILE = System.getProperty("labyrinth.profile", "default");

    private static final Path TOKEN_FILE =
            Path.of(System.getProperty("user.home"), ".labyrinth", "identifier_" + PROFILE + ".token");

    private GameClient client;
    private JFrame frame;

    private JPanel mainPanel;
    private MainMenuPanel mainMenuPanel;
    private MultiplayerLobbyPanel lobbyPanel;
    private OptionsPanel optionsPanel;
    private BoardPanel boardPanel;
    private GameOverPanel gameOverPanel;

    private String username;
    private String identifierToken;

    private ReconnectionManager reconnectionManager;
    private volatile boolean isShuttingDown = false;
    private volatile boolean gameViewShown = false;
    private volatile boolean pendingSingleplayerStart = false;

    public void start() throws Exception {
        frame = new JFrame("Labyrinth Online (" + PROFILE + ")");
        installWindowCloseHandler();

        // Lade gespeicherte Fenstergröße aus Einstellungen
        int[] windowSize = OptionsPanel.loadWindowSizeFromPreferences();
        if (windowSize[0] == -1 || windowSize[1] == -1) {
            // Maximiert
            frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        } else {
            frame.setSize(windowSize[0], windowSize[1]);
        }
        frame.setLocationRelativeTo(null);

        mainPanel = new JPanel(new CardLayout());

        client = new GameClient(SERVER_URI);
        reconnectionManager = new ReconnectionManager(client, this);

        registerCallbacks();

        // Hauptmenü erstellen
        mainMenuPanel = new MainMenuPanel();
        mainMenuPanel.setOnSingleplayerClicked(this::startSingleplayerGame);
        mainMenuPanel.setOnMultiplayerClicked(this::showMultiplayerLobby);
        mainMenuPanel.setOnOptionsClicked(this::showOptions);
        mainMenuPanel.setOnExitClicked(this::shutdownAndExit);
        mainPanel.add(mainMenuPanel, "mainmenu");

        // Multiplayer-Lobby erstellen
        lobbyPanel = new MultiplayerLobbyPanel(client, null);
        lobbyPanel.setOnBackToMenu(this::showMainMenu);
        mainPanel.add(lobbyPanel, "lobby");

        // Options Panel erstellen
        optionsPanel = new OptionsPanel();
        optionsPanel.setOnBackToMenu(this::showMainMenu);
        optionsPanel.setOnSettingsChanged(this::applySettings);
        optionsPanel.setOnMusicVolumeChanged(volume -> {
            // Musik-Lautstärke in Echtzeit ändern
            if (mainMenuPanel != null) {
                mainMenuPanel.setMusicVolume(volume);
            }
        });
        optionsPanel.setOnWindowSizeChanged(size -> {
            // Fenstergröße sofort ändern
            if (frame != null) {
                if (size[0] == -1 || size[1] == -1) {
                    // Maximiert
                    frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
                } else {
                    // Normale Größe - erst aus Maximiert-Modus holen falls nötig
                    frame.setExtendedState(JFrame.NORMAL);
                    frame.setSize(size[0], size[1]);
                    frame.setLocationRelativeTo(null);
                }
            }
        });
        mainPanel.add(optionsPanel, "options");

        // Game Over Panel
        gameOverPanel = new GameOverPanel(this::showMainMenu);
        mainPanel.add(gameOverPanel, "gameover");

        frame.setContentPane(mainPanel);
        frame.setVisible(true);

        loginSent = false;
        connectAckReceived = false;

        client.setOnConnectionLost(() -> {
            if (isShuttingDown) {
                System.out.println("Connection lost during shutdown - ignoring");
                return;
            }
            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setStatusText("Verbindung unterbrochen - Wiederverbinden...",
                        new Color(170, 120, 0));
            });
            reconnectionManager.startAutoReconnect();
        });

        client.setOnAchievementUnlocked(achievement -> {
            System.out.println("[" + PROFILE + "] Achievement unlocked: "
                    + achievement.getAchievement() + " for player " + achievement.getPlayerId());
            SwingUtilities.invokeLater(() -> {
                if (boardPanel != null) {
                    String achievementName = achievement.getAchievement() != null
                            ? achievement.getAchievement().toString() : "UNKNOWN";
                    String displayName = formatAchievementName(achievementName);
                    boardPanel.showSuccessToast("ACHIEVEMENT", "🏆 Erfolg freigeschaltet!", displayName);
                }
            });
        });

        client.setOnNextTreasure(nextTreasure -> {
            System.out.println("[" + PROFILE + "] Next treasure: " + nextTreasure.getTreasure());
            SwingUtilities.invokeLater(() -> {
                if (boardPanel != null && nextTreasure.getTreasure() != null) {
                    String treasureName = nextTreasure.getTreasure().getName();
                    boardPanel.showInfoToast("NEXT_TREASURE", "🎯 Neues Ziel!", "Finde: " + treasureName);
                }
            });
        });

        client.setOnStatusUpdate(status -> {
            SwingUtilities.invokeLater(() -> lobbyPanel.setStatusText(status, new Color(0, 150, 0)));
        });

        setupConnectionHook();
        showMainMenu();
    }

    private void showMainMenu() {
        if (gameOverPanel != null) gameOverPanel.cleanup();
        gameViewShown = false;
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "mainmenu");
    }

    private void showOptions() {
        // Musik weiterlaufen lassen, damit man die Lautstärke testen kann
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "options");
    }

    private void applySettings() {
        // Hier können die Einstellungen angewendet werden
        // z.B. Musik-Lautstärke aktualisieren
        System.out.println("Settings applied:");
        System.out.println("  Music Volume: " + optionsPanel.getMusicVolume() + "%");
        System.out.println("  SFX Volume: " + optionsPanel.getSfxVolume() + "%");
        System.out.println("  Server URL: " + optionsPanel.getServerUrl());
        System.out.println("  Dark Theme: " + optionsPanel.isDarkTheme());
    }

    private void showMultiplayerLobby() {
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "lobby");
        connectToServer();
    }

    private void startSingleplayerGame() {

        // Verbinde zum Server für Singleplayer
        loginSent = false;
        connectAckReceived = false;

        // Setze Flag für Singleplayer-Modus
        pendingSingleplayerStart = true;

        try {
            client.connect();
        } catch (Exception e) {
            e.printStackTrace();
            pendingSingleplayerStart = false;
            SwingUtilities.invokeLater(() -> {
                JOptionPane.showMessageDialog(frame,
                        "Konnte nicht zum Server verbinden: " + e.getMessage(),
                        "Verbindungsfehler", JOptionPane.ERROR_MESSAGE);
            });
        }
    }

    private void sendSingleplayerStartGame() {
        labyrinth.contracts.models.BoardSize bs = new labyrinth.contracts.models.BoardSize();
        bs.setRows(mainMenuPanel.getSingleplayerBoardSize());
        bs.setCols(mainMenuPanel.getSingleplayerBoardSize());

        int treasuresToWin = mainMenuPanel.getSingleplayerTreasures();
        int totalBonusCount = 0;
        int gameDurationSeconds = mainMenuPanel.getSingleplayerGameDuration() * 60;
        int turnTimeSeconds = mainMenuPanel.getSingleplayerTurnTime();

        try {
            System.out.println("SINGLEPLAYER -> sending START_GAME");
            System.out.println("rows=" + bs.getRows() + " cols=" + bs.getCols()
                    + " treasures=" + treasuresToWin
                    + " duration=" + gameDurationSeconds + "s"
                    + " turnTime=" + turnTimeSeconds + "s");
            client.sendStartGame(bs, treasuresToWin, totalBonusCount, gameDurationSeconds, turnTimeSeconds);
        } catch (Exception ex) {
            ex.printStackTrace();
            SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(frame,
                    "Konnte Spiel nicht starten: " + ex.getMessage(),
                    "Fehler", JOptionPane.ERROR_MESSAGE));
        }
    }

    private void connectToServer() {
        loginSent = false;
        connectAckReceived = false;
        lobbyPanel.setStatusText("Verbinde zum Server...", new Color(170, 120, 0));
        try {
            client.connect();
        } catch (Exception e) {
            e.printStackTrace();
            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setStatusText("Verbindung fehlgeschlagen", new Color(170, 0, 0));
                JOptionPane.showMessageDialog(frame,
                        "Konnte nicht zum Server verbinden: " + e.getMessage(),
                        "Verbindungsfehler", JOptionPane.ERROR_MESSAGE);
            });
        }
    }

    private void setupConnectionHook() {
        client.setOnOpenHook(() -> {
            if (loginSent) return;
            loginSent = true;

            String token = ClientIdentityStore.loadToken();
            String storedUsername = ClientIdentityStore.loadUsername();

            if (token != null) {
                System.out.println("[" + PROFILE + "] onOpen -> RECONNECT with token");
                client.sendReconnect(token);
            } else {
                // Bei Singleplayer keinen Dialog zeigen, automatisch Username verwenden
                if (pendingSingleplayerStart) {
                    username = storedUsername != null ? storedUsername : "Player";
                    System.out.println("[" + PROFILE + "] onOpen -> SINGLEPLAYER CONNECT username=" + username);
                    client.sendConnect(username);
                } else {
                    String defaultUsername = storedUsername != null ? storedUsername : "Player";
                    username = JOptionPane.showInputDialog(frame,
                            "Bitte Username eingeben (" + PROFILE + "):", defaultUsername);
                    if (username == null || username.isBlank()) username = defaultUsername;
                    lobbyPanel.setLocalUsername(username);
                    System.out.println("[" + PROFILE + "] onOpen -> CONNECT username=" + username);
                    client.sendConnect(username);
                }
            }

            final String finalStoredUsername = storedUsername;
            new Thread(() -> {
                try { Thread.sleep(2000); } catch (InterruptedException ignored) {}
                if (connectAckReceived) return;

                SwingUtilities.invokeLater(() -> {
                    String t = ClientIdentityStore.loadToken();
                    if (t != null) {
                        System.out.println("[" + PROFILE + "] No CONNECT_ACK after reconnect -> token invalid");
                        ClientIdentityStore.clearToken();
                    }
                    if (username == null || username.isBlank()) {
                        // Bei Singleplayer keinen Dialog
                        if (pendingSingleplayerStart) {
                            username = finalStoredUsername != null ? finalStoredUsername : "Player";
                        } else {
                            String defaultUsername = finalStoredUsername != null ? finalStoredUsername : "Player";
                            username = JOptionPane.showInputDialog(frame,
                                    "Reconnect nicht möglich. Bitte Username eingeben (" + PROFILE + "):", defaultUsername);
                            if (username == null || username.isBlank()) username = defaultUsername;
                            lobbyPanel.setLocalUsername(username);
                        }
                    }
                    if (!connectAckReceived) {
                        System.out.println("[" + PROFILE + "] Fallback -> CONNECT username=" + username);
                        client.sendConnect(username);
                    }
                });
            }, "login-fallback-" + PROFILE).start();
        });
    }

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
        isShuttingDown = true;
        try {
            if (reconnectionManager != null) {
                reconnectionManager.cancelReconnection();
                reconnectionManager.shutdown();
            }
            if (client != null && client.isOpen()) {
                client.disconnectCleanly();
                try { Thread.sleep(1000); } catch (InterruptedException ignored) {}
            }
            if (client != null) client.close();
        } finally {
            if (frame != null) frame.dispose();
            System.exit(0);
        }
    }

    private void switchToGameView() {
        // Music is already stopped in ensureBoardPanel before BoardPanel starts its music
        if (gameOverPanel != null) gameOverPanel.cleanup();
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "game");
    }

    private void switchToGameOverView() {
        if (mainMenuPanel != null) mainMenuPanel.stopMusic();
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "gameover");
    }

    private void switchToLobbyView() {
        if (gameOverPanel != null) gameOverPanel.cleanup();
        gameViewShown = false;
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "lobby");
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
        if (gameEndTime != null) boardPanel.setGameEndTime(gameEndTime);
        if (turnInfo != null) {
            boardPanel.setTurnEndTime(turnInfo.getTurnEndTime());
            boardPanel.setCurrentTurnState(turnInfo.getState());
        }
        switchToGameView();
        frame.revalidate();
        frame.repaint();
    }

    private void exitGameToMainMenu() {
        // Disconnect vom Server senden
        if (client != null && client.isOpen()) {
            try {
                client.disconnectCleanly();
            } catch (Exception e) {
                System.err.println("Error disconnecting: " + e.getMessage());
            }
        }

        // Zum Hauptmenü zurückkehren
        showMainMenu();
    }

    private Player resolveLocalPlayer(List<Player> players) {
        if (players == null || players.isEmpty()) return null;
        if (username != null && !username.isBlank()) {
            for (Player p : players) {
                if (p != null && username.equals(p.getName())) return p;
            }
        }
        return players.getFirst();
    }

    private void registerCallbacks() {
        client.setOnConnectAck(ack -> {
            connectAckReceived = true;
            System.out.println("[" + PROFILE + "] CONNECT_ACK playerId=" + ack.getPlayerId()
                    + " identifierToken=" + ack.getIdentifierToken());
            ClientIdentityStore.saveToken(ack.getIdentifierToken());
            ClientIdentityStore.savePlayerId(ack.getPlayerId());
            ClientIdentityStore.saveUsername(username);
            this.identifierToken = ack.getIdentifierToken();
            if (reconnectionManager != null) reconnectionManager.reset();

            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setConnected(true);
                lobbyPanel.setLocalPlayerId(ack.getPlayerId());
                lobbyPanel.setStatusText("Verbunden mit Server", new Color(0, 150, 0));

                // Wenn Singleplayer gestartet wurde, direkt Spiel starten
                if (pendingSingleplayerStart) {
                    pendingSingleplayerStart = false;
                    sendSingleplayerStartGame();
                }
            });
        });

        client.setOnLobbyState(lobby ->
                SwingUtilities.invokeLater(() -> lobbyPanel.updateLobby(lobby)));

        client.setOnGameStarted(started -> {
            System.out.println("[" + PROFILE + "] Received GAME_STARTED");
            Board board = BoardFactory.fromContracts(started.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(started.getPlayers());
            board.setPlayers(players);
            System.out.println(board);
            BoardFactory.applyTurnInfo(board, players, started.getCurrentTurnInfo());
            showGame(board, started.getGameEndTime(), started.getCurrentTurnInfo());
        });

        client.setOnGameStateUpdate(state -> {
            System.out.println("[" + PROFILE + "] Received GAME_STATE_UPDATE");
            Board board = BoardFactory.fromContracts(state.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(state.getPlayers());
            board.setPlayers(players);
            System.out.println(board);
            BoardFactory.applyTurnInfo(board, players, state.getCurrentTurnInfo());
            showGame(board, state.getGameEndTime(), state.getCurrentTurnInfo());
        });

        client.setOnGameOver(gameOver -> {
            System.out.println("[" + PROFILE + "] Received GAME_OVER");
            SwingUtilities.invokeLater(() -> {
                gameOverPanel.updateGameOver(gameOver);
                switchToGameOverView();
            });
        });

        client.setOnErrorMessage(msg -> {
            SwingUtilities.invokeLater(() -> {
                if (msg != null && msg.contains("PLAYER_NOT_FOUND")) {
                    System.out.println("[" + PROFILE + "] Token rejected by server - clearing stored credentials");
                    ClientIdentityStore.clearToken();
                    ClientIdentityStore.clearUsername();
                    JOptionPane.showMessageDialog(frame,
                            "Ihre Sitzung ist abgelaufen. Bitte melden Sie sich erneut an.\n\n" + msg,
                            "Sitzung abgelaufen", JOptionPane.WARNING_MESSAGE);
                    String newUsername = JOptionPane.showInputDialog(frame,
                            "Bitte Username eingeben (" + PROFILE + "):", "Player");
                    if (newUsername != null && !newUsername.isBlank()) {
                        username = newUsername;
                        lobbyPanel.setLocalUsername(username);
                        if (client != null && client.isOpen()) client.sendConnect(username);
                    }
                } else {
                    if (boardPanel != null && gameViewShown) {
                        handleErrorWithToast(msg);
                    } else {
                        JOptionPane.showMessageDialog(frame, msg, "Fehler", JOptionPane.ERROR_MESSAGE);
                    }
                }
            });
        });
    }

    private void handleErrorWithToast(String msg) {
        if (msg == null || msg.isBlank() || boardPanel == null) return;
        String errorCode, errorMessage, errorTitle;
        int colonIndex = msg.indexOf(':');
        if (colonIndex > 0) {
            errorCode = msg.substring(0, colonIndex).trim();
            errorMessage = msg.substring(colonIndex + 1).trim();
            switch (errorCode) {
                // Game action errors (100-199)
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

                // Permission errors (200-299)
                case "NOT_ADMIN" -> {
                    errorCode = "201";
                    errorTitle = "Keine Berechtigung";
                }

                // Bonus errors (300-399)
                case "BONUS_NOT_AVAILABLE" -> {
                    errorCode = "301";
                    errorTitle = "Bonus nicht verfügbar";
                }

                // Lobby/Connection errors (400-499)
                case "LOBBY_FULL" -> {
                    errorCode = "401";
                    errorTitle = "Lobby voll";
                }
                case "GAME_ALREADY_STARTED" -> {
                    errorCode = "402";
                    errorTitle = "Spiel bereits gestartet";
                }
                case "USERNAME_TAKEN" -> {
                    errorCode = "403";
                    errorTitle = "Username bereits vergeben";
                }
                case "PLAYER_NOT_CONNECTED" -> {
                    errorCode = "404";
                    errorTitle = "Spieler nicht verbunden";
                }

                // Command errors (500-599)
                case "INVALID_COMMAND" -> {
                    errorCode = "501";
                    errorTitle = "Ungültiger Befehl";
                }

                // General errors (999)
                case "GENERAL" -> {
                    errorCode = "999";
                    errorTitle = "Allgemeiner Fehler";
                }
                default -> {
                    errorCode = "999";
                    errorTitle = "Unbekannter Fehler";
                }
            }
        } else {
            errorCode = "999"; errorTitle = "Fehler"; errorMessage = msg;
        }
        boardPanel.showErrorToast(errorCode, errorTitle, errorMessage);
        boardPanel.unlockInput();
    }

    private String formatAchievementName(String achievementName) {
        if (achievementName == null) return "Unbekannter Erfolg";
        String formatted = achievementName.replace("_", " ").toLowerCase();
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : formatted.toCharArray()) {
            if (Character.isWhitespace(c)) {
                capitalizeNext = true;
                result.append(c);
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(c);
            }
        }
        return result.toString();
    }

    private void showGame(Board board, java.time.OffsetDateTime gameEndTime,
                          labyrinth.contracts.models.CurrentTurnInfo turnInfo) {
        if (board == null || board.getPlayers() == null) return;
        Player currentPlayer = resolveLocalPlayer(board.getPlayers());
        SwingUtilities.invokeLater(() -> {
            if (!gameViewShown) gameViewShown = true;
            frame.setTitle("Labyrinth Online (" + PROFILE + ") - Board " +
                    board.getHeight() + "x" + board.getWidth());
            ensureBoardPanel(board, currentPlayer, board.getPlayers(), gameEndTime, turnInfo);
        });
    }

    public void showManualReconnectDialog() {
        SwingUtilities.invokeLater(() -> {
            int choice = JOptionPane.showConfirmDialog(frame,
                    "Automatische Wiederverbindung fehlgeschlagen.\nMöchten Sie es manuell erneut versuchen?",
                    "Verbindung unterbrochen", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
            if (choice == JOptionPane.YES_OPTION) {
                String token = ClientIdentityStore.loadToken();
                if (token != null) {
                    lobbyPanel.setStatusText("Manueller Wiederverbindungsversuch...", new Color(170, 120, 0));
                    reconnectionManager.startAutoReconnect();
                } else {
                    username = JOptionPane.showInputDialog(frame,
                            "Bitte Username eingeben (" + PROFILE + "):",
                            username != null ? username : "Player");
                    if (username != null && !username.isBlank()) {
                        lobbyPanel.setLocalUsername(username);
                        if (client.attemptConnect(username)) {
                            lobbyPanel.setStatusText("Verbindungsversuch...", new Color(170, 120, 0));
                        }
                    }
                }
            } else {
                shutdownAndExit();
            }
        });
    }

    public void updateReconnectionStatus(int attemptNumber, int maxAttempts, int delaySeconds) {
        SwingUtilities.invokeLater(() -> {
            String status = String.format("Wiederverbinden... Versuch %d/%d (nächster Versuch in %ds)",
                    attemptNumber, maxAttempts, delaySeconds);
            lobbyPanel.setStatusText(status, new Color(170, 120, 0));
        });
    }

    public final class ClientIdentityStore {
        private static final Preferences PREFS = Preferences.userNodeForPackage(ClientIdentityStore.class);
        private static final String KEY_TOKEN = "identifierToken";
        private static final String KEY_PLAYER_ID = "playerId";
        private static final String KEY_USERNAME = "username";

        public static String loadToken() { return PREFS.get(KEY_TOKEN, null); }
        public static void saveToken(String token) {
            if (token != null && !token.isBlank()) PREFS.put(KEY_TOKEN, token);
        }
        public static void clearToken() { PREFS.remove(KEY_TOKEN); }

        public static String loadPlayerId() { return PREFS.get(KEY_PLAYER_ID, null); }
        public static void savePlayerId(String playerId) {
            if (playerId != null && !playerId.isBlank()) PREFS.put(KEY_PLAYER_ID, playerId);
        }

        public static String loadUsername() { return PREFS.get(KEY_USERNAME, null); }
        public static void saveUsername(String username) {
            if (username != null && !username.isBlank()) PREFS.put(KEY_USERNAME, username);
        }
        public static void clearUsername() { PREFS.remove(KEY_USERNAME); }
    }
}