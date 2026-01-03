package labyrinth.client.models;

import labyrinth.client.ui.BoardPanel;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.messaging.ReconnectionManager;
import labyrinth.client.ui.GameOverPanel;
import labyrinth.client.ui.MainMenuPanel;
import labyrinth.client.ui.MultiplayerLobbyPanel;
import labyrinth.client.ui.OptionsPanel;
import org.java_websocket.enums.ReadyState;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
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
    private volatile boolean sessionResetPending = false;
    private volatile boolean isGameOverCleanup = false;
    private volatile boolean isGameOver = false;

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
        // Gespeicherten Username laden und setzen (für beide Modi)
        String storedUsername = ClientIdentityStore.loadUsername();
        if (storedUsername != null && !storedUsername.isBlank()) {
            mainMenuPanel.setSingleplayerUsername(storedUsername);
            mainMenuPanel.setMultiplayerUsername(storedUsername);
        }
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
        gameOverPanel = new GameOverPanel(this::exitGameToMainMenu);
        mainPanel.add(gameOverPanel, "gameover");

        frame.setContentPane(mainPanel);
        frame.setVisible(true);

        loginSent = false;
        connectAckReceived = false;

        client.setOnConnectionLost(() -> {
            if (isShuttingDown || isGameOverCleanup) {
                System.out.println("Connection lost during shutdown/game-over cleanup - ignoring");
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
        isGameOver = false;

        // ZUERST alle Flags zurücksetzen und Token löschen
        loginSent = false;
        connectAckReceived = false;
        ClientIdentityStore.clearToken();

        // Verbindung komplett schließen, damit onOpenHook beim nächsten Verbindungsversuch erneut ausgelöst wird
        if (client != null && client.isOpen()) {
            isGameOverCleanup = true; // Verhindert ReconnectionManager
            try {
                client.disconnectCleanly(); // Vollständig schließen statt nur sendDisconnect
            } catch (Exception e) {
                System.err.println("Error disconnecting when returning to main menu: " + e.getMessage());
            } finally {
                isGameOverCleanup = false;
            }
        }

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
        // Username aus dem MainMenu-Dialog an das LobbyPanel übergeben
        String multiplayerUsername = mainMenuPanel.getMultiplayerUsername();
        lobbyPanel.setMultiplayerUsername(multiplayerUsername);

        // Für einen neuen Multiplayer-Beitritt Token löschen und Flags zurücksetzen
        ClientIdentityStore.clearToken();
        loginSent = false;
        connectAckReceived = false;

        // Wenn bereits verbunden, erst komplett schließen (nicht nur sendDisconnect)
        if (client != null && client.isOpen()) {
            isGameOverCleanup = true; // Verhindert ReconnectionManager
            try {
                client.disconnectCleanly();
                // Kurz warten damit der Server die Trennung verarbeiten kann
                Thread.sleep(200);
            } catch (Exception e) {
                System.err.println("Error disconnecting before reconnect: " + e.getMessage());
            } finally {
                isGameOverCleanup = false;
            }
        }

        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "lobby");
        connectToServer();
    }

    private void startSingleplayerGame() {
        pendingSingleplayerStart = true;

        // Bei Singleplayer immer ein neues Spiel starten - keinen alten Token verwenden
        ClientIdentityStore.clearToken();
        loginSent = false;
        connectAckReceived = false;

        // Wenn bereits verbunden, erst komplett schließen und dann neu verbinden
        if (client != null && client.isOpen()) {
            isGameOverCleanup = true; // Verhindert ReconnectionManager
            try {
                client.disconnectCleanly();
                // Kurz warten damit der Server die Trennung verarbeiten kann
                Thread.sleep(200);
            } catch (Exception e) {
                System.err.println("Error disconnecting before singleplayer start: " + e.getMessage());
            } finally {
                isGameOverCleanup = false;
            }
        }


        try {
            client.ensureTransportConnected();
        } catch (Exception e) {
            e.printStackTrace();
            pendingSingleplayerStart = false;
            SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(frame,
                    "Konnte nicht zum Server verbinden: " + e.getMessage(),
                    "Verbindungsfehler", JOptionPane.ERROR_MESSAGE));
        }
    }


    private void ensureLogin(boolean singleplayer) {
        if (connectAckReceived) return;
        if (loginSent) return;
        if (client == null || !client.isOpen()) return;

        loginSent = true;

        String token = ClientIdentityStore.loadToken();
        String storedUsername = ClientIdentityStore.loadUsername();

        if (token != null) {
            client.sendReconnect(token);
            return;
        }

        if (singleplayer) {
            // Username aus dem Singleplayer-Dialog verwenden
            username = mainMenuPanel.getSingleplayerUsername();
            if (username == null || username.isBlank()) {
                username = (storedUsername != null && !storedUsername.isBlank()) ? storedUsername : "Player";
            }
            client.sendConnect(username);
            return;
        }

        // Multiplayer: Username aus dem MainMenuPanel verwenden (wurde im Dialog festgelegt)
        username = mainMenuPanel.getMultiplayerUsername();
        if (username == null || username.isBlank()) {
            username = (storedUsername != null && !storedUsername.isBlank()) ? storedUsername : "Player";
        }
        lobbyPanel.setLocalUsername(username);
        client.sendConnect(username);
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
        lobbyPanel.setStatusText("Verbinde...", new Color(170, 120, 0));

        // Wenn gerade ein Session-Reset läuft: nicht CONNECT senden, sondern Transport neu öffnen
        if (sessionResetPending) {
            loginSent = false;
            connectAckReceived = false;
            client.ensureTransportConnected();
            return;
        }

        if (client != null && client.isOpen()) {
            ensureLogin(false);
            return;
        }

        loginSent = false;
        connectAckReceived = false;
        client.ensureTransportConnected();
    }

    private void setupConnectionHook() {
        client.setOnOpenHook(() -> {
            if (loginSent) return;
            loginSent = true;

            String token = ClientIdentityStore.loadToken();
            String storedUsername = ClientIdentityStore.loadUsername();

            // Bei Singleplayer NIE RECONNECT senden - immer neues Spiel starten
            if (pendingSingleplayerStart) {
                username = mainMenuPanel.getSingleplayerUsername();
                if (username == null || username.isBlank()) {
                    username = storedUsername != null ? storedUsername : "Player";
                }
                System.out.println("[" + PROFILE + "] onOpen -> SINGLEPLAYER CONNECT username=" + username);
                client.sendConnect(username);
                return;
            }

            if (token != null) {
                System.out.println("[" + PROFILE + "] onOpen -> RECONNECT with token");
                client.sendReconnect(token);
            } else {
                // Multiplayer: Username aus dem MainMenuPanel verwenden (wurde im Dialog festgelegt)
                username = mainMenuPanel.getMultiplayerUsername();
                if (username == null || username.isBlank()) {
                    username = storedUsername != null ? storedUsername : "Player";
                }
                lobbyPanel.setLocalUsername(username);
                System.out.println("[" + PROFILE + "] onOpen -> MULTIPLAYER CONNECT username=" + username);
                client.sendConnect(username);
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
                        // Bei Singleplayer Username aus Dialog verwenden
                        if (pendingSingleplayerStart) {
                            username = mainMenuPanel.getSingleplayerUsername();
                            if (username == null || username.isBlank()) {
                                username = finalStoredUsername != null ? finalStoredUsername : "Player";
                            }
                        } else {
                            // Multiplayer: Username aus MainMenuPanel verwenden
                            username = mainMenuPanel.getMultiplayerUsername();
                            if (username == null || username.isBlank()) {
                                username = finalStoredUsername != null ? finalStoredUsername : "Player";
                            }
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
                sessionResetPending = true;
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
        if (gameOverPanel != null) gameOverPanel.cleanup();
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "game");
    }

    private void switchToGameOverView() {
        if (mainMenuPanel != null) mainMenuPanel.stopMusic();
        gameViewShown = false;
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "gameover");
        // Sicherstellen, dass das Panel angezeigt wird
        mainPanel.revalidate();
        mainPanel.repaint();
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
            boardPanel.setOnExitGame(this::exitGameToMainMenu);
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
        if (client != null && client.isOpen()) {
            try {
                client.disconnectCleanly();
            } catch (Exception e) {
                System.err.println("Error sending DISCONNECT: " + e.getMessage());
            }
        }

        // Token löschen, da der Server das Spiel zurücksetzt und der Token ungültig wird
        ClientIdentityStore.clearToken();

        // Lokale Login-Flags zurücksetzen, damit beim nächsten Start wieder CONNECT/RECONNECT geschickt wird
        loginSent = false;
        connectAckReceived = false;
        pendingSingleplayerStart = false;

        // BoardPanel entfernen und zurücksetzen, damit beim nächsten Spiel ein frisches Panel erstellt wird
        if (boardPanel != null) {
            mainPanel.remove(boardPanel);
            boardPanel = null;
        }

        SwingUtilities.invokeLater(() -> {
            lobbyPanel.setConnected(false);
            lobbyPanel.setStatusText("Nicht verbunden", new Color(170, 120, 0));
        });

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
            sessionResetPending = false;
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
            System.out.println("[" + PROFILE + "] *** GAME_OVER EVENT RECEIVED *** Winner: " + gameOver.getWinnerId());

            // Sofort Flag setzen um weitere GAME_STATE_UPDATE Events zu ignorieren
            isGameOver = true;

            // DEBUG: Zeige ein Popup um zu verifizieren, dass das Event ankommt
            // (Kann später entfernt werden)
            // SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(frame, "GAME_OVER empfangen! Winner: " + gameOver.getWinnerId(), "DEBUG", JOptionPane.INFORMATION_MESSAGE));

            // Token löschen, da der Server das Spiel zurücksetzt und der Token ungültig wird
            ClientIdentityStore.clearToken();

            // Lokale Flags zurücksetzen, damit beim nächsten Start wieder sauber reconnect/login läuft
            loginSent = false;
            connectAckReceived = false;
            pendingSingleplayerStart = false;

            // Flag setzen um ReconnectionManager zu blockieren
            isGameOverCleanup = true;

            // UI-Updates auf dem EDT ausführen
            SwingUtilities.invokeLater(() -> {
                try {
                    System.out.println("[" + PROFILE + "] GAME_OVER UI update starting on EDT...");

                    // BoardPanel entfernen und zurücksetzen
                    if (boardPanel != null) {
                        mainPanel.remove(boardPanel);
                        boardPanel = null;
                        System.out.println("[" + PROFILE + "] BoardPanel removed");
                    }

                    // GameOverPanel aktualisieren und anzeigen
                    gameOverPanel.updateGameOver(gameOver);
                    System.out.println("[" + PROFILE + "] GameOverPanel updated");

                    // Musik stoppen
                    if (mainMenuPanel != null) mainMenuPanel.stopMusic();
                    gameViewShown = false;

                    // Zum GameOver-Panel wechseln
                    CardLayout cl = (CardLayout) mainPanel.getLayout();
                    cl.show(mainPanel, "gameover");
                    System.out.println("[" + PROFILE + "] Switched to gameover view");

                    // Panel sichtbar machen und neu zeichnen
                    gameOverPanel.setVisible(true);
                    mainPanel.revalidate();
                    mainPanel.repaint();
                    frame.revalidate();
                    frame.repaint();
                    gameOverPanel.requestFocusInWindow();
                    System.out.println("[" + PROFILE + "] GAME_OVER UI update complete");
                } catch (Exception e) {
                    System.err.println("[" + PROFILE + "] Error in GAME_OVER UI update: " + e.getMessage());
                    e.printStackTrace();
                }

                // Disconnect erst NACH den UI-Updates starten
                new Thread(() -> {
                    try {
                        Thread.sleep(1000); // Länger warten um sicher zu sein
                        if (client != null && client.isOpen()) {
                            System.out.println("[" + PROFILE + "] Disconnecting after GAME_OVER...");
                            client.disconnectCleanly();
                        }
                    } catch (Exception e) {
                        System.err.println("Error during GAME_OVER cleanup: " + e.getMessage());
                    } finally {
                        try { Thread.sleep(500); } catch (InterruptedException ignored) {}
                        isGameOverCleanup = false;
                    }
                }, "gameover-cleanup").start();
            });
        });

        client.setOnErrorMessage(msg -> {
            SwingUtilities.invokeLater(() -> {
                if (msg != null && msg.contains("PLAYER_NOT_FOUND")) {
                    // Token wurde bereits gelöscht, ignoriere diese Meldung einfach
                    // Das passiert wenn ein alter RECONNECT-Versuch den Server erreicht
                    System.out.println("[" + PROFILE + "] PLAYER_NOT_FOUND - ignoring (token already cleared)");
                    ClientIdentityStore.clearToken();
                    // Kein Popup anzeigen, Benutzer soll einfach neu verbinden
                } else if (msg != null && msg.contains("already connected")) {
                    // Diese Meldung nur loggen, nicht als Popup anzeigen - passiert bei schnellem Wechsel
                    System.out.println("[" + PROFILE + "] Session already connected - ignoring: " + msg);
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
                case "NOT_YOUR_TURN" -> { errorCode = "101"; errorTitle = "Nicht an der Reihe"; }
                case "INVALID_PUSH" -> { errorCode = "102"; errorTitle = "Ungültiger Einschub"; }
                case "INVALID_MOVE" -> { errorCode = "103"; errorTitle = "Ungültige Bewegung"; }
                case "GAME_NOT_STARTED" -> { errorCode = "104"; errorTitle = "Spiel nicht gestartet"; }
                case "TIMEOUT" -> { errorCode = "204"; errorTitle = "Zeitüberschreitung"; }
                default -> { errorCode = "999"; errorTitle = "Fehler"; }
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
        // Wenn das Spiel bereits beendet ist, keine Game-View mehr anzeigen
        if (isGameOver || isGameOverCleanup) {
            System.out.println("[" + PROFILE + "] showGame() ignored - game is over");
            return;
        }
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
                    // Username aus MainMenuPanel verwenden (Multiplayer oder Singleplayer)
                    String reconnectUsername = pendingSingleplayerStart
                            ? mainMenuPanel.getSingleplayerUsername()
                            : mainMenuPanel.getMultiplayerUsername();
                    if (reconnectUsername == null || reconnectUsername.isBlank()) {
                        reconnectUsername = username != null ? username : "Player";
                    }
                    username = reconnectUsername;
                    lobbyPanel.setLocalUsername(username);
                    if (client.attemptConnect(username)) {
                        lobbyPanel.setStatusText("Verbindungsversuch...", new Color(170, 120, 0));
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