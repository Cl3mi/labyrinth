package labyrinth.client.models;

import labyrinth.client.ai.AiController;
import labyrinth.client.messaging.ServerClientFactory;
import labyrinth.client.ui.*;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.messaging.ReconnectionManager;
import labyrinth.contracts.models.Treasure;
import labyrinth.managementclient.model.GameServer;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

public class LabyrinthApplication {

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
    private ServerBrowserPanel serverBrowserPanel;
    private OptionsPanel optionsPanel;
    private BoardPanel boardPanel;
    private GameOverPanel gameOverPanel;
    private JPanel gamePanel; // Ensure gamePanel is defined

    private String username;
    private String identifierToken;

    private ReconnectionManager reconnectionManager;
    private AiController aiController;

    // Current game state for AI assist button
    private volatile Board currentBoard;
    private volatile List<Player> currentPlayers;
    private volatile labyrinth.contracts.models.CurrentTurnInfo currentTurnInfo;

    private volatile boolean isShuttingDown = false;
    private volatile boolean gameViewShown = false;
    private volatile boolean pendingSingleplayerStart = false;
    private volatile boolean sessionResetPending = false;
    private volatile boolean isGameOverCleanup = false;
    private volatile boolean isGameOver = false;
    private volatile boolean exitedToLobby = false;

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

        // Theme-Änderungen überwachen und UI neu zeichnen
        ThemeManager.getInstance().addThemeChangeListener(() -> {
            SwingUtilities.invokeLater(() -> {
                // Alle Komponenten invalidieren und neu zeichnen
                if (mainMenuPanel != null) {
                    mainMenuPanel.revalidate();
                    mainMenuPanel.repaint();
                }
                if (lobbyPanel != null) {
                    lobbyPanel.revalidate();
                    lobbyPanel.repaint();
                }
                if (optionsPanel != null) {
                    optionsPanel.revalidate();
                    optionsPanel.repaint();
                }
                if (boardPanel != null) {
                    boardPanel.revalidate();
                    boardPanel.repaint();
                }
                mainPanel.revalidate();
                mainPanel.repaint();
                frame.repaint();
            });
        });

        // Hauptmenü erstellen
        mainMenuPanel = new MainMenuPanel();
        // Gespeicherten Username laden und setzen (für beide Modi)
        String storedUsername = ClientIdentityStore.loadUsername();
        if (storedUsername != null && !storedUsername.isBlank()) {
            mainMenuPanel.setSingleplayerUsername(storedUsername);
            mainMenuPanel.setMultiplayerUsername(storedUsername);
        }
        mainMenuPanel.setOnSingleplayerClicked(this::startSingleplayerGame);
        mainMenuPanel.setOnMultiplayerClicked(this::showServerBrowser);
        mainMenuPanel.setOnOptionsClicked(this::showOptions);
        mainMenuPanel.setOnExitClicked(this::shutdownAndExit);
        mainPanel.add(mainMenuPanel, "mainmenu");


        var serversApi = ServerClientFactory.create(OptionsPanel.loadServerUrlFromPreferences());
        serverBrowserPanel = new ServerBrowserPanel(serversApi);
        serverBrowserPanel.setOnBackToMenu(this::showMainMenu);
        serverBrowserPanel.setOnServerSelected(this::setUsername);
        mainPanel.add(serverBrowserPanel, "serverbrowser");

        // Multiplayer-Lobby erstellen
        lobbyPanel = new MultiplayerLobbyPanel(null);
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
        gameOverPanel = new GameOverPanel(this::exitGameToLobby);
        gameOverPanel.setOnStartNewRound(this::startNewRound);
        mainPanel.add(gameOverPanel, "gameover");

        frame.setContentPane(mainPanel);
        frame.setVisible(true);

        loginSent = false;
        connectAckReceived = false;


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

    private void showServerBrowser() {
        // ServerBrowserPanel mit aktueller URL neu erstellen
        var serversApi = ServerClientFactory.create(OptionsPanel.loadServerUrlFromPreferences());
        serverBrowserPanel = new ServerBrowserPanel(serversApi);
        serverBrowserPanel.setOnBackToMenu(this::showMainMenu);
        serverBrowserPanel.setOnServerSelected(this::setUsername);

        // Altes Panel entfernen, neues hinzufügen
        mainPanel.remove(serverBrowserPanel); // Falls vorhanden
        mainPanel.add(serverBrowserPanel, "serverbrowser");

        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "serverbrowser");

        serverBrowserPanel.onShow();
    }

    private void setUsername(GameServer gameServer) {
        mainMenuPanel.showMultiplayerUsernameDialog(new Consumer<String>() {
            @Override
            public void accept(String username) {
                showMultiplayerLobby(gameServer, username);
            }
        });
    }
    private void showMultiplayerLobby(GameServer gameServer, String username) {
        lobbyPanel.setMultiplayerUsername(username);

        // Für einen neuen Multiplayer-Beitritt Token löschen und Flags zurücksetzen
        ClientIdentityStore.clearToken();
        loginSent = false;
        connectAckReceived = false;

        // Wenn bereits verbunden, erst komplett schließen (nicht nur sendDisconnect)
        if (client != null && client.isOpen()) {
            isGameOverCleanup = true; // Verhindert ReconnectionManager
            try {
                client.disconnectCleanly();
                reconnectionManager.cancelReconnection();
                client = null;
                lobbyPanel.setClient(null);

                // Kurz warten damit der Server die Trennung verarbeiten kann
                Thread.sleep(200);
            } catch (Exception e) {
                System.err.println("Error disconnecting before reconnect: " + e.getMessage());
            } finally {
                isGameOverCleanup = false;
            }
        }

        var client = new GameClient(URI.create(gameServer.getUri()));
        setupClient(client);

        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "lobby");
        connectToServer();
    }

    private void setupClient(GameClient client) {
        this.client = client;

        reconnectionManager = new ReconnectionManager(client, this);
        lobbyPanel.setClient(client);

        // Initialize AI controller for manual AI assist (button click)
        aiController = new AiController(client);

        registerCallbacks();

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

            // Add achievement to GameOverPanel for later display
            if (gameOverPanel != null) {
                String achievementName = achievement.getAchievement() != null
                        ? achievement.getAchievement().toString() : "UNKNOWN";
                gameOverPanel.addAchievement(achievement.getPlayerId(), achievementName);
            }

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
            var treasure = nextTreasure.getTreasure();

            if (treasure == null) {
                System.out.println("[" + PROFILE + "] Next treasure is null");
                return;
            }

            System.out.println("[" + PROFILE + "] Next treasure: " + treasure);
            SwingUtilities.invokeLater(() -> {
                var player = resolveLocalPlayer(currentPlayers);
                player.setCurrentTargetTreasure(treasure);

                if (boardPanel != null) {
                    boardPanel.showInfoToast("NEXT_TREASURE", "🎯 Neues Ziel!", "Finde: " + treasure.getName());
                }
            });
        });

        client.setOnStatusUpdate(status -> {
            SwingUtilities.invokeLater(() -> lobbyPanel.setStatusText(status, new Color(0, 150, 0)));
        });

        setupConnectionHook();
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
        int totalBonusCount = 4;  // Default: 4 Bonuses im Singleplayer
        int gameDurationSeconds = mainMenuPanel.getSingleplayerGameDuration() * 60;
        int turnTimeSeconds = mainMenuPanel.getSingleplayerTurnTime();

        try {
            System.out.println("SINGLEPLAYER -> sending START_GAME");
            System.out.println("rows=" + bs.getRows() + " cols=" + bs.getCols()
                    + " treasures=" + treasuresToWin
                    + " bonuses=" + totalBonusCount
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

    private void ensureBoardPanel(Board board, Player currentPlayer, List<Player> allPlayers,
                                  java.time.OffsetDateTime gameEndTime,
                                  labyrinth.contracts.models.CurrentTurnInfo turnInfo) {

        if (boardPanel == null) {
            boardPanel = new BoardPanel(client, board, currentPlayer, allPlayers);
            boardPanel.setOnExitGame(this::exitGameToLobby);

            // Wire up AI toggle button
            boardPanel.setOnAiToggleRequested(() -> {
                if (aiController != null) {
                    aiController.toggleAiMode();
                    // If AI was just enabled and we have game state, trigger immediately
                    if (aiController.isAiModeEnabled() && currentBoard != null && currentPlayers != null && currentTurnInfo != null) {
                        aiController.onGameStateUpdate(currentBoard, currentPlayers, currentTurnInfo);
                    }
                }
            });

            // Set up AI callbacks for UI feedback
            if (aiController != null) {
                // Callback when AI mode is toggled
                aiController.setOnAiModeChanged(() -> SwingUtilities.invokeLater(() -> {
                    if (boardPanel != null && aiController != null) {
                        boardPanel.setAiModeEnabled(aiController.isAiModeEnabled());
                    }
                }));

                // Callback when AI starts/stops thinking
                aiController.setThinkingCallbacks(
                    () -> SwingUtilities.invokeLater(() -> {
                        if (boardPanel != null) boardPanel.setAiThinking(true);
                    }),
                    () -> SwingUtilities.invokeLater(() -> {
                        if (boardPanel != null) boardPanel.setAiThinking(false);
                    })
                );
            }

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

    private void exitGameToLobby() {
        System.out.println("[" + PROFILE + "] exitGameToLobby() - Returning to lobby");

        // Cleanup game over panel
        if (gameOverPanel != null) {
            gameOverPanel.cleanup();
        }

        // Reset game state flags
        gameViewShown = false;
        isGameOver = false;
        pendingSingleplayerStart = false;
        isGameOverCleanup = false;
        exitedToLobby = true;  // Block GAME_STATE_UPDATE from showing game again

        // BoardPanel entfernen und zurücksetzen
        if (boardPanel != null) {
            mainPanel.remove(boardPanel);
            boardPanel = null;
        }

        // Lobby-Panel aktualisieren
        SwingUtilities.invokeLater(() -> {
            if (client != null && client.isOpen()) {
                // PlayerId aus gespeicherten Preferences laden
                String savedPlayerId = ClientIdentityStore.loadPlayerId();
                if (savedPlayerId != null) {
                    lobbyPanel.setLocalPlayerId(savedPlayerId);
                    System.out.println("[" + PROFILE + "] Set localPlayerId in lobby: " + savedPlayerId);
                }

                // Username setzen
                if (username != null) {
                    lobbyPanel.setLocalUsername(username);
                    System.out.println("[" + PROFILE + "] Set localUsername in lobby: " + username);
                }

                lobbyPanel.setConnected(true);
                lobbyPanel.setStatusText("✓ Verbunden - Bereit für neues Spiel", new Color(100, 200, 100));

                // Start-Button manuell aktivieren wenn Admin
                lobbyPanel.forceEnableStartButton();
                System.out.println("[" + PROFILE + "] Lobby ready, start button force-enabled");
            } else {
                lobbyPanel.setConnected(false);
                lobbyPanel.setStatusText("Nicht verbunden", new Color(170, 120, 0));
            }
        });

        showLobby();
        System.out.println("[" + PROFILE + "] Returned to lobby");
    }

    private void startNewRound() {
        System.out.println("[" + PROFILE + "] startNewRound() - Starting new game");

        // Cleanup game over panel
        if (gameOverPanel != null) {
            gameOverPanel.cleanup();
        }

        // Reset game state flags
        gameViewShown = false;
        isGameOver = false;
        pendingSingleplayerStart = false;
        isGameOverCleanup = false;
        exitedToLobby = false;

        // BoardPanel entfernen und zurücksetzen
        if (boardPanel != null) {
            mainPanel.remove(boardPanel);
            boardPanel = null;
        }

        // Send START_GAME command with default settings
        if (client != null && client.isOpen()) {
            try {
                labyrinth.contracts.models.BoardSize bs = new labyrinth.contracts.models.BoardSize();
                bs.setRows(7);
                bs.setCols(7);

                // Use default game settings
                int treasuresToWin = 4;
                int gameDurationSeconds = 30 * 60; // 30 minutes
                int turnTimeSeconds = 30;

                System.out.println("[" + PROFILE + "] Sending START_GAME for new round");
                client.sendStartGame(bs, treasuresToWin, 0, gameDurationSeconds, turnTimeSeconds);
            } catch (Exception ex) {
                ex.printStackTrace();
                SwingUtilities.invokeLater(() -> {
                    JOptionPane.showMessageDialog(frame,
                            "Konnte neues Spiel nicht starten: " + ex.getMessage(),
                            "Fehler", JOptionPane.ERROR_MESSAGE);
                    // Fall back to lobby on error
                    exitGameToLobby();
                });
            }
        } else {
            SwingUtilities.invokeLater(() -> {
                JOptionPane.showMessageDialog(frame,
                        "Keine Verbindung zum Server. Kehre zur Lobby zurück.",
                        "Verbindungsfehler", JOptionPane.WARNING_MESSAGE);
                exitGameToLobby();
            });
        }
    }

    private void showLobby() {
        // Use CardLayout to switch to lobby
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "lobby");
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

            // Set local player ID for AI controller
            if (aiController != null) {
                aiController.setLocalPlayerId(ack.getPlayerId());
            }

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
            exitedToLobby = false;  // Reset flag - neues Spiel startet

            // Reset AI mode at game start
            if (aiController != null) {
                aiController.setAiModeEnabled(false);
            }

            Board board = BoardFactory.fromContracts(started.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(started.getPlayers());
            board.setPlayers(players);
            System.out.println(board);
            BoardFactory.applyTurnInfo(board, players, started.getCurrentTurnInfo());

            // Store current state for AI
            currentBoard = board;
            currentPlayers = players;
            currentTurnInfo = started.getCurrentTurnInfo();

            showGame(board, started.getGameEndTime(), started.getCurrentTurnInfo());

            // Trigger AI if enabled and it's local player's turn
            if (aiController != null) {
                aiController.onGameStateUpdate(board, players, started.getCurrentTurnInfo());
            }
        });

        client.setOnGameStateUpdate(state -> {
            System.out.println("[" + PROFILE + "] Received GAME_STATE_UPDATE");

            // Ignoriere Updates wenn der Spieler zur Lobby zurückgekehrt ist
            if (exitedToLobby) {
                System.out.println("[" + PROFILE + "] Ignoring GAME_STATE_UPDATE - player exited to lobby");
                return;
            }



            Optional<Treasure> currentPlayerNextTreasure = Optional.empty();
            var currentPlayer = resolveLocalPlayer(currentPlayers);
            if(currentPlayer != null) {
                currentPlayerNextTreasure = Optional.ofNullable(currentPlayer.getCurrentTargetTreasure());
            }

            Board board = BoardFactory.fromContracts(state.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(state.getPlayers());
            currentPlayer = resolveLocalPlayer(players);
            if(currentPlayer != null && currentPlayerNextTreasure.isPresent()) {
                currentPlayer.setCurrentTargetTreasure(currentPlayerNextTreasure.get());
            }

            board.setPlayers(players);
            System.out.println(board);
            BoardFactory.applyTurnInfo(board, players, state.getCurrentTurnInfo());

            // Store current state for AI
            currentBoard = board;
            currentPlayers = players;
            currentTurnInfo = state.getCurrentTurnInfo();

            showGame(board, state.getGameEndTime(), state.getCurrentTurnInfo());

            // Trigger AI if enabled and it's local player's turn
            if (aiController != null && !isGameOver) {
                aiController.onGameStateUpdate(board, players, state.getCurrentTurnInfo());
            }
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

                    // Prevent any further input on the board
                    if (boardPanel != null) {
                        boardPanel.setGameOver(true);
                    }

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

                // WICHTIG: NICHT disconnecten! Der Server sendet automatisch LOBBY_STATE
                // und wir wollen in der Lobby bleiben um ein neues Spiel zu starten
                System.out.println("[" + PROFILE + "] GAME_OVER complete - staying connected for lobby");
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
                        // Re-enable start button in lobby after an error
                        if (lobbyPanel != null) {
                            lobbyPanel.forceEnableStartButton();
                        }
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