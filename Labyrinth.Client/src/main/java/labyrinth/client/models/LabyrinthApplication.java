package labyrinth.client.models;

import labyrinth.client.ai.AiController;
import labyrinth.client.util.Logger;
import labyrinth.client.enums.PanelName;
import labyrinth.client.messaging.ServerClientFactory;
import labyrinth.client.models.extensions.TreasureUtils;
import labyrinth.client.ui.*;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.messaging.ReconnectionManager;
import labyrinth.client.util.UriHelper;
import labyrinth.contracts.models.Treasure;
import labyrinth.managementclient.model.GameServer;

import javax.swing.*;
import java.awt.*;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

public class LabyrinthApplication {

    private static final Logger log = Logger.getLogger(LabyrinthApplication.class);
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

    private String username;

    private ReconnectionManager reconnectionManager;
    private AiController aiController;

    // Current game state for AI assist button
    private volatile Board currentBoard;
    private volatile List<Player> currentPlayers;
    private volatile labyrinth.contracts.models.CurrentTurnInfo currentTurnInfo;

    private volatile boolean isShuttingDown = false;
    private volatile boolean gameViewShown = false;
    private volatile boolean pendingMultiplayerJoin = false;
    private volatile boolean sessionResetPending = false;
    private volatile boolean isGameOverCleanup = false;
    private volatile boolean isGameOver = false;
    private volatile boolean exitedToLobby = false;

    // Minimum window size (HD resolution)
    private static final int MIN_WINDOW_WIDTH = 1280;
    private static final int MIN_WINDOW_HEIGHT = 720;

    public void start() throws Exception {
        frame = new JFrame("Labyrinth Online (" + PROFILE + ")");

        // Set application icon for taskbar and window header
        java.net.URL iconUrl = getClass().getClassLoader().getResource("images/ui/icon.png");
        if (iconUrl != null) {
            ImageIcon icon = new ImageIcon(iconUrl);
            frame.setIconImage(icon.getImage());
        }

        installWindowCloseHandler();

        frame.setMinimumSize(new Dimension(MIN_WINDOW_WIDTH, MIN_WINDOW_HEIGHT));

        int[] windowSize = OptionsPanel.loadWindowSizeFromPreferences();
        if (windowSize[0] == -1 || windowSize[1] == -1) {
            frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        } else {
            int width = Math.max(windowSize[0], MIN_WINDOW_WIDTH);
            int height = Math.max(windowSize[1], MIN_WINDOW_HEIGHT);
            frame.setSize(width, height);
        }
        frame.setLocationRelativeTo(null);

        mainPanel = new JPanel(new CardLayout());

        ThemeManager.getInstance().addThemeChangeListener(() -> {
            SwingUtilities.invokeLater(() -> {
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

        mainMenuPanel = new MainMenuPanel();

        String storedUsername = ClientIdentityStore.loadUsername();
        if (storedUsername != null && !storedUsername.isBlank()) {
            mainMenuPanel.setMultiplayerUsername(storedUsername);
        }
        mainMenuPanel.setOnMultiplayerClicked(this::showServerBrowser);
        mainMenuPanel.setOnOptionsClicked(this::showOptions);
        mainMenuPanel.setOnExitClicked(this::shutdownAndExit);
        mainPanel.add(mainMenuPanel, PanelName.MAIN_MENU.getCardName());


        var serversApi = ServerClientFactory.create(OptionsPanel.loadServerUrlFromPreferences());
        serverBrowserPanel = new ServerBrowserPanel(serversApi);
        serverBrowserPanel.setOnBackToMenu(this::showMainMenu);
        serverBrowserPanel.setOnServerSelected(this::setUsername);
        mainPanel.add(serverBrowserPanel, PanelName.SERVER_BROWSER.getCardName());

        lobbyPanel = new MultiplayerLobbyPanel(null);
        lobbyPanel.setOnBackToMenu(this::showMainMenu);
        mainPanel.add(lobbyPanel, PanelName.LOBBY.getCardName());


        optionsPanel = new OptionsPanel();
        optionsPanel.setOnBackToMenu(this::showMainMenu);
        optionsPanel.setOnSettingsChanged(this::applySettings);
        optionsPanel.setOnMusicVolumeChanged(volume -> {
            if (mainMenuPanel != null) {
                mainMenuPanel.setMusicVolume(volume);
            }
        });
        optionsPanel.setOnWindowSizeChanged(size -> {
            if (frame != null) {
                if (size[0] == -1 || size[1] == -1) {
                    frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
                } else {
                    frame.setExtendedState(JFrame.NORMAL);
                    frame.setSize(size[0], size[1]);
                    frame.setLocationRelativeTo(null);
                }
            }
        });
        mainPanel.add(optionsPanel, PanelName.OPTIONS.getCardName());

        gameOverPanel = new GameOverPanel(this::exitGameToLobby);
        gameOverPanel.setOnStartNewRound(this::startNewRound);
        mainPanel.add(gameOverPanel, PanelName.GAME_OVER.getCardName());

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


        loginSent = false;
        connectAckReceived = false;
        ClientIdentityStore.clearToken();


        if (client != null && client.isOpen()) {
            isGameOverCleanup = true;
            try {
                client.disconnectCleanly();
            } catch (Exception e) {
                log.error("Error disconnecting when returning to main menu: %s", e.getMessage());
            } finally {
                isGameOverCleanup = false;
            }
        }

        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, PanelName.MAIN_MENU.getCardName());
    }

    private void showOptions() {
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, PanelName.OPTIONS.getCardName());
    }

    private void applySettings() {
        log.info("Settings applied:");
        log.info("  Music Volume: %d%%", optionsPanel.getMusicVolume());
        log.info("  SFX Volume: %d%%", optionsPanel.getSfxVolume());
        log.info("  Server URL: %s", optionsPanel.getServerUrl());
        log.info("  Dark Theme: %s", optionsPanel.isDarkTheme());
    }

    private void showServerBrowser() {
        var serversApi = ServerClientFactory.create(OptionsPanel.loadServerUrlFromPreferences());
        serverBrowserPanel = new ServerBrowserPanel(serversApi);
        serverBrowserPanel.setOnBackToMenu(this::showMainMenu);
        serverBrowserPanel.setOnServerSelected(this::setUsername);

        mainPanel.remove(serverBrowserPanel);
        mainPanel.add(serverBrowserPanel, PanelName.SERVER_BROWSER.getCardName());

        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, PanelName.SERVER_BROWSER.getCardName());

        serverBrowserPanel.onShow();
    }

    private void setUsername(GameServer gameServer) {
        mainMenuPanel.showMultiplayerUsernameDialog(username -> showMultiplayerLobby(gameServer, username));
    }

    private void showMultiplayerLobby(GameServer gameServer, String username) {
        lobbyPanel.setMultiplayerUsername(username);

        ClientIdentityStore.clearToken();
        loginSent = false;
        connectAckReceived = false;
        pendingMultiplayerJoin = true;


        if (client != null && client.isOpen()) {
            isGameOverCleanup = true; // Verhindert ReconnectionManager
            try {
                client.disconnectCleanly();
                reconnectionManager.cancelReconnection();
                client = null;
                lobbyPanel.setClient(null);

                Thread.sleep(200);
            } catch (Exception e) {
                log.error("Error disconnecting before reconnect: %s", e.getMessage());
            } finally {
                isGameOverCleanup = false;
            }
        }

        var serverUri = UriHelper.getGameUri(gameServer.getUri());
        var client = new GameClient(serverUri);

        setupClient(client);
        connectToServer();
    }

    private void setupClient(GameClient client) {
        this.client = client;

        reconnectionManager = new ReconnectionManager(client, this);
        lobbyPanel.setClient(client);

        aiController = new AiController(client);

        registerCallbacks();

        client.setOnConnectionLost(() -> {
            if (isShuttingDown || isGameOverCleanup) {
                log.info("Connection lost during shutdown/game-over cleanup - ignoring");
                return;
            }

            if (pendingMultiplayerJoin) {
                pendingMultiplayerJoin = false;
                SwingUtilities.invokeLater(() -> {
                    DialogFactory.showError(frame, "Verbindungsfehler",
                            "Verbindung zum Server fehlgeschlagen.");
                    showMainMenu();
                });
                return;
            }
            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setStatusText("Verbindung unterbrochen - Wiederverbinden...",
                        new Color(170, 120, 0));
            });
            reconnectionManager.startAutoReconnect();
        });

        client.setOnAchievementUnlocked(achievement -> {
            log.info("[%s] Achievement unlocked: %s for player %s", PROFILE, achievement.getAchievement(), achievement.getPlayerId());

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
                    boardPanel.showSuccessToast("ACHIEVEMENT", "Erfolg freigeschaltet!", displayName);
                }
            });
        });

        client.setOnNextTreasure(nextTreasure -> {
            var treasure = nextTreasure.getTreasure();

            if (treasure == null) {
                log.info("[%s] Next treasure is null", PROFILE);
                return;
            }

            log.info("[%s] Next treasure: %s", PROFILE, treasure);
            SwingUtilities.invokeLater(() -> {
                var player = resolveLocalPlayer(currentPlayers);
                player.setCurrentTargetTreasure(treasure);

                if (boardPanel != null) {
                    boardPanel.showInfoToast("NEXT_TREASURE", "Neues Ziel!", "Finde: " + TreasureUtils.getLocalName(treasure.getId()));
                }
            });
        });

        client.setOnStatusUpdate(status -> {
            SwingUtilities.invokeLater(() -> lobbyPanel.setStatusText(status, new Color(0, 150, 0)));
        });

        setupConnectionHook();
    }

    private void ensureLogin() {
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

        username = mainMenuPanel.getMultiplayerUsername();
        if (username == null || username.isBlank()) {
            username = (storedUsername != null && !storedUsername.isBlank()) ? storedUsername : "Player";
        }
        lobbyPanel.setLocalUsername(username);
        client.sendConnect(username);
    }

    private void connectToServer() {
        lobbyPanel.setStatusText("Verbinde...", new Color(170, 120, 0));

        if (sessionResetPending) {
            loginSent = false;
            connectAckReceived = false;
            client.ensureTransportConnected();
            return;
        }

        if (client != null && client.isOpen()) {
            ensureLogin();
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

            if (token != null) {
                log.info("[%s] onOpen -> RECONNECT with token", PROFILE);
                client.sendReconnect(token);
            } else {
                username = mainMenuPanel.getMultiplayerUsername();
                if (username == null || username.isBlank()) {
                    username = storedUsername != null ? storedUsername : "Player";
                }
                lobbyPanel.setLocalUsername(username);
                log.info("[%s] onOpen -> CONNECT username=%s", PROFILE, username);
                client.sendConnect(username);
            }

            final String finalStoredUsername = storedUsername;
            new Thread(() -> {
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException ignored) {
                }
                if (connectAckReceived) return;

                SwingUtilities.invokeLater(() -> {
                    String t = ClientIdentityStore.loadToken();
                    if (t != null) {
                        log.info("[%s] No CONNECT_ACK after reconnect -> token invalid", PROFILE);
                        ClientIdentityStore.clearToken();
                    }
                    if (username == null || username.isBlank()) {
                        username = mainMenuPanel.getMultiplayerUsername();
                        if (username == null || username.isBlank()) {
                            username = finalStoredUsername != null ? finalStoredUsername : "Player";
                        }
                        lobbyPanel.setLocalUsername(username);
                    }
                    if (!connectAckReceived) {
                        log.info("[%s] Fallback -> CONNECT username=%s", PROFILE, username);
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
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException ignored) {
                }
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
        cl.show(mainPanel, PanelName.BOARD.getCardName());
    }

    private void ensureBoardPanel(Board board, Player currentPlayer, List<Player> allPlayers,
                                  java.time.OffsetDateTime gameEndTime,
                                  labyrinth.contracts.models.CurrentTurnInfo turnInfo) {

        if (boardPanel == null) {
            boardPanel = new BoardPanel(client, board, currentPlayer, allPlayers);
            boardPanel.setOnExitGame(this::exitGameToMainMenu);

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

            mainPanel.add(boardPanel, PanelName.BOARD.getCardName());
        }

        // Always update game state to ensure turnState is set (fixes rotation on first turn)
        boardPanel.updateGameState(
                board,
                allPlayers,
                currentPlayer,
                gameEndTime,
                turnInfo != null ? turnInfo.getTurnEndTime() : null,
                turnInfo != null ? turnInfo.getState() : null
        );
        switchToGameView();
        frame.revalidate();
        frame.repaint();
    }

    private void exitGameToMainMenu() {
        if (client != null && client.isOpen()) {
            try {
                client.disconnectCleanly();
            } catch (Exception e) {
                log.error("Error sending DISCONNECT: %s", e.getMessage());
            }
        }

        ClientIdentityStore.clearToken();

        loginSent = false;
        connectAckReceived = false;
        pendingMultiplayerJoin = false;

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
        log.info("[%s] exitGameToLobby() - Returning to lobby", PROFILE);
        exitGameCleanup();
        showLobby();
        log.info("[%s] Returned to lobby", PROFILE);
    }


    private void exitGameCleanup() {
        if (gameOverPanel != null) {
            gameOverPanel.cleanup();
        }

        gameViewShown = false;
        isGameOver = false;
        pendingMultiplayerJoin = false;
        isGameOverCleanup = false;
        exitedToLobby = true;

        if (boardPanel != null) {
            mainPanel.remove(boardPanel);
            boardPanel = null;
        }
    }

    private void startNewRound() {
        log.info("[%s] startNewRound() - Starting new game", PROFILE);

        if (gameOverPanel != null) {
            gameOverPanel.cleanup();
        }

        gameViewShown = false;
        isGameOver = false;
        pendingMultiplayerJoin = false;
        isGameOverCleanup = false;
        exitedToLobby = false;

        if (boardPanel != null) {
            mainPanel.remove(boardPanel);
            boardPanel = null;
        }

        if (client != null && client.isOpen()) {
            try {
                labyrinth.contracts.models.BoardSize bs = new labyrinth.contracts.models.BoardSize();
                bs.setRows(7);
                bs.setCols(7);

                int treasuresToWin = 4;
                int bonusCount = 4;
                int gameDurationSeconds = 30 * 60;
                int turnTimeSeconds = 30;

                log.info("[%s] Sending START_GAME for new round with %d bonuses", PROFILE, bonusCount);
                client.sendStartGame(bs, treasuresToWin, bonusCount, gameDurationSeconds, turnTimeSeconds);
            } catch (Exception ex) {
                ex.printStackTrace();
                SwingUtilities.invokeLater(() -> {
                    DialogFactory.showError(frame, "Fehler",
                            "Konnte neues Spiel nicht starten: " + ex.getMessage());
                    exitGameToLobby();
                });
            }
        } else {
            SwingUtilities.invokeLater(() -> {
                DialogFactory.showWarning(frame, "Verbindungsfehler",
                        "Keine Verbindung zum Server. Kehre zur Lobby zurück.");
                exitGameToLobby();
            });
        }
    }

    private void showLobby() {
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, PanelName.LOBBY.getCardName());
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
            log.info("[%s] CONNECT_ACK playerId=%s identifierToken=%s", PROFILE, ack.getPlayerId(), ack.getIdentifierToken());
            ClientIdentityStore.saveToken(ack.getIdentifierToken());
            ClientIdentityStore.savePlayerId(ack.getPlayerId());
            ClientIdentityStore.saveUsername(username);
            if (reconnectionManager != null) reconnectionManager.reset();

            if (aiController != null) {
                aiController.setLocalPlayerId(ack.getPlayerId());
            }

            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setConnected(true);
                lobbyPanel.setLocalPlayerId(ack.getPlayerId());
                lobbyPanel.setStatusText("Verbunden mit Server", new Color(0, 150, 0));

                if (pendingMultiplayerJoin) {
                    pendingMultiplayerJoin = false;
                    showLobby();
                }
            });
        });

        client.setOnLobbyState(lobby ->
                SwingUtilities.invokeLater(() -> lobbyPanel.updateLobby(lobby)));

        client.setOnGameStarted(started -> {
            log.info("[%s] Received GAME_STARTED", PROFILE);
            exitedToLobby = false;  // Reset flag - neues Spiel startet

            if (aiController != null) {
                aiController.reset();
                aiController.setAiModeEnabled(false);
            }

            Board board = BoardFactory.fromContracts(started.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(started.getPlayers());
            board.setPlayers(players);
            log.info("%s", board);
            BoardFactory.applyTurnInfo(board, players, started.getCurrentTurnInfo());

            currentBoard = board;
            currentPlayers = players;
            currentTurnInfo = started.getCurrentTurnInfo();

            showGame(board, started.getGameEndTime(), started.getCurrentTurnInfo());

            if (aiController != null) {
                aiController.onGameStateUpdate(board, players, started.getCurrentTurnInfo());
            }
        });

        client.setOnGameStateUpdate(state -> {
            log.info("[%s] Received GAME_STATE_UPDATE", PROFILE);

            if (exitedToLobby) {
                log.info("[%s] Ignoring GAME_STATE_UPDATE - player exited to lobby", PROFILE);
                return;
            }

            Optional<Treasure> currentPlayerNextTreasure = Optional.empty();
            var currentPlayer = resolveLocalPlayer(currentPlayers);
            if (currentPlayer != null) {
                currentPlayerNextTreasure = Optional.ofNullable(currentPlayer.getCurrentTargetTreasure());
            }

            Board board = BoardFactory.fromContracts(state.getBoard());
            List<Player> players = BoardFactory.convertPlayerStates(state.getPlayers());
            currentPlayer = resolveLocalPlayer(players);
            if (currentPlayer != null && currentPlayerNextTreasure.isPresent()) {
                currentPlayer.setCurrentTargetTreasure(currentPlayerNextTreasure.get());
            }

            board.setPlayers(players);
            log.info("%s", board);
            BoardFactory.applyTurnInfo(board, players, state.getCurrentTurnInfo());

            currentBoard = board;
            currentPlayers = players;
            currentTurnInfo = state.getCurrentTurnInfo();

            showGame(board, state.getGameEndTime(), state.getCurrentTurnInfo());

            if (aiController != null && !isGameOver) {
                aiController.onGameStateUpdate(board, players, state.getCurrentTurnInfo());
            }
        });

        client.setOnGameOver(gameOver -> {
            log.info("[%s] *** GAME_OVER EVENT RECEIVED *** Winner: %s", PROFILE, gameOver.getWinnerId());

            isGameOver = true;

            if (aiController != null) {
                aiController.stop();
            }

            pendingMultiplayerJoin = false;
            isGameOverCleanup = true;

            SwingUtilities.invokeLater(() -> {
                try {
                    log.info("[%s] GAME_OVER UI update starting on EDT...", PROFILE);

                    if (boardPanel != null) {
                        boardPanel.setGameOver(true);
                    }

                    if (boardPanel != null) {
                        mainPanel.remove(boardPanel);
                        boardPanel = null;
                        log.info("[%s] BoardPanel removed", PROFILE);
                    }

                    if (currentPlayers != null) {
                        Map<String, String> playerNames = new HashMap<>();
                        for (Player p : currentPlayers) {
                            if (p.getId() != null && p.getName() != null) {
                                playerNames.put(p.getId(), p.getName());
                            }
                        }
                        gameOverPanel.setPlayerNames(playerNames);
                        log.info("[%s] Set player names for GameOverPanel: %s", PROFILE, playerNames);
                    }

                    gameOverPanel.updateGameOver(gameOver);
                    log.info("[%s] GameOverPanel updated", PROFILE);

                    if (mainMenuPanel != null) mainMenuPanel.stopMusic();
                    gameViewShown = false;

                    CardLayout cl = (CardLayout) mainPanel.getLayout();
                    cl.show(mainPanel, PanelName.GAME_OVER.getCardName());
                    log.info("[%s] Switched to gameover view", PROFILE);

                    gameOverPanel.setVisible(true);
                    mainPanel.revalidate();
                    mainPanel.repaint();
                    frame.revalidate();
                    frame.repaint();
                    gameOverPanel.requestFocusInWindow();
                    log.info("[%s] GAME_OVER UI update complete", PROFILE);
                } catch (Exception e) {
                    log.error("[%s] Error in GAME_OVER UI update: %s", PROFILE, e.getMessage());
                    e.printStackTrace();
                }

                log.info("[%s] GAME_OVER complete - staying connected for lobby", PROFILE);
            });
        });

        client.setOnErrorMessage(msg -> {
            SwingUtilities.invokeLater(() -> {
                if (msg != null && msg.contains("PLAYER_NOT_FOUND")) {
                    log.info("[%s] PLAYER_NOT_FOUND - ignoring (token already cleared)", PROFILE);
                    ClientIdentityStore.clearToken();
                } else if (msg != null && msg.contains("already connected")) {
                    log.info("[%s] Session already connected - ignoring: %s", PROFILE, msg);
                } else if (pendingMultiplayerJoin) {
                    pendingMultiplayerJoin = false;
                    DialogFactory.showError(frame, "Verbindungsfehler", msg);
                    showMainMenu();
                } else {
                    if (boardPanel != null && gameViewShown) {
                        handleErrorWithToast(msg);
                    } else {
                        DialogFactory.showError(frame, "Fehler", msg);
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

                case "NOT_ADMIN" -> {
                    errorCode = "201";
                    errorTitle = "Keine Berechtigung";
                }

                case "BONUS_NOT_AVAILABLE" -> {
                    errorCode = "301";
                    errorTitle = "Bonus nicht verfügbar";
                }

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
                case "INVALID_COMMAND" -> {
                    errorCode = "501";
                    errorTitle = "Ungültiger Befehl";
                }

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
            errorCode = "999";
            errorTitle = "Fehler";
            errorMessage = msg;
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
        if (isGameOver || isGameOverCleanup) {
            log.info("[%s] showGame() ignored - game is over", PROFILE);
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
            boolean retry = DialogFactory.showConfirm(frame, "Verbindung unterbrochen",
                    "Automatische Wiederverbindung fehlgeschlagen.\nMöchten Sie es manuell erneut versuchen?");
            if (retry) {
                String token = ClientIdentityStore.loadToken();
                if (token != null) {
                    lobbyPanel.setStatusText("Manueller Wiederverbindungsversuch...", new Color(170, 120, 0));
                    reconnectionManager.startAutoReconnect();
                } else {
                    String reconnectUsername = mainMenuPanel.getMultiplayerUsername();
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

    public static final class ClientIdentityStore {
        private static final Preferences PREFS = Preferences.userNodeForPackage(ClientIdentityStore.class);
        private static final String KEY_TOKEN = "identifierToken";
        private static final String KEY_PLAYER_ID = "playerId";
        private static final String KEY_USERNAME = "username";

        public static String loadToken() {
            return PREFS.get(KEY_TOKEN, null);
        }

        public static void saveToken(String token) {
            if (token != null && !token.isBlank()) PREFS.put(KEY_TOKEN, token);
        }

        public static void clearToken() {
            PREFS.remove(KEY_TOKEN);
        }

        public static void savePlayerId(String playerId) {
            if (playerId != null && !playerId.isBlank()) PREFS.put(KEY_PLAYER_ID, playerId);
        }

        public static String loadUsername() {
            return PREFS.get(KEY_USERNAME, null);
        }

        public static void saveUsername(String username) {
            if (username != null && !username.isBlank()) PREFS.put(KEY_USERNAME, username);
        }
    }
}