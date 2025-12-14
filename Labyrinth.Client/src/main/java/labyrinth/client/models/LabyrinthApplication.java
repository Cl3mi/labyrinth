package labyrinth.client.models;

import labyrinth.client.BoardPanel;
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

public class LabyrinthApplication {

    private static final URI SERVER_URI = URI.create("ws://localhost:8080/game");

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

        // 1) Frame + Layout
        frame = new JFrame("Labyrinth Online (" + PROFILE + ")");
        installWindowCloseHandler();
        frame.setSize(1400, 900);
        frame.setLocationRelativeTo(null);

        mainPanel = new JPanel(new CardLayout());

        // 2) Client erstellen
        client = new GameClient(SERVER_URI);
        registerCallbacks();

        // 3) LobbyPanel mit client erstellen
        lobbyPanel = new LobbyPanel(client, null);
        mainPanel.add(lobbyPanel, "lobby");

        frame.setContentPane(mainPanel);
        frame.setVisible(true);

        // 4) Login nur einmal, nur in onOpen
        final boolean[] loginSent = {false};

        client.setOnOpenHook(() -> {
            if (loginSent[0]) return;
            loginSent[0] = true;

            // Reconnect ist bei eurem Server nach Neustart/Disconnect nicht stabil -> daher immer CONNECT.
            username = JOptionPane.showInputDialog(frame, "Bitte Username eingeben (" + PROFILE + "):");
            if (username == null || username.isBlank()) username = "Player";

            // Für Lobby-UI ("Du" und Admin via Name)
            lobbyPanel.setLocalUsername(username);

            System.out.println("[" + PROFILE + "] onOpen -> CONNECT username=" + username);
            client.sendConnect(username);
        });

        // 5) connect() starten
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
            if (lobbyPanel != null) lobbyPanel.stopMusic();

            if (client != null && client.isOpen()) {
                client.sendDisconnect();
            }
            if (client != null) {
                client.close();
            }

            // Wichtig: keine Reconnect-Versuche beim nächsten Start -> verhindert "Reconnect nicht möglich"
            deleteToken();

        } catch (Exception ex) {
            ex.printStackTrace();
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

    private void registerCallbacks() {

        // CONNECT_ACK
        client.setOnConnectAck(ack -> {
            System.out.println("[" + PROFILE + "] CONNECT_ACK identifierToken=" + ack.getIdentifierToken());
            this.identifierToken = ack.getIdentifierToken();

            // Optional speichern (falls ihr später serverseitig Reconnect stabil macht)
            saveToken(identifierToken);

            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setConnected(true);

                // Hinweis: das ist ein Token, keine Player-ID (Admin-Erkennung daher im LobbyPanel über localUsername lösen)
                lobbyPanel.setLocalPlayerId(identifierToken);
            });
        });

        // LOBBY_STATE
        client.setOnLobbyState(lobby -> SwingUtilities.invokeLater(() -> lobbyPanel.updateLobby(lobby)));

        // GAME_STARTED
        client.setOnGameStarted(started -> {
            System.out.println("[" + PROFILE + "] Received GAME_STARTED");

            Board clientBoard = BoardFactory.fromContracts(started.getInitialBoard());
            List<Player> clientPlayers = BoardFactory.convertPlayerStates(started.getPlayers());

            Player currentPlayer = resolveLocalPlayer(clientPlayers);

            SwingUtilities.invokeLater(() -> {
                frame.setTitle("Labyrinth Online (" + PROFILE + ") - Board " +
                        clientBoard.getHeight() + "x" + clientBoard.getWidth());

                ensureBoardPanel(clientBoard, currentPlayer, clientPlayers);
            });
        });

        // GAME_STATE_UPDATE
        client.setOnGameStateUpdate(update -> {
            System.out.println("[" + PROFILE + "] Received GAME_STATE_UPDATE");

            Board clientBoard = BoardFactory.fromContracts(update.getBoard());
            List<Player> clientPlayers = BoardFactory.convertPlayerStates(update.getPlayers());

            Player currentPlayer = resolveLocalPlayer(clientPlayers);

            SwingUtilities.invokeLater(() -> {
                // Wenn Boardgröße sich ändert, Panel neu erstellen
                if (boardPanel != null) {
                    Board old = boardPanel.getBoard();
                    if (old != null &&
                            (old.getHeight() != clientBoard.getHeight() ||
                                    old.getWidth() != clientBoard.getWidth())) {
                        mainPanel.remove(boardPanel);
                        boardPanel = null;
                    }
                }

                frame.setTitle("Labyrinth Online (" + PROFILE + ") - Board " +
                        clientBoard.getHeight() + "x" + clientBoard.getWidth());

                ensureBoardPanel(clientBoard, currentPlayer, clientPlayers);
            });
        });

        // Fehler → Dialog
        client.setOnErrorMessage(msg -> SwingUtilities.invokeLater(() -> {
            JOptionPane.showMessageDialog(frame, msg, "Fehler", JOptionPane.ERROR_MESSAGE);
        }));
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
}
