package labyrinth.client.models;

import labyrinth.client.BoardPanel;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.ui.LobbyPanel;

import labyrinth.contracts.models.BoardSize;
import labyrinth.contracts.models.GameStateUpdateEventPayload;
import labyrinth.contracts.models.PlayerState;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import java.util.List;

public class LabyrinthApplication {

    private GameClient client;
    private JFrame frame;

    private JPanel mainPanel;    // CardLayout: "lobby" / "game"
    private LobbyPanel lobbyPanel;
    private BoardPanel boardPanel;

    private String playerId;

    public void start() throws Exception {
        // 1. WebSocket verbinden
        client = new GameClient(new URI("ws://localhost:8080/game"));
        registerCallbacks();
        client.connect();

        // 2. UI vorbereiten
        frame = new JFrame("Labyrinth Online");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(1400, 900);
        frame.setLocationRelativeTo(null);

        mainPanel = new JPanel(new CardLayout());

        // wir kennen playerId noch nicht → null übergeben
        lobbyPanel = new LobbyPanel(client, null);
        mainPanel.add(lobbyPanel, "lobby");

        frame.setContentPane(mainPanel);
        frame.setVisible(true);

        // 3. Username abfragen & CONNECT senden
        String username = JOptionPane.showInputDialog(frame, "Bitte Username eingeben:");
        if (username == null || username.isBlank()) {
            username = "Player";
        }
        client.sendConnect(username);
    }

    // --------------------------------------------------------------------
    // Ansicht umschalten / BoardPanel aufbauen
    // --------------------------------------------------------------------

    private void switchToGameView() {
        CardLayout cl = (CardLayout) mainPanel.getLayout();
        cl.show(mainPanel, "game");
    }

    private void ensureBoardPanel(Board board, Player currentPlayer, List<Player> allPlayers) {
        if (boardPanel == null) {
            boardPanel = new BoardPanel(board, currentPlayer, allPlayers);
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
    // Callbacks für GameClient
    // --------------------------------------------------------------------

    private void registerCallbacks() {

        // CONNECT_ACK → eigene PlayerId merken und Lobby als "verbunden" markieren
        client.setOnConnectAck(ack -> {
            System.out.println("Connected as " + ack.getPlayerId());
            this.playerId = ack.getPlayerId();

            SwingUtilities.invokeLater(() -> {
                lobbyPanel.setConnected(true);
                lobbyPanel.setLocalPlayerId(playerId);
            });
        });

        // LOBBY_STATE → Spielerliste im LobbyPanel aktualisieren
        client.setOnLobbyState(lobby -> {
            SwingUtilities.invokeLater(() -> lobbyPanel.updateLobby(lobby));
        });

        // GAME_STATE_UPDATE → Board + Spieler mappen und BoardPanel anzeigen
        client.setOnGameStateUpdate(update -> {
            System.out.println("Received game state update");

            // 1. Board aus Contracts → Client-Board
            Board clientBoard = BoardFactory.fromContracts(update.getBoard());

            // 2. Players aus PlayerState[] → Client-Player-Liste
            List<Player> clientPlayers = BoardFactory.convertPlayerStates(update.getPlayers());

            // 3. aktuellen Spieler finden
            Player currentPlayer = null;
            if (update.getCurrentPlayerId() != null) {
                for (Player p : clientPlayers) {
                    if (p.getId().equals(update.getCurrentPlayerId())) {
                        currentPlayer = p;
                        break;
                    }
                }
            }

            Player finalCurrentPlayer = currentPlayer;
            SwingUtilities.invokeLater(() ->
                    ensureBoardPanel(clientBoard, finalCurrentPlayer, clientPlayers)
            );
        });

        // Fehler → Dialog
        client.setOnErrorMessage(msg -> SwingUtilities.invokeLater(() ->
                JOptionPane.showMessageDialog(
                        frame,
                        msg,
                        "Fehler",
                        JOptionPane.ERROR_MESSAGE
                )
        ));
    }
}
