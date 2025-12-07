package labyrinth.client.models;

import labyrinth.client.BoardPanel;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.factories.BoardFactory;
import labyrinth.contracts.models.*;

import javax.swing.*;
import java.net.URI;
import java.util.List;

public class LabyrinthApplication {

    private GameClient client;
    private JFrame frame;
    private BoardPanel boardPanel;
    private String playerId;

    public void start() throws Exception {
        // 1. WebSocket verbinden
        client = new GameClient(new URI("ws://localhost:8080/game"));
        registerCallbacks();
        client.connect();

        // 2. UI vorbereiten
        frame = new JFrame("Labyrinth");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(1400, 900);
        frame.setLocationRelativeTo(null);

        // 3. Spielername auswählen
        String username = JOptionPane.showInputDialog(frame, "Bitte Username eingeben:");
        client.sendConnect(username);

        frame.setVisible(true);
    }

    private void registerCallbacks() {

        client.setOnConnectAck(ack -> {
            System.out.println("Connected as " + ack.getPlayerId());
            this.playerId = ack.getPlayerId();
        });

        client.setOnLobbyState(lobby -> {
            System.out.println("Lobby has " + lobby.getPlayers().length + " players.");
            // Hier könntest du eine Lobby anzeigen.
        });

        client.setOnGameStateUpdate(update -> {

            Board clientBoard = BoardFactory.fromContracts(update.getBoard());
            List<Player> clientPlayers = BoardFactory.convertPlayerStates(update.getPlayers());

            SwingUtilities.invokeLater(() -> {

                Player currentPlayer = null;
                for (Player p : clientPlayers) {
                    if (p.getId().equals(update.getCurrentPlayerId())) {
                        currentPlayer = p;
                        break;
                    }
                }

                if (boardPanel == null) {
                    boardPanel = new BoardPanel(clientBoard, currentPlayer, clientPlayers);
                    frame.setContentPane(boardPanel);
                    frame.revalidate();
                } else {
                    boardPanel.setBoard(clientBoard);
                    boardPanel.setPlayers(clientPlayers);
                    boardPanel.setCurrentPlayer(currentPlayer);
                    boardPanel.repaint();
                }
            });
        });

        client.setOnErrorMessage(msg -> {
            JOptionPane.showMessageDialog(frame, msg, "Fehler", JOptionPane.ERROR_MESSAGE);
        });
    }
}
