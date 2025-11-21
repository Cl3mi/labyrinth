package labyrinth.server.game;

import labyrinth.server.game.models.*;

import javax.swing.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.List;

/**
 * Small launcher for the BoardPanel viewer.
 * Generates a random board using BoardFactory and displays it.
 */
public class LabyrinthViewer {
    private static BoardPanel currentPanel;
    public static void viewSwing(Game game, List<Player> player) {
        SwingUtilities.invokeLater(() -> {
            var reachable = game.getBoard().getReachableTiles(player.getFirst());

            JFrame frame = new JFrame("Labyrinth Board Viewer - Player can reach " + reachable.size() + " Tiles");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

            currentPanel = new BoardPanel(game, player.getFirst(), player);
            frame.add(currentPanel);

            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'q' || e.getKeyChar() == 'Q') {
                        frame.dispose(); // Close old GUI
                        System.out.println("\n--- Resetting debug ---\n");
                        Testing.simulateGameStart(); // Re-run debug
                    }
                }
            });

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'p' || e.getKeyChar() == 'P') {
                        currentPanel.switchPlayer();
                    }
                }
            });

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'r' || e.getKeyChar() == 'R') {
                        currentPanel.rotateExtraTile();
                    }
                }
            });

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'f' || e.getKeyChar() == 'F') {
                        currentPanel.toggleFreeRoam();
                    }
                }
            });

            frame.setFocusable(true);
            frame.requestFocusInWindow();
        });
    }

    /**
     * Convenience overload for showing the board without a player.
     */
    public static void viewSwing(Game game) {
        viewSwing(game, game.getPlayers());
    }

    public static void repaintView() {
        if (currentPanel != null) {
            currentPanel.updateReachableTilesAndRepaintAuto();
        }
    }
}
