package labyrinth.game;

import labyrinth.game.factories.BoardFactory;
import labyrinth.game.models.*;

import javax.swing.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.List;

/**
 * Small launcher for the BoardPanel viewer.
 * Generates a random board using BoardFactory and displays it.
 */
public class LabyrinthViewer {

    public static void viewSwing(Board board, List<Player> player) {
        SwingUtilities.invokeLater(() -> {
            var reachable = board.getReachableTiles(player.getFirst());

            JFrame frame = new JFrame("Labyrinth Board Viewer - Player can reach " + reachable.size() + " Tiles");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

            BoardPanel panel = new BoardPanel(board, player.getFirst(), player);
            frame.add(panel);

            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'q' || e.getKeyChar() == 'Q') {
                        frame.dispose(); // Close old GUI
                        System.out.println("\n--- Resetting debug ---\n");
                        Testing.debug(); // Re-run debug
                    }
                }
            });

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'p' || e.getKeyChar() == 'P') {
                        panel.switchPlayer();
                    }
                }
            });

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'r' || e.getKeyChar() == 'R') {
                        panel.rotateExtraTile();
                    }
                }
            });

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'f' || e.getKeyChar() == 'F') {
                        panel.toggleFreeRoam();
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
    public static void viewSwing(Board board) {
        viewSwing(board, null);
    }

    public static void viewSwing(Room room) {
        viewSwing(room.getBoard(), room.getPlayers());
    }
}
