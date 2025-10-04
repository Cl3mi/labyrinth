package labyrinth.game;

import labyrinth.game.factories.BoardFactory;
import labyrinth.game.models.*;

import javax.swing.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * Small launcher for the BoardPanel viewer.
 * Generates a random board using BoardFactory and displays it.
 */
public class LabyrinthViewer {

    public static void viewSwing(Board board, Player player) {
        SwingUtilities.invokeLater(() -> {
            var reachable = board.getReachableTiles(player);

            JFrame frame = new JFrame("Labyrinth Board Viewer - Player can reach " + reachable.size() + " Tiles");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

            BoardPanel panel = new BoardPanel(board, player);
            frame.add(panel);

            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);

            frame.addKeyListener(new KeyAdapter() {
                @Override
                public void keyPressed(KeyEvent e) {
                    if (e.getKeyChar() == 'r' || e.getKeyChar() == 'R') {
                        frame.dispose(); // Close old GUI
                        System.out.println("\n--- Resetting debug ---\n");
                        Testing.debug(); // Re-run debug
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
}
