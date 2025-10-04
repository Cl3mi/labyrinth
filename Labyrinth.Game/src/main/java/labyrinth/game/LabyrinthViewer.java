package labyrinth.game;

import labyrinth.game.factories.BoardFactory;
import labyrinth.game.models.*;

import javax.swing.*;

/**
 * Small launcher for the BoardPanel viewer.
 * Generates a random board using BoardFactory and displays it.
 */
public class LabyrinthViewer {

    public static void viewSwing(Board board, Player player) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Labyrinth Board Viewer");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

            BoardPanel panel = new BoardPanel(board, player);
            frame.add(panel);

            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }

    /**
     * Convenience overload for showing the board without a player.
     */
    public static void viewSwing(Board board) {
        viewSwing(board, null);
    }
}
