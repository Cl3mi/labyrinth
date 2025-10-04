package labyrinth.game;
import labyrinth.game.enums.Direction;
import labyrinth.game.models.*;

import javax.swing.*;
import java.awt.*;
import java.util.HashSet;
import java.util.Set;

/**
 * Panel that draws a Labyrinth {@link Board} with optional reachable tile highlighting.
 */
public class BoardPanel extends JPanel {

    private final Board board;
    private final Player player;
    private final Set<Tile> reachableTiles;

    /**
     * Creates a viewer panel for the given board and optional player.
     * If a player is provided, their reachable tiles will be highlighted.
     *
     * @param board  board to render
     * @param player optional player to highlight reachable tiles
     */
    public BoardPanel(Board board, Player player) {
        this.board = board;
        this.player = player;

        if (player != null) {
            this.reachableTiles = new HashSet<>(board.getReachableTiles(player));
        } else {
            this.reachableTiles = Set.of();
        }

        setBackground(Color.DARK_GRAY);
        setPreferredSize(new Dimension(700, 700));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        if (board == null) return;

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int cols = board.getWidth();
            int rows = board.getHeight();

            int w = getWidth();
            int h = getHeight();

            int cellW = w / cols;
            int cellH = h / rows;
            int size = Math.min(cellW, cellH);

            int xOffset = (w - size * cols) / 2;
            int yOffset = (h - size * rows) / 2;

            Tile[][] tiles = board.getTiles();

            Color corridorColor = new Color(235, 235, 220);
            Color wallColor = new Color(50, 50, 50);
            Color reachableBg = new Color(80, 160, 80);   // green highlight
            Color normalBg = new Color(60, 65, 75);
            Color playerColor = new Color(200, 80, 80);
            Color fixedBg = new Color(160, 160, 0);

            int corridorWidth = Math.max(4, size / 6);

            for (int row = 0; row < rows; row++) {
                for (int col = 0; col < cols; col++) {
                    Tile tile = tiles[row][col];
                    if (tile == null) continue;

                    int x = xOffset + col * size;
                    int y = yOffset + row * size;
                    int cx = x + size / 2;
                    int cy = y + size / 2;

                    // Tile background — highlight reachable ones
                    if (reachableTiles.contains(tile))
                        g2.setColor(reachableBg);
                    else
                        g2.setColor(normalBg);
                    g2.fillRect(x, y, size, size);

                    // Corridors
                    g2.setColor(corridorColor);
                    if (tile.getEntrances().contains(Direction.UP))
                        g2.fillRect(cx - corridorWidth / 2, y, corridorWidth, size / 2);
                    if (tile.getEntrances().contains(Direction.DOWN))
                        g2.fillRect(cx - corridorWidth / 2, cy, corridorWidth, size / 2);
                    if (tile.getEntrances().contains(Direction.LEFT))
                        g2.fillRect(x, cy - corridorWidth / 2, size / 2, corridorWidth);
                    if (tile.getEntrances().contains(Direction.RIGHT))
                        g2.fillRect(cx, cy - corridorWidth / 2, size / 2, corridorWidth);

                    // Center dot for tile
                    int dotSize = Math.max(4, corridorWidth);
                    if(tile.isFixed())  {
                        g2.setColor(fixedBg);
                    } else {
                        g2.setColor(wallColor);

                    }

                    g2.fillOval(cx - dotSize / 2, cy - dotSize / 2, dotSize, dotSize);


                    // Player position (red circle)
                    if (player != null && player.getCurrentPosition().getRow() == row &&
                            player.getCurrentPosition().getColumn() == col) {
                        int r = size / 3;
                        g2.setColor(playerColor);
                        g2.fillOval(cx - r / 2, cy - r / 2, r, r);
                    }

                    // Border
                    g2.setColor(Color.BLACK);
                    g2.drawRect(x, y, size, size);
                }
            }

        } finally {
            g2.dispose();
        }
    }
}
