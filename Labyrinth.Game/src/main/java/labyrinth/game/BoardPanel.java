package labyrinth.game;
import labyrinth.game.enums.Direction;
import labyrinth.game.models.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Panel that draws a Labyrinth {@link Board} with optional reachable tile highlighting
 * and clickable arrows to shift rows and columns.
 */
public class BoardPanel extends JPanel {

    private final Board board;
    private Player currentPlayer;
    private final List<Player> players;
    private int currentPlayerIndex = 0;
    private final Set<Tile> reachableTiles;
    private final List<ArrowButton> arrowButtons = new ArrayList<>();

    private int xOffset;
    private int yOffset;
    private int size;
    private int arrowSize = 30;

    /**
     * Represents a clickable arrow button for shifting rows/columns.
     */
    private static class ArrowButton {
        Rectangle bounds;
        Direction direction;
        int index; // row or column index
        boolean isRow; // true for row shift, false for column shift
        Path2D.Double arrowShape;

        ArrowButton(Rectangle bounds, Direction direction, int index, boolean isRow) {
            this.bounds = bounds;
            this.direction = direction;
            this.index = index;
            this.isRow = isRow;
            this.arrowShape = createArrowShape(bounds, direction);
        }

        private Path2D.Double createArrowShape(Rectangle bounds, Direction dir) {
            Path2D.Double arrow = new Path2D.Double();
            int cx = bounds.x + bounds.width / 2;
            int cy = bounds.y + bounds.height / 2;
            int size = Math.min(bounds.width, bounds.height) / 2;

            switch (dir) {
                case LEFT:
                    arrow.moveTo(cx + size/2, cy - size/2);
                    arrow.lineTo(cx - size/2, cy);
                    arrow.lineTo(cx + size/2, cy + size/2);
                    break;
                case RIGHT:
                    arrow.moveTo(cx - size/2, cy - size/2);
                    arrow.lineTo(cx + size/2, cy);
                    arrow.lineTo(cx - size/2, cy + size/2);
                    break;
                case UP:
                    arrow.moveTo(cx - size/2, cy + size/2);
                    arrow.lineTo(cx, cy - size/2);
                    arrow.lineTo(cx + size/2, cy + size/2);
                    break;
                case DOWN:
                    arrow.moveTo(cx - size/2, cy - size/2);
                    arrow.lineTo(cx, cy + size/2);
                    arrow.lineTo(cx + size/2, cy - size/2);
                    break;
            }
            arrow.closePath();
            return arrow;
        }

        boolean contains(Point p) {
            return bounds.contains(p);
        }
    }

    /**
     * Creates a viewer panel for the given board and optional player.
     * If a player is provided, their reachable tiles will be highlighted.
     *
     * @param board  board to render
     * @param currentPlayer optional player to highlight reachable tiles
     */
    public BoardPanel(Board board, Player currentPlayer) {
        this(board, currentPlayer, currentPlayer != null ? List.of(currentPlayer) : List.of());
    }

    /**
     * Creates a viewer panel for the given board and list of players.
     * If a player is provided, their reachable tiles will be highlighted.
     *
     * @param board  board to render
     * @param currentPlayer optional player to highlight reachable tiles
     * @param players list of all players to display
     */
    public BoardPanel(Board board, Player currentPlayer, List<Player> players) {
        this.board = board;
        this.currentPlayer = currentPlayer;
        this.players = players != null ? players : List.of();

        if (currentPlayer != null) {
            this.reachableTiles = new HashSet<>(board.getReachableTiles(currentPlayer));
        } else {
            this.reachableTiles = Set.of();
        }

        setBackground(Color.DARK_GRAY);
        setPreferredSize(new Dimension(800, 800));

        // Add mouse listener for arrow clicks
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                handleArrowClick(e.getPoint());
            }
        });
    }

    private void handleArrowClick(Point p) {
        for (ArrowButton arrow : arrowButtons) {
            if (arrow.contains(p)) {
                if (arrow.isRow) {
                    board.shiftRow(arrow.index, arrow.direction);
                } else {
                    board.shiftColumn(arrow.index, arrow.direction);
                }

                // Recalculate reachable tiles if player exists
                reachableTiles.clear();
                if (currentPlayer != null) {
                    Set<Tile> newReachable = board.getReachableTiles(currentPlayer);
                    System.out.println("=== Debug: Reachable Tiles ===");
                    System.out.println("Player position: (" + currentPlayer.getCurrentPosition().getRow() + ", " + currentPlayer.getCurrentPosition().getColumn() + ")");
                    System.out.println("Reachable tiles returned: " + (newReachable != null ? newReachable.size() : "null"));
                    if (newReachable != null) {
                        reachableTiles.addAll(newReachable);
                    }
                    System.out.println("Total in reachableTiles set: " + reachableTiles.size());
                    System.out.println("==============================");
                }

                repaint();
                break;
            }
        }
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

            int w = getWidth() - 2 * arrowSize - 20; // Reserve space for arrows
            int h = getHeight() - 2 * arrowSize - 20;

            int cellW = w / cols;
            int cellH = h / rows;
            size = Math.min(cellW, cellH);

            xOffset = (getWidth() - size * cols) / 2;
            yOffset = (getHeight() - size * rows) / 2;

            // Clear arrow buttons and recreate them
            arrowButtons.clear();

            Tile[][] tiles = board.getTiles();

            Color corridorColor = new Color(235, 235, 220);
            Color wallColor = new Color(50, 50, 50);
            Color reachableBg = new Color(80, 160, 80);   // green highlight
            Color normalBg = new Color(100, 100, 100);
            Color fixedBg = new Color(160, 160, 0);
            Color arrowColor = new Color(70, 130, 180);
            Color arrowHoverColor = new Color(100, 160, 210);
            Color[] playerColors = {
                    new Color(200, 80, 80),
                    new Color(80, 180, 80),
                    new Color(80, 120, 200),
                    new Color(230, 200, 80)
            };

            int corridorWidth = Math.max(4, size / 6);

            // Draw tiles
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

                    if (tile.getTreasureCard() != null) {
                        String treasureName = tile.getTreasureCard().getTreasureName();
                        g2.setColor(Color.BLACK);
                        FontMetrics fm = g2.getFontMetrics();
                        int textWidth = fm.stringWidth(treasureName);
                        int textHeight = fm.getAscent();

                        g2.drawString(treasureName, cx - textWidth / 2, (cy + textHeight / 2) - 20);
                    }

                    // Draw coordinates in lower left corner
                    FontMetrics fm = g2.getFontMetrics();
                    {
                    String coords = "(" + row + "," + col + ")";
                    g2.setColor(Color.WHITE);
                    Font oldFont = g2.getFont();
                    g2.setFont(new Font("Arial", Font.PLAIN, 10));
                    g2.drawString(coords, x + 3, y + size - 3);
                    g2.setFont(oldFont);
                    }

                    // Player positions with different colors
                    for (int i = 0; i < players.size(); i++) {
                        Player p = players.get(i);
                        if (p != null && p.getCurrentPosition().getRow() == row &&
                                p.getCurrentPosition().getColumn() == col) {

                            Color playerColor = playerColors[i % playerColors.length]; // cycles colors if more players than colors

                            Font oldFont = g2.getFont();
                            g2.setFont(new Font("Arial", Font.BOLD, 30));
                            fm = g2.getFontMetrics();

                            String text = "P" + (i + 1); // show player number (P1, P2, ...)
                            int textWidth = fm.stringWidth(text);
                            int textHeight = fm.getAscent();

                            g2.setColor(playerColor);
                            g2.drawString(text, cx - textWidth / 2, cy + textHeight / 2 - fm.getDescent());

                            g2.setFont(oldFont);
                        }
                    }

                    // Border
                    g2.setColor(Color.BLACK);
                    g2.drawRect(x, y, size, size);
                }
            }

            // Draw arrow buttons
            g2.setColor(arrowColor);
            g2.setStroke(new BasicStroke(2));

            // Left arrows (shift rows right)
            for (int row = 0; row < rows; row++) {
                int y = yOffset + row * size;
                Rectangle bounds = new Rectangle(xOffset - arrowSize - 5, y + (size - arrowSize) / 2, arrowSize, arrowSize);
                ArrowButton arrow = new ArrowButton(bounds, Direction.RIGHT, row, true);
                arrowButtons.add(arrow);
                g2.fill(arrow.arrowShape);
                g2.draw(arrow.arrowShape);
            }

            // Right arrows (shift rows left)
            for (int row = 0; row < rows; row++) {
                int y = yOffset + row * size;
                Rectangle bounds = new Rectangle(xOffset + cols * size + 5, y + (size - arrowSize) / 2, arrowSize, arrowSize);
                ArrowButton arrow = new ArrowButton(bounds, Direction.LEFT, row, true);
                arrowButtons.add(arrow);
                g2.fill(arrow.arrowShape);
                g2.draw(arrow.arrowShape);
            }

            // Top arrows (shift columns down)
            for (int col = 0; col < cols; col++) {
                int x = xOffset + col * size;
                Rectangle bounds = new Rectangle(x + (size - arrowSize) / 2, yOffset - arrowSize - 5, arrowSize, arrowSize);
                ArrowButton arrow = new ArrowButton(bounds, Direction.DOWN, col, false);
                arrowButtons.add(arrow);
                g2.fill(arrow.arrowShape);
                g2.draw(arrow.arrowShape);
            }

            // Bottom arrows (shift columns up)
            for (int col = 0; col < cols; col++) {
                int x = xOffset + col * size;
                Rectangle bounds = new Rectangle(x + (size - arrowSize) / 2, yOffset + rows * size + 5, arrowSize, arrowSize);
                ArrowButton arrow = new ArrowButton(bounds, Direction.UP, col, false);
                arrowButtons.add(arrow);
                g2.fill(arrow.arrowShape);
                g2.draw(arrow.arrowShape);
            }

        } finally {
            g2.dispose();
        }
    }

    public void switchPlayer(){
        currentPlayerIndex++;
        if (currentPlayerIndex >= players.size()) {
            currentPlayerIndex = 0;
        }
        System.out.println("Switching player " + currentPlayerIndex);
        currentPlayer = players.get(currentPlayerIndex);
        this.reachableTiles.clear();
        this.reachableTiles.addAll(board.getReachableTiles(currentPlayer));
        repaint();
    }
}