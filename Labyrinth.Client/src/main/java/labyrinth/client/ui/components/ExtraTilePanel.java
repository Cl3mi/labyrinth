package labyrinth.client.ui.components;

import labyrinth.client.models.Board;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.client.ui.utils.ResourceLoader;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.TurnState;
import labyrinth.contracts.models.Direction;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.EnumSet;

public class ExtraTilePanel extends JPanel {

    private final ResourceLoader resources;
    private Board board;
    private TurnState currentTurnState;
    private int tileSize = 50;

    public ExtraTilePanel() {
        this.resources = ResourceLoader.getInstance();
        setOpaque(false);
    }

    public void setBoard(Board board) {
        this.board = board;
        repaint();
    }

    public void setCurrentTurnState(TurnState turnState) {
        this.currentTurnState = turnState;
        repaint();
    }

    public void setTileSize(int size) {
        this.tileSize = size;
        repaint();
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);

        // Center content
        int x = (getWidth() - tileSize) / 2;
        int y = (getHeight() - tileSize) / 2 - 10; // slightly up to make room for text below

        // Scale font size based on tile size
        int labelFontSize = Math.max(10, Math.min(14, tileSize / 7));
        int hintFontSize = Math.max(8, Math.min(10, tileSize / 9));

        // Label above the tile
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, labelFontSize));
        g2.setColor(ThemeManager.getInstance().getTextLight());
        FontMetrics fm = g2.getFontMetrics();
        String label = "Schiebekarte";
        g2.drawString(label, x + (tileSize - fm.stringWidth(label)) / 2, y - 8);

        if (board != null) {
            Tile extraTile = board.getExtraTile();
            if (extraTile != null) {
                drawTileAt(g2, extraTile, x, y, tileSize);
            } else {
                g2.setColor(new Color(255, 0, 0, 140));
                g2.drawRect(x, y, tileSize, tileSize);
                g2.setFont(new Font("Arial", Font.PLAIN, 12));
                g2.drawString("NULL", x + 5, y + 15);
            }
        }

        // Optional hint text that it can be rotated
        if (currentTurnState == TurnState.WAITING_FOR_PUSH) {
            g2.setFont(ResourceLoader.getCachedFont("Arial", Font.ITALIC, hintFontSize));
            g2.setColor(new Color(255, 255, 255, 200));
            String hint = "R/Q/E: Drehen";
            fm = g2.getFontMetrics();
            g2.drawString(hint, x + (tileSize - fm.stringWidth(hint)) / 2, y + tileSize + 15);
        }

        g2.dispose();
    }

    private void drawTileAt(Graphics2D g2, Tile tile, int x, int y, int size) {
        TileImageInfo info = getTileImageInfo(tile);
        boolean drewImage = false;

        if (info != null) {
            BufferedImage img = resources.getTileImage(info.type);
            if (img != null) {
                drawRotatedImage(g2, img, x, y, size, info.rotation);
                drewImage = true;
            }
        }

        if (!drewImage) {
            drawCorridorsFallback(g2, tile, x, y, size);
        }

        // Draw treasure
        if (tile.getTreasure() != null) {
            int cx = x + size / 2;
            int cy = y + size / 2;
            BufferedImage treasureImg = resources.getTreasureImage(tile.getTreasure().getName());
            if (treasureImg != null) {
                int imgSize = Math.min((int) (size * 0.45), 48);
                g2.drawImage(treasureImg, cx - imgSize / 2, cy - imgSize / 2 - 12, imgSize, imgSize, null);
            }
        }
        // Draw Bonus
        if (tile.getBonus() != null && tile.getTreasure() == null) {
            int cx = x + size / 2;
            int cy = y + size / 2;
            int imgSize = (int) (size * 0.5);
            BufferedImage bag = resources.getBonusBagImage();
            if (bag != null) {
                g2.drawImage(bag, cx - imgSize / 2, cy - imgSize / 2, imgSize, imgSize, null);
            }
        }
    }

    private void drawRotatedImage(Graphics2D g2, Image img, int x, int y, int size, int rotationDeg) {
        Graphics2D g = (Graphics2D) g2.create();
        try {
            g.setClip(new Rectangle(x, y, size, size));
            double scale = 1.2;
            int drawSize = (int) Math.round(size * scale);
            int drawX = x - (drawSize - size) / 2;
            int drawY = y - (drawSize - size) / 2;
            int cx = drawX + drawSize / 2;
            int cy = drawY + drawSize / 2;
            g.rotate(Math.toRadians(rotationDeg), cx, cy);
            g.drawImage(img, drawX, drawY, drawSize, drawSize, null);
        } finally {
            g.dispose();
        }
    }

    private static class TileImageInfo {
        final String type;
        final int rotation;

        TileImageInfo(String type, int rotation) {
            this.type = type;
            this.rotation = rotation;
        }
    }

    private TileImageInfo getTileImageInfo(Tile tile) {
        Direction[] entrancesArray = tile.getEntrances();
        if (entrancesArray == null || entrancesArray.length == 0)
            return null;
        EnumSet<Direction> dirs = EnumSet.noneOf(Direction.class);
        Collections.addAll(dirs, entrancesArray);

        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.DOWN))
                return new TileImageInfo("I", 0);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.RIGHT))
                return new TileImageInfo("I", 90);
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.RIGHT))
                return new TileImageInfo("L", 0);
            if (dirs.contains(Direction.RIGHT) && dirs.contains(Direction.DOWN))
                return new TileImageInfo("L", 90);
            if (dirs.contains(Direction.DOWN) && dirs.contains(Direction.LEFT))
                return new TileImageInfo("L", 180);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.UP))
                return new TileImageInfo("L", 270);
        }
        if (dirs.size() == 3) {
            if (!dirs.contains(Direction.DOWN))
                return new TileImageInfo("T", 0);
            if (!dirs.contains(Direction.LEFT))
                return new TileImageInfo("T", 90);
            if (!dirs.contains(Direction.UP))
                return new TileImageInfo("T", 180);
            if (!dirs.contains(Direction.RIGHT))
                return new TileImageInfo("T", 270);
        }
        return null;
    }

    private void drawCorridorsFallback(Graphics2D g2, Tile tile, int x, int y, int size) {
        int cx = x + size / 2;
        int cy = y + size / 2;
        int corridorWidth = Math.max(4, size / 6);
        Direction[] entrances = tile.getEntrances();
        if (entrances == null)
            return;

        g2.setColor(new Color(235, 235, 220));
        for (Direction d : entrances) {
            switch (d) {
                case UP -> g2.fillRect(cx - corridorWidth / 2, y, corridorWidth, size / 2);
                case DOWN -> g2.fillRect(cx - corridorWidth / 2, cy, corridorWidth, size / 2);
                case LEFT -> g2.fillRect(x, cy - corridorWidth / 2, size / 2, corridorWidth);
                case RIGHT -> g2.fillRect(cx, cy - corridorWidth / 2, size / 2, corridorWidth);
            }
        }
    }
}
