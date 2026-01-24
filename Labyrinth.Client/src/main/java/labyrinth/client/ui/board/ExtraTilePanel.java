package labyrinth.client.ui.board;

import labyrinth.client.models.extensions.TreasureUtils;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import labyrinth.contracts.models.TurnState;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

/**
 * Panel displaying the extra tile that can be pushed into the board.
 * Separated from BoardPanel for independent repaint when rotating.
 */
public class ExtraTilePanel extends JPanel {

    private static final int TILE_SIZE = 80;
    private static final Color CORRIDOR_COLOR = new Color(180, 160, 130);
    private static final Color WALL_COLOR = new Color(60, 50, 40);

    private Tile extraTile;
    private TurnState turnState;

    private final Map<String, BufferedImage> tileImages = new HashMap<>();
    private final Map<Integer, BufferedImage> treasureImages = new HashMap<>();
    private BufferedImage bonusBagImage;

    public ExtraTilePanel() {
        setOpaque(false);
        setPreferredSize(new Dimension(TILE_SIZE + 20, TILE_SIZE + 50));
        loadImages();
    }

    private void loadImages() {
        String[] tileTypes = {"I", "L", "T"};
        for (String type : tileTypes) {
            try {
                var url = getClass().getResource("/images/tiles/" + type + "_tile.png");
                if (url != null) {
                    tileImages.put(type, ImageIO.read(url));
                }
            } catch (Exception e) {
                System.err.println("Failed to load tile image: " + type);
            }
        }

        Map<Integer, String> treasureMapping = Map.ofEntries(
                Map.entry(1, "Ghost"), Map.entry(2, "Dragon"), Map.entry(3, "Witch"),
                Map.entry(4, "Owl"), Map.entry(5, "Rat"), Map.entry(6, "Bug"),
                Map.entry(7, "Spider"), Map.entry(8, "Snake"), Map.entry(9, "Bat"),
                Map.entry(10, "Crown"), Map.entry(11, "Key"), Map.entry(12, "Treasure"),
                Map.entry(13, "Helmet"), Map.entry(14, "Book"), Map.entry(15, "Candle"),
                Map.entry(16, "Ring"), Map.entry(17, "Bag"), Map.entry(18, "Skull"),
                Map.entry(19, "Map"), Map.entry(20, "Sword"), Map.entry(21, "chalice"),
                Map.entry(22, "Diamond"), Map.entry(23, "Jug"), Map.entry(24, "Mouse")
        );

        for (var entry : treasureMapping.entrySet()) {
            try {
                var url = getClass().getResource("/images/tiles/" + entry.getValue() + ".png");
                if (url != null) {
                    treasureImages.put(entry.getKey(), ImageIO.read(url));
                }
            } catch (Exception e) {
                System.err.println("Failed to load treasure: " + entry.getValue());
            }
        }

        try {
            var url = getClass().getResource("/images/tiles/BonusBag.png");
            if (url != null) {
                bonusBagImage = ImageIO.read(url);
            }
        } catch (Exception e) {
            System.err.println("Failed to load bonus bag image");
        }
    }

    public void setExtraTile(Tile tile) {
        this.extraTile = tile;
        repaint();
    }

    public void setTurnState(TurnState state) {
        this.turnState = state;
        repaint();
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);

            int x = (getWidth() - TILE_SIZE) / 2;
            int y = 20;

            g2.setFont(FontManager.getFontForSize(12f, Font.BOLD));
            g2.setColor(ThemeManager.getInstance().getTextLight());
            String label = "Schiebekarte";
            FontMetrics fm = g2.getFontMetrics();
            g2.drawString(label, (getWidth() - fm.stringWidth(label)) / 2, y - 5);

            if (extraTile == null) {
                g2.setColor(new Color(255, 0, 0, 140));
                g2.drawRect(x, y, TILE_SIZE, TILE_SIZE);
                g2.setFont(FontManager.getBodyMedium());
                g2.drawString("NULL", x + 5, y + 15);
                return;
            }

            drawTile(g2, extraTile, x, y);

            if (turnState == TurnState.WAITING_FOR_PUSH) {
                g2.setFont(FontManager.getFontForSize(10f, Font.ITALIC));
                g2.setColor(new Color(255, 255, 255, 200));
                String hint = "R/Q/E: Drehen";
                fm = g2.getFontMetrics();
                g2.drawString(hint, (getWidth() - fm.stringWidth(hint)) / 2, y + TILE_SIZE + 15);
            }

        } finally {
            g2.dispose();
        }
    }

    private static class TileInfo {
        final String type;
        final int rotation;

        TileInfo(String type, int rotation) {
            this.type = type;
            this.rotation = rotation;
        }
    }

    private TileInfo getTileInfo(Tile tile) {
        Direction[] entrancesArray = tile.getEntrances();
        if (entrancesArray == null || entrancesArray.length == 0) {
            return null;
        }

        EnumSet<Direction> dirs = EnumSet.noneOf(Direction.class);
        Collections.addAll(dirs, entrancesArray);

        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.DOWN)) return new TileInfo("I", 0);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.RIGHT)) return new TileInfo("I", 90);
        }

        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.RIGHT)) return new TileInfo("L", 0);
            if (dirs.contains(Direction.RIGHT) && dirs.contains(Direction.DOWN)) return new TileInfo("L", 90);
            if (dirs.contains(Direction.DOWN) && dirs.contains(Direction.LEFT)) return new TileInfo("L", 180);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.UP)) return new TileInfo("L", 270);
        }

        if (dirs.size() == 3) {
            if (!dirs.contains(Direction.DOWN)) return new TileInfo("T", 0);
            if (!dirs.contains(Direction.LEFT)) return new TileInfo("T", 90);
            if (!dirs.contains(Direction.UP)) return new TileInfo("T", 180);
            if (!dirs.contains(Direction.RIGHT)) return new TileInfo("T", 270);
        }

        return null;
    }

    private void drawTile(Graphics2D g2, Tile tile, int x, int y) {
        TileInfo info = getTileInfo(tile);

        if (info != null) {
            BufferedImage tileImg = tileImages.get(info.type);
            if (tileImg != null) {
                BufferedImage rotatedImg = rotateImage(tileImg, info.rotation);
                g2.drawImage(rotatedImg, x, y, TILE_SIZE, TILE_SIZE, null);
            } else {
                drawCorridorsFallback(g2, tile, x, y);
            }
        } else {
            drawCorridorsFallback(g2, tile, x, y);
        }

        if (tile.getTreasure() != null) {
            drawTreasure(g2, tile.getTreasure(), x + TILE_SIZE / 2, y + TILE_SIZE / 2);
        } else if (tile.getBonus() != null) {
            drawBonus(g2, tile.getBonus(), x + TILE_SIZE / 2, y + TILE_SIZE / 2);
        }
    }

    private BufferedImage rotateImage(BufferedImage img, int rotation) {
        if (rotation == 0) return img;

        double angle = Math.toRadians(rotation);
        int w = img.getWidth();
        int h = img.getHeight();

        BufferedImage rotated = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = rotated.createGraphics();
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);

        AffineTransform at = new AffineTransform();
        at.translate(w / 2.0, h / 2.0);
        at.rotate(angle);
        at.translate(-w / 2.0, -h / 2.0);
        g2.setTransform(at);
        g2.drawImage(img, 0, 0, null);
        g2.dispose();

        return rotated;
    }

    private boolean hasEntrance(Tile tile, Direction dir) {
        Direction[] entrances = tile.getEntrances();
        if (entrances == null) return false;
        for (Direction d : entrances) {
            if (d == dir) return true;
        }
        return false;
    }

    private void drawCorridorsFallback(Graphics2D g2, Tile tile, int x, int y) {
        g2.setColor(WALL_COLOR);
        g2.fillRect(x, y, TILE_SIZE, TILE_SIZE);

        g2.setColor(CORRIDOR_COLOR);
        int corridorWidth = TILE_SIZE / 3;
        int cx = x + TILE_SIZE / 2;
        int cy = y + TILE_SIZE / 2;

        g2.fillRect(cx - corridorWidth / 2, cy - corridorWidth / 2, corridorWidth, corridorWidth);

        if (hasEntrance(tile, Direction.UP)) {
            g2.fillRect(cx - corridorWidth / 2, y, corridorWidth, TILE_SIZE / 2);
        }
        if (hasEntrance(tile, Direction.DOWN)) {
            g2.fillRect(cx - corridorWidth / 2, cy, corridorWidth, TILE_SIZE / 2);
        }
        if (hasEntrance(tile, Direction.LEFT)) {
            g2.fillRect(x, cy - corridorWidth / 2, TILE_SIZE / 2, corridorWidth);
        }
        if (hasEntrance(tile, Direction.RIGHT)) {
            g2.fillRect(cx, cy - corridorWidth / 2, TILE_SIZE / 2, corridorWidth);
        }
    }

    private void drawTreasure(Graphics2D g2, Treasure treasure, int cx, int cy) {
        BufferedImage img = treasureImages.get(treasure.getId());
        int imgSize = (int) (TILE_SIZE * 0.45);

        if (img != null) {
            g2.drawImage(img, cx - imgSize / 2, cy - imgSize / 2 - 8, imgSize, imgSize, null);
        } else {
            g2.setColor(new Color(180, 140, 70, 200));
            g2.fillOval(cx - imgSize / 2, cy - imgSize / 2, imgSize, imgSize);
        }

        g2.setFont(FontManager.getBodyTiny());
        g2.setColor(new Color(255, 255, 255, 220));
        String name = TreasureUtils.getLocalName(treasure.getId());
        if (name.length() > 10) name = name.substring(0, 9) + "...";
        FontMetrics fm = g2.getFontMetrics();
        g2.drawString(name, cx - fm.stringWidth(name) / 2, cy + imgSize / 2 + 5);
    }

    private void drawBonus(Graphics2D g2, BonusType bonus, int cx, int cy) {
        int imgSize = (int) (TILE_SIZE * 0.5);

        if (bonusBagImage != null) {
            g2.drawImage(bonusBagImage, cx - imgSize / 2, cy - imgSize / 2, imgSize, imgSize, null);
        } else {
            g2.setColor(new Color(255, 215, 0, 200));
            g2.fillOval(cx - imgSize / 2, cy - imgSize / 2, imgSize, imgSize);

            g2.setFont(FontManager.getFontForSize(imgSize / 3f, Font.BOLD));
            g2.setColor(Color.BLACK);
            String label = switch (bonus) {
                case BEAM -> "T";
                case SWAP -> "S";
                case PUSH_FIXED -> "F";
                case PUSH_TWICE -> "2";
            };
            FontMetrics fm = g2.getFontMetrics();
            g2.drawString(label, cx - fm.stringWidth(label) / 2, cy + fm.getAscent() / 3);
        }
    }
}
