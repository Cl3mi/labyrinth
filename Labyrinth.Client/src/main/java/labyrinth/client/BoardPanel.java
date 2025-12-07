package labyrinth.client;

import labyrinth.client.enums.Direction;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.Tile;
import labyrinth.client.models.TreasureCard;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;
import java.util.*;
import java.util.List;
import labyrinth.client.audio.AudioPlayer;

/**
 * Panel that draws a Labyrinth {@link Board} with optional reachable tile highlighting
 * and clickable arrows to shift rows and columns.
 */
public class BoardPanel extends JPanel {

    // --- Drawing & Layout Constants ---
    private static final int CARD_PANEL_WIDTH = 40;
    private static final int PANEL_PADDING = 20;
    private static final int ARROW_MARGIN = 5;
    private static final Font DEBUG_INFO_FONT = new Font("Arial", Font.BOLD, 16);
    private static final Font PLAYER_MARKER_FONT = new Font("Arial", Font.BOLD, 30); // nur Fallback
    private static final Font COORDINATE_FONT = new Font("Arial", Font.PLAIN, 10);
    private static final Color BACKGROUND_COLOR = Color.DARK_GRAY;
    private static final Color CORRIDOR_COLOR = new Color(235, 235, 220);
    private static final Color WALL_COLOR = new Color(50, 50, 50);
    private static final Color REACHABLE_TILE_HIGHLIGHT_COLOR = new Color(80, 160, 80);
    private static final Color NORMAL_TILE_BACKGROUND_COLOR = new Color(100, 100, 100);
    private static final Color FIXED_TILE_BACKGROUND_COLOR = new Color(160, 160, 0);
    private static final Color ARROW_COLOR = new Color(70, 130, 180);
    private static final Color ARROW_COLOR_HOVER = new Color(120, 180, 230);
    private static final Color[] PLAYER_COLORS = {
            new Color(200, 80, 80), new Color(80, 180, 80),
            new Color(80, 120, 200), new Color(230, 200, 80)
    };

    // --- Images ---
    private final Map<String, BufferedImage> tileImages = new HashMap<>(); // "I","L","T"
    private final List<BufferedImage> playerIcons = new ArrayList<>();     // index = player
    private Image backgroundImage;
    private static final int TILE_PADDING = 0;

    // --- Class Fields ---
    private Board board;
    private Player currentPlayer;
    private List<Player> players;
    private int currentPlayerIndex = 0;
    private final Set<Tile> reachableTiles;
    private final List<ArrowButton> arrowButtons = new ArrayList<>();
    private ArrowButton hoveredArrow = null;

    // --- Dynamic Layout Fields ---
    private int xOffset;
    private int yOffset;
    private int size;
    private int arrowSize = 30;

    // --- Audio ---
    private AudioPlayer backgroundMusic;

    /**
     * Creates a viewer panel for the given board and list of players.
     */
    public BoardPanel(Board board, Player currentPlayer, List<Player> players) {
        this.board = board;
        this.currentPlayer = currentPlayer;
        this.players = players != null ? players : List.of();
        this.reachableTiles = new HashSet<>();

        loadBackgroundImage();
        loadTileImages();
        loadPlayerIcons();

        updateReachableTilesAndRepaint();
        setBackground(BACKGROUND_COLOR);
        setPreferredSize(new Dimension(1400, 800));
        setupMouseListener();

        backgroundMusic = new AudioPlayer("/sounds/06-Kokiri-Forest.wav");
        backgroundMusic.loop();
    }

    /**
     * Creates a viewer panel for the given board and optional player.
     */
    public BoardPanel(Board board, Player currentPlayer) {
        this(board, currentPlayer, currentPlayer != null ? List.of(currentPlayer) : List.of());
    }

    // =================================================================================
    // IMAGE LOADING
    // =================================================================================

    private void loadBackgroundImage() {
        try {
            var url = getClass().getResource("/images/ui/background.jpg");
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
            } else {
                System.err.println("Background image not found: /images/ui/background.jpg");
            }
        } catch (Exception e) {
            System.err.println("Error loading background image: " + e.getMessage());
        }
    }

    private BufferedImage loadImage(String path) {
        try {
            var url = getClass().getResource(path);
            if (url == null) {
                System.err.println("Image not found on classpath: " + path);
                return null;
            }
            return javax.imageio.ImageIO.read(url);
        } catch (Exception e) {
            System.err.println("Error loading image: " + path + " -> " + e.getMessage());
            return null;
        }
    }

    private void loadTileImages() {
        tileImages.put("I", loadImage("/images/tiles/tile_I.png"));
        tileImages.put("L", loadImage("/images/tiles/tile_L.png"));
        tileImages.put("T", loadImage("/images/tiles/tile_princess.png"));
        // Optional Kreuz:
        // tileImages.put("X", loadImage("/images/tiles/tile_X.png"));
    }

    private void loadPlayerIcons() {
        // Versuche bis zu 4 Icons zu laden, fallback auf Text wenn null
        for (int i = 1; i <= 4; i++) {
            BufferedImage img = loadImage("/images/players/player" + i + ".png");
            playerIcons.add(img); // img kann null sein; dann wird Text verwendet
        }
    }

    // =================================================================================
    // TILE IMAGE INFO & DRAWING
    // =================================================================================

    private static class TileImageInfo {
        final String type;   // "I","L","T","X"
        final int rotation;  // 0,90,180,270

        TileImageInfo(String type, int rotation) {
            this.type = type;
            this.rotation = rotation;
        }
    }

    /**
     * Leitet aus den Tile-Entrances die Tile-Form (I/L/T) und Rotation ab.
     * Nutzt dein client-Model: Tile.getEntrances() -> Set<Direction>.
     */
    private TileImageInfo getTileImageInfo(Tile tile) {
        Set<Direction> dirs = tile.getEntrances();
        if (dirs == null || dirs.isEmpty()) {
            return null;
        }

        // I-Tiles: zwei gegenüberliegende Eingänge
        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.DOWN)) {
                return new TileImageInfo("I", 0);
            }
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.RIGHT)) {
                return new TileImageInfo("I", 90);
            }
        }

        // L-Tiles: zwei benachbarte Eingänge
        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.RIGHT)) {
                return new TileImageInfo("L", 0);
            }
            if (dirs.contains(Direction.RIGHT) && dirs.contains(Direction.DOWN)) {
                return new TileImageInfo("L", 90);
            }
            if (dirs.contains(Direction.DOWN) && dirs.contains(Direction.LEFT)) {
                return new TileImageInfo("L", 180);
            }
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.UP)) {
                return new TileImageInfo("L", 270);
            }
        }

        // T-Tiles: drei Eingänge
        if (dirs.size() == 3) {
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.UP) && dirs.contains(Direction.RIGHT)) {
                return new TileImageInfo("T", 0);
            }
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.RIGHT) && dirs.contains(Direction.DOWN)) {
                return new TileImageInfo("T", 90);
            }
            if (dirs.contains(Direction.RIGHT) && dirs.contains(Direction.DOWN) && dirs.contains(Direction.LEFT)) {
                return new TileImageInfo("T", 180);
            }
            if (dirs.contains(Direction.DOWN) && dirs.contains(Direction.LEFT) && dirs.contains(Direction.UP)) {
                return new TileImageInfo("T", 270);
            }
        }

        return null;
    }

    private void drawRotatedImage(Graphics2D g2, Image img, int x, int y, int rotationDeg) {
        if (img == null) return;

        Graphics2D g = (Graphics2D) g2.create();

        // Nur innerhalb dieses Tiles zeichnen
        Shape oldClip = g.getClip();
        g.setClip(new Rectangle(x, y, size, size));

        // Etwas größer zeichnen als das Tile (z.B. 1.2 = 20% größer)
        double scale = 1.2;
        int drawSize = (int) Math.round(size * scale);

        // so zentrieren, dass es mittig im Tile liegt
        int drawX = x - (drawSize - size) / 2;
        int drawY = y - (drawSize - size) / 2;

        int cx = drawX + drawSize / 2;
        int cy = drawY + drawSize / 2;

        g.rotate(Math.toRadians(rotationDeg), cx, cy);
        g.drawImage(img, drawX, drawY, drawSize, drawSize, null);

        // Clip zurücksetzen
        g.setClip(oldClip);
        g.dispose();
    }

    private void drawCorridorsFallback(Graphics2D g2, Tile tile, int x, int y) {
        int cx = x + size / 2;
        int cy = y + size / 2;
        int corridorWidth = Math.max(4, size / 6);

        g2.setColor(CORRIDOR_COLOR);
        if (tile.getEntrances().contains(Direction.UP)) {
            g2.fillRect(cx - corridorWidth / 2, y, corridorWidth, size / 2);
        }
        if (tile.getEntrances().contains(Direction.DOWN)) {
            g2.fillRect(cx - corridorWidth / 2, cy, corridorWidth, size / 2);
        }
        if (tile.getEntrances().contains(Direction.LEFT)) {
            g2.fillRect(x, cy - corridorWidth / 2, size / 2, corridorWidth);
        }
        if (tile.getEntrances().contains(Direction.RIGHT)) {
            g2.fillRect(cx, cy - corridorWidth / 2, size / 2, corridorWidth);
        }

        int dotSize = Math.max(4, corridorWidth);
        g2.setColor(tile.isFixed() ? FIXED_TILE_BACKGROUND_COLOR : WALL_COLOR);
        g2.fillOval(cx - dotSize / 2, cy - dotSize / 2, dotSize, dotSize);
    }

    private void drawTileHighlight(Graphics2D g2, int x, int y) {
        g2.setColor(new Color(255, 255, 0, 120));
        g2.fillRoundRect(x - 4, y - 4, size + 8, size + 8, 15, 15);
    }

    // =================================================================================
    // MOUSE EVENT HANDLING
    // =================================================================================

    private void setupMouseListener() {
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                // Prioritize arrow clicks. If an arrow was clicked, the event is handled.
                if (handleArrowClick(e.getPoint())) {
                    return;
                }
                // Otherwise, check for a click on a game tile.
                handleTileClick(e.getPoint());
            }
        });

        addMouseMotionListener(new MouseMotionAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                hoveredArrow = null;
                for (ArrowButton arrow : arrowButtons) {
                    if (arrow.contains(e.getPoint())) {
                        hoveredArrow = arrow;
                        break;
                    }
                }
                repaint();
            }
        });
    }

    private boolean handleArrowClick(Point p) {
        for (ArrowButton arrow : arrowButtons) {
            if (arrow.contains(p)) {
                if (arrow.isRow) {
                    board.shiftRow(arrow.index, arrow.direction, currentPlayer);
                } else {
                    board.shiftColumn(arrow.index, arrow.direction, currentPlayer);
                }
                updateReachableTilesAndRepaint();
                return true;
            }
        }
        return false;
    }

    private void handleTileClick(Point p) {
        System.out.println("---------");
        System.out.println("Current player: " + currentPlayer);

        outer:
        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Rectangle tileRect = new Rectangle(xOffset + col * size, yOffset + row * size, size, size);
                if (tileRect.contains(p)) {
                    Tile clickedTile = board.getTiles()[row][col];
                    if (reachableTiles.contains(clickedTile)) {
                        boolean moved = board.movePlayerToTile(currentPlayer, row, col);
                        if (moved) {
                            updateReachableTilesAndRepaint();
                        }
                    }
                    break outer;
                }
            }
        }
        System.out.println("---------");
    }

    private void updateReachableTilesAndRepaint() {
        reachableTiles.clear();
        if (currentPlayer != null) {
            reachableTiles.addAll(board.getReachableTiles(currentPlayer));
        }
        repaint();
    }

    // =================================================================================
    // PAINTING LOGIC
    // =================================================================================

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        if (board == null) return;

        // Hintergrundbild
        if (backgroundImage != null) {
            g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
        }

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            calculateLayoutMetrics();
            drawBoardGrid(g2);
            createAndDrawArrowButtons(g2);
            drawExtraTile(g2);
            drawDebugInfo(g2);

            if (currentPlayer != null) {
                drawTreasureCards(g2, currentPlayer);
            }
        } finally {
            g2.dispose();
        }
    }

    private void calculateLayoutMetrics() {
        int w = getWidth() - 2 * arrowSize - PANEL_PADDING - CARD_PANEL_WIDTH;
        int h = getHeight() - 2 * arrowSize - PANEL_PADDING;
        size = Math.min(w / board.getWidth(), h / board.getHeight());

        xOffset = CARD_PANEL_WIDTH + (getWidth() - size * board.getWidth()) / 2;
        yOffset = (getHeight() - size * board.getHeight()) / 2;
    }

    private void drawBoardGrid(Graphics2D g2) {
        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Tile tile = board.getTiles()[row][col];
                if (tile != null) {
                    int x = xOffset + col * size;
                    int y = yOffset + row * size;
                    drawTileAt(g2, tile, x, y, row, col, true);
                    drawPlayersOnTile(g2, row, col);
                }
            }
        }
    }

    private void drawTileAt(Graphics2D g2, Tile tile, int x, int y, int row, int col, boolean drawDetails) {
        // Hintergrund für den Fall, dass kein Bild vorhanden ist

        // Bild + Rotation (jetzt mit Zoom+Clip)
        TileImageInfo info = getTileImageInfo(tile);
        boolean drewImage = false;
        if (info != null) {
            BufferedImage img = tileImages.get(info.type);
            if (img != null) {
                drawRotatedImage(g2, img, x, y, info.rotation);
                drewImage = true;
            }
        }

        // Fallback, falls kein Bild gefunden wurde
        if (!drewImage) {
            drawCorridorsFallback(g2, tile, x, y);
        }

        if (drawDetails) {
            if (tile.getTreasureCard() != null) {
                int cx = x + size / 2;
                int cy = y + size / 2;
                drawTreasureOnTile(g2, tile.getTreasureCard(), cx, cy);
            }
            drawCoordinates(g2, x, y, row, col);
        }

        // KEIN zusätzlicher schwarzer Rahmen mehr
        // g2.setColor(Color.BLACK);
        // g2.drawRect(x, y, size, size);
    }


    private void drawTreasureOnTile(Graphics2D g2, TreasureCard card, int centerX, int centerY) {
        g2.setColor(Color.BLACK);
        FontMetrics fm = g2.getFontMetrics();
        int textWidth = fm.stringWidth(card.getTreasureName());
        g2.drawString(card.getTreasureName(), centerX - textWidth / 2, (centerY + fm.getAscent() / 2) - 20);
    }

    private void drawCoordinates(Graphics2D g2, int x, int y, int row, int col) {
        String coords = "(" + row + "," + col + ")";
        g2.setColor(Color.WHITE);
        Font oldFont = g2.getFont();
        g2.setFont(COORDINATE_FONT);
        g2.drawString(coords, x + 3, y + size - 3);
        g2.setFont(oldFont);
    }

    private void drawPlayersOnTile(Graphics2D g2, int row, int col) {
        for (int i = 0; i < players.size(); i++) {
            Player p = players.get(i);
            if (p != null && p.getCurrentPosition() != null &&
                    p.getCurrentPosition().getRow() == row &&
                    p.getCurrentPosition().getColumn() == col) {

                int cx = xOffset + col * size + size / 2;
                int cy = yOffset + row * size + size / 2;

                BufferedImage icon = (i < playerIcons.size()) ? playerIcons.get(i) : null;
                if (icon != null) {
                    int iconSize = (int) (size * 0.6);
                    g2.drawImage(icon,
                            cx - iconSize / 2,
                            cy - iconSize / 2,
                            iconSize,
                            iconSize,
                            null);
                } else {
                    // Fallback: Text "P1" etc.
                    g2.setColor(PLAYER_COLORS[i % PLAYER_COLORS.length]);
                    Font oldFont = g2.getFont();
                    g2.setFont(PLAYER_MARKER_FONT);
                    FontMetrics fm = g2.getFontMetrics();
                    String text = "P" + (i + 1);
                    int textWidth = fm.stringWidth(text);
                    g2.drawString(text, cx - textWidth / 2, cy + fm.getAscent() / 2 - fm.getDescent());
                    g2.setFont(oldFont);
                }
            }
        }
    }

    private void createAndDrawArrowButtons(Graphics2D g2) {
        arrowButtons.clear();
        g2.setStroke(new BasicStroke(2));

        int rows = board.getHeight();
        int cols = board.getWidth();

        // ROW SHIFT — Nur jede 2. Zeile (Index 1,3,5,...)
        for (int row = 0; row < rows; row++) {

            if (row % 2 == 0) continue; // nur ungerade Zeilen erlauben

            int y = yOffset + row * size + (size - arrowSize) / 2;

            // LINKS vom Board: Pfeil zeigt NACH RECHTS (schiebt nach rechts rein)
            Rectangle leftBounds = new Rectangle(xOffset - arrowSize - ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton leftArrow = new ArrowButton(leftBounds, Direction.RIGHT, row, true);
            arrowButtons.add(leftArrow);
            drawArrowButton(g2, leftArrow);

            // RECHTS vom Board: Pfeil zeigt NACH LINKS (schiebt nach links rein)
            Rectangle rightBounds = new Rectangle(xOffset + cols * size + ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton rightArrow = new ArrowButton(rightBounds, Direction.LEFT, row, true);
            arrowButtons.add(rightArrow);
            drawArrowButton(g2, rightArrow);
        }

        // COLUMN SHIFT — Nur jede 2. Spalte (Index 1,3,5,...)
        for (int col = 0; col < cols; col++) {

            if (col % 2 == 0) continue; // nur ungerade Spalten erlauben

            int x = xOffset + col * size + (size - arrowSize) / 2;

            // ÜBER dem Board: Pfeil zeigt NACH UNTEN (schiebt nach unten rein)
            Rectangle upBounds = new Rectangle(x, yOffset - arrowSize - ARROW_MARGIN, arrowSize, arrowSize);
            ArrowButton upArrow = new ArrowButton(upBounds, Direction.DOWN, col, false);
            arrowButtons.add(upArrow);
            drawArrowButton(g2, upArrow);

            // UNTER dem Board: Pfeil zeigt NACH OBEN (schiebt nach oben rein)
            Rectangle downBounds = new Rectangle(x, yOffset + rows * size + ARROW_MARGIN, arrowSize, arrowSize);
            ArrowButton downArrow = new ArrowButton(downBounds, Direction.UP, col, false);
            arrowButtons.add(downArrow);
            drawArrowButton(g2, downArrow);
        }
    }


    private void drawArrowButton(Graphics2D g2, ArrowButton arrow) {
        if (arrow == hoveredArrow) {
            g2.setColor(ARROW_COLOR_HOVER);
        } else {
            g2.setColor(ARROW_COLOR);
        }

        // kleiner Schatten
        g2.fillRoundRect(arrow.bounds.x + 2, arrow.bounds.y + 2, arrow.bounds.width, arrow.bounds.height, 8, 8);
        g2.setColor(new Color(20, 20, 40, 180));
        g2.drawRoundRect(arrow.bounds.x + 2, arrow.bounds.y + 2, arrow.bounds.width, arrow.bounds.height, 8, 8);

        // eigentlicher Button
        g2.setColor(arrow == hoveredArrow ? ARROW_COLOR_HOVER : ARROW_COLOR);
        g2.fillRoundRect(arrow.bounds.x, arrow.bounds.y, arrow.bounds.width, arrow.bounds.height, 8, 8);

        g2.setColor(Color.WHITE);
        g2.fill(arrow.arrowShape);
    }

    private void drawExtraTile(Graphics2D g2) {
        Tile extraTile = board.getExtraTile();
        if (extraTile != null) {
            int margin = 20;
            int x = getWidth() - size - margin;
            int y = getHeight() - size - margin;
            drawTileAt(g2, extraTile, x, y, -1, -1, false);
        }
    }

    private void drawDebugInfo(Graphics2D g2) {
        int infoX = getWidth() - 250;
        int infoY = 40;

        g2.setFont(DEBUG_INFO_FONT);
        g2.setColor(Color.RED);

        List<String> infoLines = new ArrayList<>();
        var players = board.getPlayers();

        if (players != null && !players.isEmpty()) {
            var current = players.get(board.getCurrentPlayerIndex());
            infoLines.add("Player to move: " + current.getName());
        } else {
            infoLines.add("Player to move: None");
        }

        infoLines.add("Free roam: " + board.isFreeRoam());
        infoLines.add("Current move state: " + board.getCurrentMoveState());
        infoLines.add(""); // blank line
        infoLines.add("=== Players ===");

        if (players != null && !players.isEmpty()) {
            for (Player p : players) {
                var pos = p.getCurrentPosition();
                String positionText = (pos != null) ? "(" + pos.getRow() + "," + pos.getColumn() + ")" : "(not placed)";
                infoLines.add(p.getName() + " at " + positionText);
            }
        }

        int lineHeight = 25;
        for (int i = 0; i < infoLines.size(); i++) {
            g2.drawString(infoLines.get(i), infoX, infoY + i * lineHeight);
        }
    }

    private void drawTreasureCards(Graphics2D g2, Player player) {
        int cardWidth = 60;
        int cardHeight = 90;
        int padding = 10;
        int startX = 10;
        int startY = 40;

        List<TreasureCard> cards = player.getAssignedTreasureCards();

        for (int i = 0; i < cards.size(); i++) {
            TreasureCard card = cards.get(i);
            g2.setColor(card.isCollected() ? new Color(50, 220, 50) : new Color(220, 220, 250));

            int y = startY + i * (cardHeight + padding);
            g2.fillRoundRect(startX, y, cardWidth, cardHeight, 10, 10);

            g2.setColor(Color.BLACK);
            String treasureName = cards.get(i).getTreasureName();
            FontMetrics fm = g2.getFontMetrics();
            int textWidth = fm.stringWidth(treasureName);
            g2.drawString(treasureName, startX + (cardWidth - textWidth) / 2, y + cardHeight / 2);

            g2.setColor(new Color(220, 220, 250));
        }
    }

    // =================================================================================
    // PUBLIC GAME LOGIC METHODS
    // =================================================================================

    public void switchPlayer() {
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size();
        this.currentPlayer = players.get(currentPlayerIndex);
        System.out.println("Switching player " + (currentPlayerIndex + 1));
        updateReachableTilesAndRepaint();
    }

    public void rotateExtraTile() {
        if (board.getExtraTile() != null) {
            board.getExtraTile().rotate();
            repaint();
        }
    }

    public void toggleFreeRoam() {
        board.setFreeRoam(!board.isFreeRoam());
        repaint();
    }

    // =================================================================================
    // INNER CLASSES
    // =================================================================================

    private static class ArrowButton {
        Rectangle bounds;
        Direction direction;
        int index;
        boolean isRow;
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
                case LEFT -> {
                    arrow.moveTo(cx + size / 2.0, cy - size / 2.0);
                    arrow.lineTo(cx - size / 2.0, cy);
                    arrow.lineTo(cx + size / 2.0, cy + size / 2.0);
                }
                case RIGHT -> {
                    arrow.moveTo(cx - size / 2.0, cy - size / 2.0);
                    arrow.lineTo(cx + size / 2.0, cy);
                    arrow.lineTo(cx - size / 2.0, cy + size / 2.0);
                }
                case UP -> {
                    arrow.moveTo(cx - size / 2.0, cy + size / 2.0);
                    arrow.lineTo(cx, cy - size / 2.0);
                    arrow.lineTo(cx + size / 2.0, cy + size / 2.0);
                }
                case DOWN -> {
                    arrow.moveTo(cx - size / 2.0, cy - size / 2.0);
                    arrow.lineTo(cx, cy + size / 2.0);
                    arrow.lineTo(cx + size / 2.0, cy - size / 2.0);
                }
            }
            arrow.closePath();
            return arrow;
        }

        boolean contains(Point p) {
            return bounds.contains(p);
        }


    }

    public void setBoard(Board board) {
        this.board = board;
        updateReachableTilesAndRepaint();
    }

    public void setPlayers(List<Player> players) {
        this.players = players != null ? players : List.of();
        repaint();
    }

    public void setCurrentPlayer(Player currentPlayer) {
        this.currentPlayer = currentPlayer;
        updateReachableTilesAndRepaint();
    }
}
