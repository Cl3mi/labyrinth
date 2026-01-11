package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.audio.SoundEffects;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import lombok.Getter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;
import java.util.*;
import java.util.List;

/**
 * Server-autoratives BoardPanel:
 * - Rendert den letzten State vom Server.
 * - User-Input sendet ausschließlich Commands (MOVE_PAWN, PUSH_TILE).
 * - Keine lokale Spielzustands-Mutation.
 * - Reachable Tiles (Highlighting) werden optional vom Server gesetzt.
 */
public class BoardPanel extends JPanel {

    private JButton optionsButton;

    private static final int CARD_PANEL_WIDTH = 40;
    private static final int PANEL_PADDING = 20;
    private static final int ARROW_MARGIN = 5;

    private static final Font DEBUG_INFO_FONT = new Font("Arial", Font.BOLD, 16);
    private static final Font PLAYER_MARKER_FONT = new Font("Arial", Font.BOLD, 30);
    private static final Font COORDINATE_FONT = new Font("Arial", Font.PLAIN, 10);

    private static final Color BACKGROUND_COLOR = Color.DARK_GRAY;
    private static final Color CORRIDOR_COLOR = new Color(235, 235, 220);
    private static final Color WALL_COLOR = new Color(50, 50, 50);
    private static final Color FIXED_TILE_BACKGROUND_COLOR = new Color(160, 160, 0);
    private static final Color ARROW_COLOR = new Color(70, 130, 180);
    private static final Color ARROW_COLOR_HOVER = new Color(120, 180, 230);

    private static final Color[] PLAYER_COLORS = {
            new Color(200, 80, 80), new Color(80, 180, 80),
            new Color(80, 120, 200), new Color(230, 200, 80)
    };

    private final GameClient client;
    private final ToastManager toastManager;
    private final SoundEffects soundEffects;

    private final List<BufferedImage> playerIcons = new ArrayList<>();
    private Image backgroundImage;

    @Getter
    private Board board;

    // Track previous board state to detect changes
    private Board previousBoard;

    // Track last push action for visual feedback
    private Integer lastPushedIndex = null;  // Row or column index
    private Boolean lastPushedWasRow = null; // true = row, false = column
    private Direction lastPushDirection = null;
    private long lastPushTimestamp = 0;
    private static final long PUSH_HIGHLIGHT_DURATION = 2000; // 2 seconds

    // Track current target to detect changes
    private String lastTargetTreasureName = null;
    private boolean showTargetBanner = false; // Show banner until first action

    // Keyboard navigation
    private int selectedRow = 0;
    private int selectedCol = 0;
    private boolean keyboardNavigationActive = false;

    private Player currentPlayer;
    private List<Player> players;

    private java.time.OffsetDateTime gameEndTime;
    private java.time.OffsetDateTime turnEndTime;
    private labyrinth.contracts.models.TurnState currentTurnState;
    private final Map<String, BufferedImage> treasureImages = new HashMap<>();
    private final Map<String, BufferedImage> tileImages = new HashMap<>();

    /**
     * Reachable Tiles kommen vom Server (optional).
     * Wenn du (noch) nichts vom Server bekommst, bleibt es leer -> kein Highlighting.
     */
    private final Set<Tile> reachableTiles = new HashSet<>();

    private final List<ArrowButton> arrowButtons = new ArrayList<>();
    private ArrowButton hoveredArrow = null;

    private int xOffset;
    private int yOffset;
    private int size;
    private int arrowSize = 30;

    private final Random random = new Random();

    private AudioPlayer backgroundMusic;

    private boolean inputLocked = false;

    // Flag to prevent commands after game is over
    private boolean gameIsOver = false;

    private static final String EXTRA_KEY = "EXTRA";

    public BoardPanel(GameClient client, Board board, Player currentPlayer, List<Player> players) {
        this.client = Objects.requireNonNull(client, "client must not be null");
        this.board = Objects.requireNonNull(board, "board must not be null");
        this.currentPlayer = currentPlayer;
        this.players = players != null ? players : List.of();

        loadBackgroundImage();
        loadTileImages();
        loadTreasureImages();
        loadPlayerIcons();

        setBackground(BACKGROUND_COLOR);
        setPreferredSize(new Dimension(1920, 1080));

        setLayout(null); // Overlay-Button

        optionsButton = createStyledOptionsButton();
        add(optionsButton);

        // Initialize toast manager and sound effects
        this.toastManager = new ToastManager(this);
        this.soundEffects = new SoundEffects();

        setupMouseListener();
        setupKeyboardListener();

        // Lade gespeicherte Lautstärke
        java.util.prefs.Preferences prefs = java.util.prefs.Preferences.userNodeForPackage(getClass());
        int savedMusicVolume = prefs.getInt("musicVolume", 50);
        int savedSfxVolume = prefs.getInt("sfxVolume", 70);

        backgroundMusic = AudioPlayer.getInstance();

        // Apply persisted global music volume and ensure background music is running
        backgroundMusic.setMusicVolume(savedMusicVolume / 100.0f);
        backgroundMusic.playMenuMusic();

        soundEffects.setVolume(savedSfxVolume / 100.0f);

        // Start timer to update sidebar countdowns every second
        javax.swing.Timer sidebarTimer = new javax.swing.Timer(1000, e -> repaint());
        sidebarTimer.start();
    }

    // Callback für Beenden
    private Runnable onExitGame;

    public void setOnExitGame(Runnable callback) {
        this.onExitGame = callback;
    }

    private JButton createStyledOptionsButton() {
        JButton btn = new JButton("⚙") {
            private boolean isHovered = false;

            {
                addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseEntered(MouseEvent e) { isHovered = true; repaint(); }
                    @Override
                    public void mouseExited(MouseEvent e) { isHovered = false; repaint(); }
                });
            }

            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                int w = getWidth();
                int h = getHeight();

                // Background
                if (isHovered) {
                    g2.setColor(new Color(80, 70, 55, 220));
                } else {
                    g2.setColor(new Color(50, 45, 40, 200));
                }
                g2.fillRoundRect(0, 0, w, h, 10, 10);

                // Border
                g2.setColor(new Color(180, 150, 100, isHovered ? 255 : 180));
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, w - 2, h - 2, 10, 10);

                // Icon
                g2.setFont(new Font("SansSerif", Font.BOLD, 20));
                g2.setColor(new Color(255, 215, 0));
                FontMetrics fm = g2.getFontMetrics();
                String text = "⚙";
                int textX = (w - fm.stringWidth(text)) / 2;
                int textY = (h + fm.getAscent() - fm.getDescent()) / 2;
                g2.drawString(text, textX, textY);

                g2.dispose();
            }
        };
        btn.setToolTipText("Optionen (ESC)");
        btn.setBounds(10, 10, 45, 40);
        btn.setOpaque(false);
        btn.setContentAreaFilled(false);
        btn.setBorderPainted(false);
        btn.setFocusPainted(false);
        btn.setCursor(new Cursor(Cursor.HAND_CURSOR));
        btn.addActionListener(e -> showOptionsDialog());
        return btn;
    }

    // =================================================================================
    // IMAGE LOADING
    // =================================================================================

    private void loadBackgroundImage() {
        try {
            var url = getClass().getResource("/images/ui/background.png");
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
            } else {
                System.err.println("Background image not found: /images/ui/background.png");
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

    private void loadTreasureImages() {
        // ✅ MAPPING: Deutsche Server-Namen -> Englische Dateinamen
        Map<String, String> treasureFileMapping = new HashMap<>();

        // Server sendet deutsche Namen, Dateien haben englische Namen
        treasureFileMapping.put("Geist", "Ghost");           // id: 1
        treasureFileMapping.put("Drache", "Dragon");         // id: 2
        treasureFileMapping.put("Hexe", "Witch");            // id: 3
        treasureFileMapping.put("Eule", "Owl");              // id: 4
        treasureFileMapping.put("Ratte", "Rat");             // id: 5
        treasureFileMapping.put("Käfer", "Bug");             // id: 6
        treasureFileMapping.put("Spinne", "Spider");         // id: 7
        treasureFileMapping.put("Schlange", "Snake");        // id: 8
        treasureFileMapping.put("Fledermaus", "Bat");        // id: 9
        treasureFileMapping.put("Krone", "Crown");           // id: 10
        treasureFileMapping.put("Schlüssel", "Key");         // id: 11
        treasureFileMapping.put("Schatztruhe", "Treasure"); // id: 12
        treasureFileMapping.put("Helm", "Helmet");           // id: 13
        treasureFileMapping.put("Buch", "Book");              // id: 14
        treasureFileMapping.put("Kerze", "Candle");          // id: 15
        treasureFileMapping.put("Ring", "Ring");             // id: 16
        treasureFileMapping.put("Beutel", "Bag");            // id: 17
        treasureFileMapping.put("Totenkopf", "Skull");       // id: 18
        treasureFileMapping.put("Karte", "Map");             // id: 19
        treasureFileMapping.put("Schwert", "Sword");         // id: 20
        treasureFileMapping.put("Kelch", "chalice");         // id: 21
        treasureFileMapping.put("Edelstein", "Diamond");     // id: 22
        treasureFileMapping.put("Krug", "Jug");              // id: 23
        treasureFileMapping.put("Maus", "Mouse");            // id: 24

        // Load each treasure image
        for (Map.Entry<String, String> entry : treasureFileMapping.entrySet()) {
            String serverName = entry.getKey();
            String fileName = entry.getValue();

            BufferedImage img = null;

            img = loadImage("/images/tiles/" + fileName + ".png");

            if (img != null) {
                treasureImages.put(serverName, img);
                System.out.println("✅ Loaded treasure: " + serverName + " -> " + fileName);
            } else {
                System.err.println("❌ Failed to load treasure: " + serverName + " (file: " + fileName + ")");
            }
        }

        System.out.println("📦 Loaded " + treasureImages.size() + "/24 treasure images");
    }


    /**
     * Lädt ALLE Varianten in tileVariants und initialisiert die tileBags.
     */
    private void loadTileImages() {
        // Nur noch je 1 Bild pro Typ
        tileImages.put("I", loadImage("/images/tiles/I_tile.png"));
        tileImages.put("L", loadImage("/images/tiles/L_tile.png"));
        tileImages.put("T", loadImage("/images/tiles/T_tile.png"));

        System.out.println("📦 Loaded tile images:");
        tileImages.forEach((type, img) -> {
            if (img != null) {
                System.out.println("  ✅ " + type + " tile");
            } else {
                System.err.println("  ❌ " + type + " tile FEHLT!");
            }
        });
    }

    private void loadPlayerIcons() {
        for (int i = 1; i <= 4; i++) {
            BufferedImage img = loadImage("/images/players/player" + i + ".png");
            playerIcons.add(img);
        }
    }

    // =================================================================================
    // TILE IMAGE INFO & DRAWING
    // =================================================================================

    private static class TileImageInfo {
        final String type;
        final int rotation;

        TileImageInfo(String type, int rotation) {
            this.type = type;
            this.rotation = rotation;
        }
    }

    private BufferedImage getTileImage(String type) {
        return tileImages.get(type);
    }


    private TileImageInfo getTileImageInfo(Tile tile) {
        Direction[] entrancesArray = tile.getEntrances();
        if (entrancesArray == null || entrancesArray.length == 0) {
            return null;
        }

        EnumSet<Direction> dirs = EnumSet.noneOf(Direction.class);
        Collections.addAll(dirs, entrancesArray);

        // I
        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.DOWN)) return new TileImageInfo("I", 0);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.RIGHT)) return new TileImageInfo("I", 90);
        }

        // L
        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.RIGHT)) return new TileImageInfo("L", 0);
            if (dirs.contains(Direction.RIGHT) && dirs.contains(Direction.DOWN)) return new TileImageInfo("L", 90);
            if (dirs.contains(Direction.DOWN) && dirs.contains(Direction.LEFT)) return new TileImageInfo("L", 180);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.UP)) return new TileImageInfo("L", 270);
        }

        // T
        if (dirs.size() == 3) {
            if (!dirs.contains(Direction.DOWN)) return new TileImageInfo("T", 0);
            if (!dirs.contains(Direction.LEFT)) return new TileImageInfo("T", 90);
            if (!dirs.contains(Direction.UP)) return new TileImageInfo("T", 180);
            if (!dirs.contains(Direction.RIGHT)) return new TileImageInfo("T", 270);
        }

        return null;
    }

    private void drawRotatedImage(Graphics2D g2, Image img, int x, int y, int rotationDeg) {
        if (img == null) return;

        Graphics2D g = (Graphics2D) g2.create();
        try {
            Shape oldClip = g.getClip();
            g.setClip(new Rectangle(x, y, size, size));

            double scale = 1.2;
            int drawSize = (int) Math.round(size * scale);

            int drawX = x - (drawSize - size) / 2;
            int drawY = y - (drawSize - size) / 2;

            int cx = drawX + drawSize / 2;
            int cy = drawY + drawSize / 2;

            g.rotate(Math.toRadians(rotationDeg), cx, cy);
            g.drawImage(img, drawX, drawY, drawSize, drawSize, null);

            g.setClip(oldClip);
        } finally {
            g.dispose();
        }
    }

    private boolean hasEntrance(Direction[] entrances, Direction dir) {
        if (entrances == null) return false;
        for (Direction d : entrances) {
            if (d == dir) return true;
        }
        return false;
    }

    private void drawCorridorsFallback(Graphics2D g2, Tile tile, int x, int y) {
        int cx = x + size / 2;
        int cy = y + size / 2;
        int corridorWidth = Math.max(4, size / 6);

        Direction[] entrances = tile.getEntrances();
        if (entrances == null || entrances.length == 0) return;

        g2.setColor(CORRIDOR_COLOR);

        if (hasEntrance(entrances, Direction.UP)) {
            g2.fillRect(cx - corridorWidth / 2, y, corridorWidth, size / 2);
        }
        if (hasEntrance(entrances, Direction.DOWN)) {
            g2.fillRect(cx - corridorWidth / 2, cy, corridorWidth, size / 2);
        }
        if (hasEntrance(entrances, Direction.LEFT)) {
            g2.fillRect(x, cy - corridorWidth / 2, size / 2, corridorWidth);
        }
        if (hasEntrance(entrances, Direction.RIGHT)) {
            g2.fillRect(cx, cy - corridorWidth / 2, size / 2, corridorWidth);
        }

        int dotSize = Math.max(4, corridorWidth);
        g2.setColor(Boolean.TRUE.equals(tile.getIsFixed()) ? FIXED_TILE_BACKGROUND_COLOR : WALL_COLOR);
        g2.fillOval(cx - dotSize / 2, cy - dotSize / 2, dotSize, dotSize);
    }

    private void drawTileHighlight(Graphics2D g2, int x, int y) {
        g2.setColor(new Color(255, 255, 0, 120));
        g2.fillRoundRect(x - 4, y - 4, size + 8, size + 8, 15, 15);
    }

    // =================================================================================
    // INPUT (SERVER-AUTORITATIV)
    // =================================================================================

    private void setupMouseListener() {
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (gameIsOver) return;  // Don't allow any input if game is over
                if (inputLocked) return;
                if (board == null) return;

                if (handleArrowClick(e.getPoint())) return;
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

    private void setupKeyboardListener() {
        // Make panel focusable to receive keyboard events
        setFocusable(true);
        requestFocusInWindow();

        addKeyListener(new java.awt.event.KeyAdapter() {
            @Override
            public void keyPressed(java.awt.event.KeyEvent e) {
                if (gameIsOver && e.getKeyCode() != java.awt.event.KeyEvent.VK_ESCAPE) return;
                handleKeyPress(e);
            }
        });

        // Request focus when clicked
        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                requestFocusInWindow();
            }
        });
    }

    private void handleKeyPress(java.awt.event.KeyEvent e) {
        if (inputLocked && e.getKeyCode() != java.awt.event.KeyEvent.VK_ESCAPE) return;
        if (board == null) return;

        switch (e.getKeyCode()) {
            // Rotation controls
            case java.awt.event.KeyEvent.VK_R, java.awt.event.KeyEvent.VK_E -> {
                // Rotate spare tile clockwise
                if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
                    soundEffects.playRotate();
                    client.sendRotateTile();
                    showTargetBanner = false; // Hide banner after first action
                    toastManager.showInfo("ROTATE", "Extra-Tile gedreht", "R/E: im Uhrzeigersinn, Q: gegen Uhrzeigersinn");
                    inputLocked = true;
                }
            }
            case java.awt.event.KeyEvent.VK_Q -> {
                // Rotate spare tile counter-clockwise (3 times clockwise = 1 counter-clockwise)
                if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
                    soundEffects.playRotate();
                    // Send 3 rotate commands for counter-clockwise
                    client.sendRotateTile();
                    try { Thread.sleep(50); } catch (InterruptedException ignored) {}
                    client.sendRotateTile();
                    try { Thread.sleep(50); } catch (InterruptedException ignored) {}
                    client.sendRotateTile();
                    showTargetBanner = false; // Hide banner after first action
                    toastManager.showInfo("ROTATE", "Extra-Tile gedreht", "Gegen Uhrzeigersinn gedreht");
                    inputLocked = true;
                }
            }

            // Arrow key navigation
            case java.awt.event.KeyEvent.VK_UP -> {
                keyboardNavigationActive = true;
                selectedRow = Math.max(0, selectedRow - 1);
                repaint();
            }
            case java.awt.event.KeyEvent.VK_DOWN -> {
                keyboardNavigationActive = true;
                selectedRow = Math.min(board.getHeight() - 1, selectedRow + 1);
                repaint();
            }
            case java.awt.event.KeyEvent.VK_LEFT -> {
                keyboardNavigationActive = true;
                selectedCol = Math.max(0, selectedCol - 1);
                repaint();
            }
            case java.awt.event.KeyEvent.VK_RIGHT -> {
                keyboardNavigationActive = true;
                selectedCol = Math.min(board.getWidth() - 1, selectedCol + 1);
                repaint();
            }

            // Confirm selection with Enter or Space
            case java.awt.event.KeyEvent.VK_ENTER, java.awt.event.KeyEvent.VK_SPACE -> {
                if (keyboardNavigationActive && currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_MOVE) {
                    client.sendMovePawn(selectedRow, selectedCol);
                    showTargetBanner = false; // Hide banner after first action
                    inputLocked = true;
                    keyboardNavigationActive = false;
                } else if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
                    toastManager.showInfo("INFO", "Hinweis", "Erst Tile einschieben, dann Figur bewegen");
                }
            }

            // Escape - pause/options
            case java.awt.event.KeyEvent.VK_ESCAPE -> {
                showOptionsDialog();
            }

            // Tab - toggle keyboard navigation help
            case java.awt.event.KeyEvent.VK_TAB -> {
                showKeyboardHelp();
            }
        }
    }

    private void showKeyboardHelp() {
        String helpText = """
            TASTATUR-STEUERUNG:

            Pfeiltasten: Tile-Auswahl navigieren
            Enter/Leertaste: Ausgewähltes Tile bestätigen

            R / E: Extra-Tile im Uhrzeigersinn drehen
            Q: Extra-Tile gegen Uhrzeigersinn drehen

            Esc: Optionen/Pause
            Tab: Diese Hilfe anzeigen
            """;

        toastManager.showInfo("HELP", "Tastatur-Hilfe", helpText);
    }

    private boolean handleArrowClick(Point p) {
        for (ArrowButton arrow : arrowButtons) {
            if (arrow.contains(p)) {
                // Record push action for visual feedback
                lastPushedIndex = arrow.index;
                lastPushedWasRow = arrow.isRow;
                lastPushDirection = arrow.direction;
                lastPushTimestamp = System.currentTimeMillis();

                // Play push sound
                soundEffects.playPush();

                // Show toast notification
                String rowCol = arrow.isRow ? "Zeile " + arrow.index : "Spalte " + arrow.index;
                String directionStr = switch (arrow.direction) {
                    case UP -> "nach oben";
                    case DOWN -> "nach unten";
                    case LEFT -> "nach links";
                    case RIGHT -> "nach rechts";
                };
                toastManager.showInfo("PUSH", "Einschub", rowCol + " wird " + directionStr + " geschoben");

                // Send command to server
                client.sendPushTile(arrow.index, arrow.direction);
                showTargetBanner = false; // Hide banner after first action
                inputLocked = true;
                return true;
            }
        }
        return false;
    }

    private void handleTileClick(Point p) {
        if (board == null) return;

        outer:
        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Rectangle tileRect = new Rectangle(xOffset + col * size, yOffset + row * size, size, size);
                if (tileRect.contains(p)) {
                    soundEffects.playMove();
                    client.sendMovePawn(row, col);
                    showTargetBanner = false; // Hide banner after first action
                    inputLocked = true;
                    break outer;
                }
            }
        }
    }

    // =================================================================================
    // PAINTING
    // =================================================================================

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        if (board == null) return;

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
            drawSidebar(g2);
            drawCurrentTargetOverlay(g2); // NEW: Draw target treasure overlay
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
        // Draw push highlight overlay first (behind tiles)
        drawPushHighlight(g2);

        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Tile tile = board.getTiles()[row][col];
                if (tile == null) continue;

                int x = xOffset + col * size;
                int y = yOffset + row * size;

                // Show keyboard selection highlight
                if (keyboardNavigationActive && row == selectedRow && col == selectedCol) {
                    drawKeyboardSelectionHighlight(g2, x, y);
                }

                if (reachableTiles.contains(tile)) {
                    drawTileHighlight(g2, x, y);
                }

                drawTileAt(g2, tile, x, y, row, col, true);
                drawPlayersOnTile(g2, row, col);
            }
        }
    }

    private void drawKeyboardSelectionHighlight(Graphics2D g2, int x, int y) {
        // Draw animated selection border
        long time = System.currentTimeMillis();
        int pulseAlpha = 150 + (int) (50 * Math.sin(time / 200.0));

        g2.setColor(new Color(255, 255, 100, pulseAlpha));
        g2.setStroke(new BasicStroke(4));
        g2.drawRoundRect(x - 2, y - 2, size + 4, size + 4, 10, 10);

        // Draw corner markers
        g2.setColor(new Color(255, 255, 0, 200));
        int cornerSize = 10;
        // Top-left
        g2.fillRect(x - 2, y - 2, cornerSize, 3);
        g2.fillRect(x - 2, y - 2, 3, cornerSize);
        // Top-right
        g2.fillRect(x + size - cornerSize + 2, y - 2, cornerSize, 3);
        g2.fillRect(x + size - 1, y - 2, 3, cornerSize);
        // Bottom-left
        g2.fillRect(x - 2, y + size - 1, cornerSize, 3);
        g2.fillRect(x - 2, y + size - cornerSize + 2, 3, cornerSize);
        // Bottom-right
        g2.fillRect(x + size - cornerSize + 2, y + size - 1, cornerSize, 3);
        g2.fillRect(x + size - 1, y + size - cornerSize + 2, 3, cornerSize);
    }

    /**
     * Draws a highlight overlay on the last pushed row or column
     */
    private void drawPushHighlight(Graphics2D g2) {
        if (lastPushedIndex == null || lastPushedWasRow == null) return;

        // Check if highlight has expired
        long elapsed = System.currentTimeMillis() - lastPushTimestamp;
        if (elapsed > PUSH_HIGHLIGHT_DURATION) {
            lastPushedIndex = null;
            return;
        }

        // Calculate fade-out alpha based on time
        float alpha = 1.0f - (elapsed / (float) PUSH_HIGHLIGHT_DURATION);
        alpha = Math.max(0.2f, alpha); // Minimum 20% opacity

        // Set highlight color with fade
        Color highlightColor = new Color(70, 130, 180, (int) (100 * alpha));
        g2.setColor(highlightColor);

        if (lastPushedWasRow) {
            // Highlight entire row
            int row = lastPushedIndex;
            int y = yOffset + row * size;
            g2.fillRect(xOffset, y, board.getWidth() * size, size);

            // Draw directional indicator
            drawPushDirectionIndicator(g2, row, -1, lastPushDirection, alpha);
        } else {
            // Highlight entire column
            int col = lastPushedIndex;
            int x = xOffset + col * size;
            g2.fillRect(x, yOffset, size, board.getHeight() * size);

            // Draw directional indicator
            drawPushDirectionIndicator(g2, -1, col, lastPushDirection, alpha);
        }
    }

    /**
     * Draws directional arrows on the pushed row/column
     */
    private void drawPushDirectionIndicator(Graphics2D g2, int row, int col, Direction direction, float alpha) {
        if (direction == null) return;

        g2.setColor(new Color(255, 255, 255, (int) (200 * alpha)));
        g2.setStroke(new BasicStroke(3));

        if (row >= 0) {
            // Draw arrows along the row
            int y = yOffset + row * size + size / 2;
            for (int c = 0; c < board.getWidth(); c++) {
                int x = xOffset + c * size + size / 2;
                drawDirectionArrow(g2, x, y, direction, size / 4);
            }
        } else if (col >= 0) {
            // Draw arrows along the column
            int x = xOffset + col * size + size / 2;
            for (int r = 0; r < board.getHeight(); r++) {
                int y = yOffset + r * size + size / 2;
                drawDirectionArrow(g2, x, y, direction, size / 4);
            }
        }
    }

    /**
     * Draws a single directional arrow
     */
    private void drawDirectionArrow(Graphics2D g2, int cx, int cy, Direction direction, int arrowSize) {
        int[] xPoints = new int[3];
        int[] yPoints = new int[3];

        switch (direction) {
            case UP -> {
                xPoints = new int[]{cx - arrowSize / 2, cx, cx + arrowSize / 2};
                yPoints = new int[]{cy + arrowSize / 3, cy - arrowSize / 3, cy + arrowSize / 3};
            }
            case DOWN -> {
                xPoints = new int[]{cx - arrowSize / 2, cx, cx + arrowSize / 2};
                yPoints = new int[]{cy - arrowSize / 3, cy + arrowSize / 3, cy - arrowSize / 3};
            }
            case LEFT -> {
                xPoints = new int[]{cx + arrowSize / 3, cx - arrowSize / 3, cx + arrowSize / 3};
                yPoints = new int[]{cy - arrowSize / 2, cy, cy + arrowSize / 2};
            }
            case RIGHT -> {
                xPoints = new int[]{cx - arrowSize / 3, cx + arrowSize / 3, cx - arrowSize / 3};
                yPoints = new int[]{cy - arrowSize / 2, cy, cy + arrowSize / 2};
            }
        }

        g2.fillPolygon(xPoints, yPoints, 3);
    }

    private void drawTileAt(Graphics2D g2, Tile tile, int x, int y, int row, int col, boolean drawDetails) {
        TileImageInfo info = getTileImageInfo(tile);
        boolean drewImage = false;

        if (info != null) {
            // ✅ Einfach: Hole Bild direkt aus Map
            BufferedImage img = getTileImage(info.type);
            if (img != null) {
                drawRotatedImage(g2, img, x, y, info.rotation);
                drewImage = true;
            }
        }

        if (!drewImage) {
            drawCorridorsFallback(g2, tile, x, y);
        }

        if (drawDetails) {
            // Treasure zeichnen
            if (tile.getTreasure() != null) {
                int cx = x + size / 2;
                int cy = y + size / 2;
                drawTreasureOnTile(g2, tile.getTreasure(), cx, cy);
            }

            // Koordinaten nur bei gültigen row/col
            if (row >= 0 && col >= 0) {
                drawCoordinates(g2, x, y, row, col);
            }
        }
    }

    private void drawTreasureOnTile(Graphics2D g2, Treasure treasure, int centerX, int centerY) {
        if (treasure == null || treasure.getName() == null) return;

        // Check if this is the current player's target treasure
        boolean isCurrentTarget = false;
        if (currentPlayer != null && currentPlayer.getAssignedTreasureCards() != null
                && !currentPlayer.getAssignedTreasureCards().isEmpty()) {
            Treasure currentTarget = currentPlayer.getAssignedTreasureCards().get(0);
            isCurrentTarget = currentTarget != null && currentTarget.getName() != null
                    && currentTarget.getName().equals(treasure.getName());
        }

        // ✅ Pulsing glow effect for target treasure
        if (isCurrentTarget) {
            long time = System.currentTimeMillis();
            int glowRadius = 28 + (int) (8 * Math.sin(time / 300.0));
            int glowAlpha = 120 + (int) (60 * Math.sin(time / 300.0));

            // Outer glow
            g2.setColor(new Color(255, 215, 0, Math.min(glowAlpha / 2, 100)));
            g2.fillOval(centerX - glowRadius, centerY - glowRadius - 8, glowRadius * 2, glowRadius * 2);

            // Inner glow
            g2.setColor(new Color(255, 255, 0, Math.min(glowAlpha, 150)));
            g2.fillOval(centerX - glowRadius / 2, centerY - glowRadius / 2 - 8, glowRadius, glowRadius);
        }

        // ✅ Draw treasure IMAGE
        BufferedImage treasureImg = treasureImages.get(treasure.getName());
        if (treasureImg != null) {
            // Calculate image size (scaled based on tile size, but not too large)
            int imgSize = Math.min((int)(size * 0.45), 48); // Max 48px, ~45% of tile

            // Draw subtle shadow for depth
            g2.setColor(new Color(0, 0, 0, 80));
            g2.fillOval(centerX - imgSize / 2 + 2, centerY - imgSize / 2 + 2 - 8, imgSize, imgSize);

            // Draw actual treasure image
            g2.drawImage(treasureImg,
                    centerX - imgSize / 2,
                    centerY - imgSize / 2 - 12,
                    imgSize,
                    imgSize,
                    null);

            // Add extra border highlight for current target
            if (isCurrentTarget) {
                g2.setColor(new Color(255, 215, 0, 200));
                g2.setStroke(new BasicStroke(3));
                g2.drawOval(centerX - imgSize / 2 - 2, centerY - imgSize / 2 - 14, imgSize + 4, imgSize + 4);
            }
        } else {
            // ⚠️ Fallback if image missing: Simple colored circle with first letter
            int fallbackSize = Math.min((int)(size * 0.4), 40);

            // Circle background
            g2.setColor(isCurrentTarget ? new Color(255, 215, 0, 200) : new Color(180, 140, 70, 200));
            g2.fillOval(centerX - fallbackSize / 2, centerY - fallbackSize / 2 - 12, fallbackSize, fallbackSize);

            // Border
            g2.setColor(new Color(100, 70, 30));
            g2.setStroke(new BasicStroke(2));
            g2.drawOval(centerX - fallbackSize / 2, centerY - fallbackSize / 2 - 12, fallbackSize, fallbackSize);

            // First letter
            g2.setFont(new Font("Arial", Font.BOLD, fallbackSize / 2));
            g2.setColor(Color.WHITE);
            String letter = treasure.getName().substring(0, 1);
            FontMetrics fm = g2.getFontMetrics();
            int letterWidth = fm.stringWidth(letter);
            g2.drawString(letter, centerX - letterWidth / 2, centerY + fm.getAscent() / 2 - 12);

            System.err.println("⚠️ Using fallback for treasure: " + treasure.getName());
        }

        // ✅ Draw treasure name UNDER the image with background
        g2.setFont(new Font("Arial", Font.BOLD, isCurrentTarget ? 11 : 9));
        FontMetrics fm = g2.getFontMetrics();
        String displayName = treasure.getName();

        // Shorten long names
        if (displayName.length() > 10) {
            displayName = displayName.substring(0, 9) + "…";
        }

        int textWidth = fm.stringWidth(displayName);
        int textHeight = fm.getHeight();
        int boxY = centerY + 12;

        // Background box for better readability
        if (isCurrentTarget) {
            // Gold background for target
            g2.setColor(new Color(255, 215, 0, 240));
            g2.fillRoundRect(centerX - textWidth / 2 - 5, boxY - 2, textWidth + 10, textHeight, 6, 6);

            // Border
            g2.setColor(new Color(200, 160, 0));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(centerX - textWidth / 2 - 5, boxY - 2, textWidth + 10, textHeight, 6, 6);

            // Text
            g2.setColor(new Color(0, 0, 0));
        } else {
            // White/transparent background for non-target
            g2.setColor(new Color(255, 255, 255, 220));
            g2.fillRoundRect(centerX - textWidth / 2 - 4, boxY - 1, textWidth + 8, textHeight - 2, 5, 5);

            // Subtle border
            g2.setColor(new Color(150, 120, 80, 180));
            g2.setStroke(new BasicStroke(1));
            g2.drawRoundRect(centerX - textWidth / 2 - 4, boxY - 1, textWidth + 8, textHeight - 2, 5, 5);

            // Text
            g2.setColor(new Color(60, 40, 10));
        }

        g2.drawString(displayName, centerX - textWidth / 2, boxY + fm.getAscent() - 2);

        // Target indicator above the treasure - use golden circle instead of emoji
        if (isCurrentTarget) {
            // Draw golden star indicator
            int starSize = 16;
            int starX = centerX - starSize / 2;
            int starY = centerY - 40;

            // Glow effect
            g2.setColor(new Color(255, 215, 0, 150));
            g2.fillOval(starX - 4, starY - 4, starSize + 8, starSize + 8);

            // Star center
            g2.setColor(new Color(255, 255, 100));
            g2.fillOval(starX, starY, starSize, starSize);

            // Border
            g2.setColor(new Color(200, 150, 0));
            g2.setStroke(new BasicStroke(2));
            g2.drawOval(starX, starY, starSize, starSize);
        }
    }

    private void drawCoordinates(Graphics2D g2, int x, int y, int row, int col) {
        if (row < 0 || col < 0) return;

        String coords = "(" + row + "," + col + ")";
        g2.setColor(Color.WHITE);

        Font oldFont = g2.getFont();
        g2.setFont(COORDINATE_FONT);
        g2.drawString(coords, x + 3, y + size - 3);
        g2.setFont(oldFont);
    }

    private void drawPlayersOnTile(Graphics2D g2, int row, int col) {
        // First, collect all players on this tile
        List<Integer> playersOnTile = new ArrayList<>();
        for (int i = 0; i < players.size(); i++) {
            Player p = players.get(i);
            if (p == null || p.getCurrentPosition() == null) continue;

            if (p.getCurrentPosition().getRow() == row &&
                    p.getCurrentPosition().getColumn() == col) {
                playersOnTile.add(i);
            }
        }

        if (playersOnTile.isEmpty()) return;

        // Calculate positions for multiple players in a grid pattern
        int baseX = xOffset + col * size;
        int baseY = yOffset + row * size;

        // Positions for 1-4 players
        int[][] offsets = {
                {size / 2, size / 2},                    // 1 player: center
                {size / 3, size / 2, 2 * size / 3, size / 2},  // 2 players: left, right
                {size / 3, size / 3, 2 * size / 3, size / 3, size / 2, 2 * size / 3},  // 3 players: top-left, top-right, bottom-center
                {size / 3, size / 3, 2 * size / 3, size / 3, size / 3, 2 * size / 3, 2 * size / 3, 2 * size / 3}  // 4 players: all corners
        };

        int count = Math.min(playersOnTile.size(), 4);
        int[] positions = offsets[count - 1];

        for (int idx = 0; idx < count; idx++) {
            int i = playersOnTile.get(idx);
            int px = baseX + positions[idx * 2];
            int py = baseY + positions[idx * 2 + 1];

            BufferedImage icon = (i < playerIcons.size()) ? playerIcons.get(i) : null;
            if (icon != null) {
                int iconSize = count > 1 ? (int) (size * 0.35) : (int) (size * 0.6);
                g2.drawImage(icon,
                        px - iconSize / 2,
                        py - iconSize / 2,
                        iconSize,
                        iconSize,
                        null);
            } else {
                g2.setColor(PLAYER_COLORS[i % PLAYER_COLORS.length]);
                Font oldFont = g2.getFont();
                Font scaledFont = count > 1 ? PLAYER_MARKER_FONT.deriveFont(20f) : PLAYER_MARKER_FONT;
                g2.setFont(scaledFont);

                FontMetrics fm = g2.getFontMetrics();
                String text = "P" + (i + 1);
                int textWidth = fm.stringWidth(text);

                g2.drawString(text, px - textWidth / 2,
                        py + fm.getAscent() / 2 - fm.getDescent());

                g2.setFont(oldFont);
            }
        }
    }

    private void createAndDrawArrowButtons(Graphics2D g2) {
        arrowButtons.clear();
        g2.setStroke(new BasicStroke(2));

        int rows = board.getHeight();
        int cols = board.getWidth();

        for (int row = 0; row < rows; row++) {
            if (row % 2 == 0) continue;

            int y = yOffset + row * size + (size - arrowSize) / 2;

            Rectangle leftBounds = new Rectangle(xOffset - arrowSize - ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton leftArrow = new ArrowButton(leftBounds, Direction.RIGHT, row, true);
            arrowButtons.add(leftArrow);
            drawArrowButton(g2, leftArrow);

            Rectangle rightBounds = new Rectangle(xOffset + cols * size + ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton rightArrow = new ArrowButton(rightBounds, Direction.LEFT, row, true);
            arrowButtons.add(rightArrow);
            drawArrowButton(g2, rightArrow);
        }

        for (int col = 0; col < cols; col++) {
            if (col % 2 == 0) continue;

            int x = xOffset + col * size + (size - arrowSize) / 2;

            Rectangle upBounds = new Rectangle(x, yOffset - arrowSize - ARROW_MARGIN, arrowSize, arrowSize);
            ArrowButton upArrow = new ArrowButton(upBounds, Direction.DOWN, col, false);
            arrowButtons.add(upArrow);
            drawArrowButton(g2, upArrow);

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

        g2.fillRoundRect(arrow.bounds.x + 2, arrow.bounds.y + 2, arrow.bounds.width, arrow.bounds.height, 8, 8);
        g2.setColor(new Color(20, 20, 40, 180));
        g2.drawRoundRect(arrow.bounds.x + 2, arrow.bounds.y + 2, arrow.bounds.width, arrow.bounds.height, 8, 8);

        g2.setColor(arrow == hoveredArrow ? ARROW_COLOR_HOVER : ARROW_COLOR);
        g2.fillRoundRect(arrow.bounds.x, arrow.bounds.y, arrow.bounds.width, arrow.bounds.height, 8, 8);

        g2.setColor(Color.WHITE);
        g2.fill(arrow.arrowShape);
    }

    private void drawExtraTile(Graphics2D g2) {
        int margin = 20;
        int x = getWidth() - size - margin;
        int y = getHeight() - size - margin;

        // Label über dem Tile
        g2.setFont(new Font("Arial", Font.BOLD, 14));
        g2.setColor(Color.BLACK);
        g2.drawString("Schiebekarte", x, y - 8);

        Tile extraTile = board.getExtraTile();
        if (extraTile == null) {
            // Debug: Fallback wenn null
            g2.setColor(new Color(255, 0, 0, 140));
            g2.drawRect(x, y, size, size);
            g2.setFont(new Font("Arial", Font.PLAIN, 12));
            g2.drawString("NULL", x + 5, y + 15);
            return;
        }

        // ✅ Zeichne Tile MIT allen Details (inkl. Treasure!)
        // Verwende drawTileAt() mit drawDetails = true
        drawTileAt(g2, extraTile, x, y, -1, -1, true);

        // Optional: Hinweistext dass es gedreht werden kann
        if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
            g2.setFont(new Font("Arial", Font.ITALIC, 10));
            g2.setColor(new Color(255, 255, 255, 200));
            g2.drawString("Drücke R/Q/E zum Drehen", x, y + size + 15);
        }
    }

    private String formatTimeRemaining(java.time.OffsetDateTime endTime) {
        if (endTime == null) return "--:--";

        java.time.Duration remaining = java.time.Duration.between(
                java.time.OffsetDateTime.now(),
                endTime
        );

        if (remaining.isNegative()) return "00:00";

        long totalSeconds = remaining.getSeconds();
        long hours = totalSeconds / 3600;
        long minutes = (totalSeconds % 3600) / 60;
        long seconds = totalSeconds % 60;

        if (hours > 0) {
            return String.format("%d:%02d:%02d", hours, minutes, seconds);
        } else {
            return String.format("%d:%02d", minutes, seconds);
        }
    }

    private void drawSidebar(Graphics2D g2) {
        int sidebarWidth = 300;
        int sidebarX = 10;
        int sidebarY = 60;
        int padding = 15;

        // Background panel with gradient
        GradientPaint gradient = new GradientPaint(
                sidebarX, sidebarY,
                new Color(40, 40, 50, 240),
                sidebarX, getHeight(),
                new Color(30, 30, 40, 240)
        );
        g2.setPaint(gradient);
        g2.fillRoundRect(sidebarX, sidebarY, sidebarWidth, getHeight() - sidebarY - 20, 15, 15);

        // Border with glow effect
        g2.setColor(new Color(100, 130, 180, 150));
        g2.setStroke(new BasicStroke(3));
        g2.drawRoundRect(sidebarX, sidebarY, sidebarWidth, getHeight() - sidebarY - 20, 15, 15);

        int currentY = sidebarY + padding;

        // Header
        g2.setFont(new Font("Arial", Font.BOLD, 22));
        g2.setColor(new Color(255, 215, 0)); // Gold color
        currentY += 35;

        // Game timer section
        if (gameEndTime != null) {
            drawSectionHeader(g2, "SPIEL-TIMER", sidebarX + padding, currentY);
            currentY += 22;

            g2.setFont(new Font("Arial", Font.BOLD, 16));
            g2.setColor(new Color(255, 200, 100));
            String timeRemaining = formatTimeRemaining(gameEndTime);
            g2.drawString(timeRemaining, sidebarX + padding + 10, currentY);
            currentY += 25;
        }

        // Divider
        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += 15;

        // Current turn info
        List<Player> allPlayers = (players != null && !players.isEmpty()) ? players :
                (board != null && board.getPlayers() != null) ? board.getPlayers() : List.of();

        if (!allPlayers.isEmpty() && board != null) {
            Player currentTurnPlayer = allPlayers.get(board.getCurrentPlayerIndex());

            drawSectionHeader(g2, "AKTUELLER ZUG", sidebarX + padding, currentY);
            currentY += 25;

            // Player name with larger font
            g2.setFont(new Font("Arial", Font.BOLD, 18));
            g2.setColor(new Color(255, 255, 150));
            String turnText = currentTurnPlayer.getName();
            if (currentTurnPlayer.isAiControlled()) {
                turnText += " [AI]";
            }
            g2.drawString( turnText, sidebarX + padding + 10, currentY);
            currentY += 28;

            // Turn state - use server TurnState if available, otherwise client MoveState
            g2.setFont(new Font("Arial", Font.PLAIN, 11));
            g2.setColor(new Color(180, 180, 200));
            String stateText;
            if (currentTurnState != null) {
                stateText = switch (currentTurnState) {
                    case WAITING_FOR_PUSH -> "Waiting for tile push";
                    case WAITING_FOR_MOVE -> "Waiting for pawn move";
                };
            } else {
                stateText = board.getCurrentMoveState() == null ? "WAITING" :
                        board.getCurrentMoveState().toString().replace("_", " ");
            }
            g2.drawString(stateText, sidebarX + padding + 10, currentY);
            currentY += 18;

            // Turn timer
            if (turnEndTime != null) {
                String turnTime = formatTimeRemaining(turnEndTime);
                g2.setFont(new Font("Arial", Font.BOLD, 12));
                g2.setColor(new Color(255, 150, 150));
                g2.drawString("Zeit: " + turnTime, sidebarX + padding + 10, currentY);
                currentY += 20;
            } else {
                currentY += 7;
            }

            // Hint for staying in place
            if (currentTurnState != null && currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_MOVE) {
                g2.setFont(new Font("Arial", Font.ITALIC, 10));
                g2.setColor(new Color(150, 150, 170));
                g2.drawString("(Click your tile to stay in place)", sidebarX + padding + 10, currentY);
                currentY += 15;
            }
        }

        // Divider
        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += 15;

        // Players section
        drawSectionHeader(g2, "SPIELER", sidebarX + padding, currentY);
        currentY += 25;

        // Draw each player
        for (int i = 0; i < allPlayers.size(); i++) {
            Player p = allPlayers.get(i);
            boolean isCurrentTurn = (board != null && i == board.getCurrentPlayerIndex());

            currentY = drawPlayerCard(g2, p, sidebarX + padding, currentY, sidebarWidth - 2 * padding, isCurrentTurn, i);
            currentY += 12;
        }

        // Current player's treasure cards at the bottom
        if (currentPlayer != null && currentPlayer.getAssignedTreasureCards() != null &&
                !currentPlayer.getAssignedTreasureCards().isEmpty()) {

            currentY += 10;
            drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
            currentY += 15;

            drawSectionHeader(g2, "DEINE ZIELE", sidebarX + padding, currentY);
            currentY += 22;

            // Draw CURRENT TARGET treasure prominently
            Treasure currentTarget = currentPlayer.getAssignedTreasureCards().get(0);

            // Current target box with pulsing effect
            long time = System.currentTimeMillis();
            int pulseAlpha = 200 + (int) (55 * Math.sin(time / 400.0));

            // Background box for current target - make it taller for image
            int boxHeight = 70;
            g2.setColor(new Color(255, 215, 0, pulseAlpha));
            g2.fillRoundRect(sidebarX + padding + 5, currentY - 15, sidebarWidth - 2 * padding - 10, boxHeight, 10, 10);

            // Border
            g2.setColor(new Color(255, 255, 255, 200));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(sidebarX + padding + 5, currentY - 15, sidebarWidth - 2 * padding - 10, boxHeight, 10, 10);

            // "AKTUELLES ZIEL" label
            g2.setFont(new Font("Arial", Font.BOLD, 10));
            g2.setColor(new Color(100, 70, 0));
            g2.drawString("AKTUELLES ZIEL:", sidebarX + padding + 15, currentY - 2);

            // Draw treasure image
            BufferedImage treasureImg = treasureImages.get(currentTarget.getName());
            int imgSize = 40;
            int imgX = sidebarX + padding + 15;
            int imgY = currentY + 5;

            if (treasureImg != null) {
                g2.drawImage(treasureImg, imgX, imgY, imgSize, imgSize, null);
            } else {
                // Fallback circle
                g2.setColor(new Color(180, 140, 70));
                g2.fillOval(imgX, imgY, imgSize, imgSize);
            }

            // Current target name next to image
            g2.setFont(new Font("Arial", Font.BOLD, 16));
            g2.setColor(new Color(0, 0, 0));
            g2.drawString(currentTarget.getName(), imgX + imgSize + 10, imgY + imgSize / 2 + 5);

            currentY += boxHeight - 5;

            // Draw remaining treasure cards (if any)
            if (currentPlayer.getAssignedTreasureCards().size() > 1) {
                currentY += 10;
                g2.setFont(new Font("Arial", Font.ITALIC, 11));
                g2.setColor(new Color(180, 180, 200));
                g2.drawString("Weitere Ziele:", sidebarX + padding + 10, currentY);
                currentY += 18;

                for (int i = 1; i < currentPlayer.getAssignedTreasureCards().size(); i++) {
                    Treasure card = currentPlayer.getAssignedTreasureCards().get(i);
                    g2.setFont(new Font("Arial", Font.PLAIN, 11));
                    g2.setColor(new Color(200, 200, 220));
                    g2.drawString("  • " + card.getName(), sidebarX + padding + 10, currentY);
                    currentY += 16;
                }
            }
        }

        // Add keyboard hints at the bottom
        currentY = Math.max(currentY + 10, getHeight() - 80);
        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += 15;

        g2.setFont(new Font("Arial", Font.ITALIC, 10));
        g2.setColor(new Color(150, 150, 170));
        g2.drawString("Pfeiltasten: Navigation", sidebarX + padding, currentY);
        currentY += 15;
        g2.drawString("R/Q/E: Tile drehen", sidebarX + padding, currentY);
        currentY += 15;
        g2.drawString("Tab: Tastaturhilfe", sidebarX + padding, currentY);
    }

    /**
     * Draws a large overlay showing the current target treasure at the top of the screen
     */
    /**
     * Draws a golden highlight on the target treasure tile
     */
    private void drawCurrentTargetOverlay(Graphics2D g2) {
        if (currentPlayer == null || currentPlayer.getAssignedTreasureCards() == null
                || currentPlayer.getAssignedTreasureCards().isEmpty()) {
            return;
        }

        Treasure currentTarget = currentPlayer.getAssignedTreasureCards().get(0);

        // Find the position of the target treasure on the board
        int targetRow = -1;
        int targetCol = -1;

        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Tile tile = board.getTiles()[row][col];
                if (tile != null && tile.getTreasure() != null
                        && tile.getTreasure().getName() != null
                        && tile.getTreasure().getName().equals(currentTarget.getName())) {
                    targetRow = row;
                    targetCol = col;
                    break;
                }
            }
            if (targetRow >= 0) break;
        }

        // Pulsing effect
        long time = System.currentTimeMillis();

        // Draw the golden border on the target tile
        if (targetRow >= 0 && targetCol >= 0) {
            int tileX = xOffset + targetCol * size;
            int tileY = yOffset + targetRow * size;

            // Pulsing tile highlight
            int highlightAlpha = 150 + (int) (50 * Math.sin(time / 400.0));
            g2.setColor(new Color(255, 215, 0, highlightAlpha));
            g2.setStroke(new BasicStroke(6));
            g2.drawRoundRect(tileX - 3, tileY - 3, size + 6, size + 6, 12, 12);

            // Draw thicker inner border
            g2.setColor(new Color(255, 255, 0, highlightAlpha + 50));
            g2.setStroke(new BasicStroke(3));
            g2.drawRoundRect(tileX - 6, tileY - 6, size + 12, size + 12, 15, 15);
        }

        // Banner removed - target info is shown in sidebar "DEINE ZIELE" section
    }

    /**
     * Draws a section header with consistent styling
     */
    private void drawSectionHeader(Graphics2D g2, String text, int x, int y) {
        g2.setFont(new Font("Arial", Font.BOLD, 13));
        g2.setColor(new Color(180, 200, 255));
        g2.drawString(text, x, y);
    }

    /**
     * Draws a visual divider line
     */
    private void drawDivider(Graphics2D g2, int x1, int x2, int y) {
        // Draw gradient divider
        GradientPaint dividerGradient = new GradientPaint(
                x1, y,
                new Color(100, 130, 180, 50),
                (x1 + x2) / 2, y,
                new Color(100, 130, 180, 180)
        );
        g2.setPaint(dividerGradient);
        g2.setStroke(new BasicStroke(2));
        g2.drawLine(x1, y, x2, y);
    }

    private int drawPlayerCard(Graphics2D g2, Player player, int x, int y, int width, boolean isCurrentTurn, int playerIndex) {
        int cardHeight = 95;
        int padding = 10;

        // Card background
        Color bgColor = isCurrentTurn ? new Color(80, 100, 140, 200) : new Color(50, 50, 60, 180);
        g2.setColor(bgColor);
        g2.fillRoundRect(x, y, width, cardHeight, 10, 10);

        // Border for current turn
        if (isCurrentTurn) {
            g2.setColor(new Color(255, 220, 100));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(x, y, width, cardHeight, 10, 10);
        }

        int currentY = y + padding + 15;

        // Player icon (left side)
        if (playerIndex < playerIcons.size() && playerIcons.get(playerIndex) != null) {
            BufferedImage icon = playerIcons.get(playerIndex);
            int iconSize = 32;
            g2.drawImage(icon, x + padding, y + padding, iconSize, iconSize, null);
        } else {
            // Fallback: Color indicator circle
            if (player.getColor() != null) {
                Color playerColor = getAwtColor(player.getColor());
                g2.setColor(playerColor);
                g2.fillOval(x + padding, y + padding + 3, 24, 24);
                g2.setColor(Color.WHITE);
                g2.setStroke(new BasicStroke(1.5f));
                g2.drawOval(x + padding, y + padding + 3, 24, 24);
            }
        }

        // Player name
        g2.setFont(new Font("Arial", Font.BOLD, 14));
        g2.setColor(Color.WHITE);
        String name = player.getName();
        if (name.length() > 15) {
            name = name.substring(0, 12) + "...";
        }
        g2.drawString(name, x + padding + 42, currentY);

        // Badges (admin, AI, disconnected)
        int badgeX = x + width - padding - 20;
        g2.setFont(new Font("Arial", Font.PLAIN, 10));

        // Show AI badge or OFFLINE badge (but not both - AI bots don't need connections)
        if (player.isAiControlled()) {
            g2.setColor(new Color(150, 150, 255));
            g2.drawString("AI", badgeX, currentY);
        } else if (!player.isConnected()) {
            g2.setColor(new Color(200, 80, 80));
            g2.drawString("OFFLINE", badgeX - 40, currentY);
        }

        if (player.isAdmin()) {
            g2.setColor(new Color(255, 215, 0));
            g2.drawString("★", x + width - padding - 5, currentY);
        }

        currentY += 20;

        // Score: treasures found / total
        int treasuresFound = player.getTreasuresFound() != null ? player.getTreasuresFound().size() : 0;
        int totalTreasures = treasuresFound + player.getRemainingTreasureCount();

        g2.setFont(new Font("Arial", Font.PLAIN, 12));
        g2.setColor(new Color(200, 200, 220));
        g2.drawString("Treasures: " + treasuresFound + "/" + totalTreasures, x + padding + 24, currentY);
        currentY += 18;

        // Progress bar
        if (totalTreasures > 0) {
            int barWidth = width - 2 * padding - 24;
            int barHeight = 8;
            int barX = x + padding + 24;

            // Background
            g2.setColor(new Color(60, 60, 70));
            g2.fillRoundRect(barX, currentY - 6, barWidth, barHeight, 4, 4);

            // Progress
            int progressWidth = (int) ((double) treasuresFound / totalTreasures * barWidth);
            if (progressWidth > 0) {
                g2.setColor(new Color(100, 200, 100));
                g2.fillRoundRect(barX, currentY - 6, progressWidth, barHeight, 4, 4);
            }
        }

        return y + cardHeight;
    }

    private Color getAwtColor(labyrinth.contracts.models.PlayerColor playerColor) {
        return switch (playerColor) {
            case RED -> new Color(220, 80, 80);
            case BLUE -> new Color(80, 140, 220);
            case GREEN -> new Color(80, 200, 120);
            case YELLOW -> new Color(230, 200, 80);
        };
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

    // =================================================================================
    // TOAST NOTIFICATIONS (Public API)
    // =================================================================================

    /**
     * Shows an error toast with structured error code
     */
    public void showErrorToast(String errorCode, String title, String message) {
        toastManager.showError(errorCode, title, message);
    }

    /**
     * Shows a warning toast
     */
    public void showWarningToast(String id, String title, String message) {
        toastManager.showWarning(id, title, message);
    }

    /**
     * Shows an info toast
     */
    public void showInfoToast(String id, String title, String message) {
        toastManager.showInfo(id, title, message);
    }

    /**
     * Shows a success toast
     */
    public void showSuccessToast(String id, String title, String message) {
        toastManager.showSuccess(id, title, message);
    }

    // =================================================================================
    // SETTER (aus Server-Events)
    // =================================================================================

    public void setBoard(Board board) {
        this.board = Objects.requireNonNull(board, "board must not be null");

        // Detect board changes for notifications
        detectAndNotifyBoardChanges(previousBoard, board);

        // Store current board for next comparison
        this.previousBoard = copyBoardState(board);

        // Unlock nach Server-Update
        this.inputLocked = false;

        repaint();
    }

    /**
     * Detects changes between previous and current board state and shows notifications
     */
    private void detectAndNotifyBoardChanges(Board previous, Board current) {
        if (previous == null || current == null) return;

        // Check if board size changed (shouldn't happen, but defensive)
        if (previous.getWidth() != current.getWidth() || previous.getHeight() != current.getHeight()) {
            return;
        }

        // Check for tile shifts by comparing positions
        int shiftedRow = -1;
        int shiftedCol = -1;

        // Check each position for changes
        for (int row = 0; row < current.getHeight(); row++) {
            for (int col = 0; col < current.getWidth(); col++) {
                Tile prevTile = previous.getTiles()[row][col];
                Tile currTile = current.getTiles()[row][col];

                // Compare tile entrances to detect if board shifted
                if (!tilesEqual(prevTile, currTile)) {
                    // Board changed - likely a shift occurred
                    // We can't easily determine which row/col was shifted without more data
                    // So we'll skip detailed shift notifications for now
                    return;
                }
            }
        }
    }

    private boolean tilesEqual(Tile t1, Tile t2) {
        if (t1 == null && t2 == null) return true;
        if (t1 == null || t2 == null) return false;

        Direction[] e1 = t1.getEntrances();
        Direction[] e2 = t2.getEntrances();

        if (e1 == null && e2 == null) return true;
        if (e1 == null || e2 == null) return false;
        if (e1.length != e2.length) return false;

        EnumSet<Direction> set1 = EnumSet.noneOf(Direction.class);
        EnumSet<Direction> set2 = EnumSet.noneOf(Direction.class);

        Collections.addAll(set1, e1);
        Collections.addAll(set2, e2);

        return set1.equals(set2);
    }

    private Board copyBoardState(Board board) {
        // Simple shallow copy for comparison purposes
        // We only need to track tile changes, not deep clone
        return board;
    }

    public void setPlayers(List<Player> players) {
        this.players = players != null ? players : List.of();
        System.out.println("[BoardPanel] setPlayers called with " + this.players.size() + " players:");
        for (int i = 0; i < this.players.size(); i++) {
            Player p = this.players.get(i);
            System.out.println("  [" + i + "] " + p.getName() + " at " + p.getCurrentPosition());
        }
        repaint();
    }

    public void setCurrentPlayer(Player currentPlayer) {
        this.currentPlayer = currentPlayer;

        // Initialize keyboard selection to current player's position
        if (currentPlayer != null && currentPlayer.getCurrentPosition() != null) {
            selectedRow = currentPlayer.getCurrentPosition().getRow();
            selectedCol = currentPlayer.getCurrentPosition().getColumn();
        }

        // Check if target treasure has changed and show toast
        if (currentPlayer != null && currentPlayer.getAssignedTreasureCards() != null
                && !currentPlayer.getAssignedTreasureCards().isEmpty()) {

            Treasure currentTarget = currentPlayer.getAssignedTreasureCards().get(0);
            String currentTargetName = currentTarget.getName();

            if (lastTargetTreasureName == null || !lastTargetTreasureName.equals(currentTargetName)) {
                // Target changed or first time - show toast and banner
                showNewTargetToast(currentTargetName);
                lastTargetTreasureName = currentTargetName;
                showTargetBanner = true; // Show banner until first action
            }
        }

        repaint();
    }

    private void showNewTargetToast(String treasureName) {
        String message = "Bewege dich zum Feld mit diesem Schatz!";
        toastManager.showInfo("TARGET", "Dein Ziel: " + treasureName, message);
    }

    /**
     * Reachable Tiles werden idealerweise aus einem Server-Payload gesetzt.
     * Du musst dazu deine Event-Payloads erweitern (z.B. GAME_STATE_UPDATE).
     */
    public void setReachableTiles(Collection<Tile> tiles) {
        reachableTiles.clear();
        if (tiles != null) reachableTiles.addAll(tiles);
        repaint();
    }

    public void setGameEndTime(java.time.OffsetDateTime gameEndTime) {
        this.gameEndTime = gameEndTime;
        repaint();
    }

    public void setTurnEndTime(java.time.OffsetDateTime turnEndTime) {
        this.turnEndTime = turnEndTime;
        repaint();
    }

    public void setCurrentTurnState(labyrinth.contracts.models.TurnState currentTurnState) {
        this.currentTurnState = currentTurnState;
        repaint();
    }

    /**
     * Unlocks input after an error occurs.
     * This allows the player to retry their action after receiving an error message.
     */
    public void unlockInput() {
        this.inputLocked = false;
    }


    private void showOptionsDialog() {
        // Farben
        Color CARD_BG = new Color(35, 32, 28, 240);
        Color CARD_BORDER = new Color(100, 85, 60);
        Color PRIMARY_GOLD = new Color(218, 165, 32);
        Color PRIMARY_GOLD_LIGHT = new Color(255, 215, 0);
        Color TEXT_LIGHT = new Color(255, 248, 230);
        Color TEXT_MUTED = new Color(180, 170, 155);
        Color STONE_DARK = new Color(45, 42, 38);

        JDialog dialog = new JDialog(
                SwingUtilities.getWindowAncestor(this),
                "Optionen",
                Dialog.ModalityType.APPLICATION_MODAL
        );
        dialog.setUndecorated(true);
        dialog.setBackground(new Color(0, 0, 0, 0));

        // Main panel with custom painting
        JPanel mainPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Background
                g2.setColor(CARD_BG);
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 20, 20);

                // Border
                g2.setColor(CARD_BORDER);
                g2.setStroke(new BasicStroke(3));
                g2.drawRoundRect(1, 1, getWidth() - 3, getHeight() - 3, 20, 20);

                // Top highlight
                g2.setColor(new Color(255, 255, 255, 15));
                g2.fillRoundRect(2, 2, getWidth() - 4, 50, 18, 18);

                g2.dispose();
                super.paintComponent(g);
            }
        };
        mainPanel.setOpaque(false);
        mainPanel.setLayout(new BorderLayout(0, 15));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(25, 30, 25, 30));

        // Title
        JLabel titleLabel = new JLabel("⚙ Optionen");
        titleLabel.setFont(new Font("Serif", Font.BOLD, 24));
        titleLabel.setForeground(PRIMARY_GOLD_LIGHT);
        titleLabel.setHorizontalAlignment(SwingConstants.CENTER);
        mainPanel.add(titleLabel, BorderLayout.NORTH);

        // Content panel
        JPanel contentPanel = new JPanel();
        contentPanel.setOpaque(false);
        contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));

        // Lade aktuelle Einstellungen
        java.util.prefs.Preferences prefs = java.util.prefs.Preferences.userNodeForPackage(getClass());
        int currentMusicVolume = prefs.getInt("musicVolume", 50);
        int currentSfxVolume = prefs.getInt("sfxVolume", 70);
        boolean darkTheme = prefs.getBoolean("darkTheme", true);

        // === AUDIO SECTION ===
        JPanel audioSection = createOptionsSection("🔊 Audio", CARD_BG, CARD_BORDER, PRIMARY_GOLD_LIGHT);
        JPanel audioContent = new JPanel(new GridBagLayout());
        audioContent.setOpaque(false);
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 10, 8, 10);

        // Music Volume
        gbc.gridx = 0; gbc.gridy = 0; gbc.weightx = 0.3;
        JLabel musicLabel = new JLabel("Musik:");
        musicLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        musicLabel.setForeground(TEXT_LIGHT);
        audioContent.add(musicLabel, gbc);

        gbc.gridx = 1; gbc.weightx = 0.5;
        JSlider musicSlider = new JSlider(0, 100, currentMusicVolume);
        musicSlider.setOpaque(false);
        musicSlider.setPreferredSize(new Dimension(180, 30));
        audioContent.add(musicSlider, gbc);

        gbc.gridx = 2; gbc.weightx = 0.2;
        JLabel musicValueLabel = new JLabel(currentMusicVolume + "%");
        musicValueLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        musicValueLabel.setForeground(PRIMARY_GOLD);
        musicValueLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        audioContent.add(musicValueLabel, gbc);

        musicSlider.addChangeListener(e -> {
            int vol = musicSlider.getValue();
            musicValueLabel.setText(vol + "%");
            backgroundMusic.setMusicVolume(vol / 100.0f);
        });

        // SFX Volume
        gbc.gridx = 0; gbc.gridy = 1;
        JLabel sfxLabel = new JLabel("Effekte:");
        sfxLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        sfxLabel.setForeground(TEXT_LIGHT);
        audioContent.add(sfxLabel, gbc);

        gbc.gridx = 1;
        JSlider sfxSlider = new JSlider(0, 100, currentSfxVolume);
        sfxSlider.setOpaque(false);
        sfxSlider.setPreferredSize(new Dimension(180, 30));
        audioContent.add(sfxSlider, gbc);

        gbc.gridx = 2;
        JLabel sfxValueLabel = new JLabel(currentSfxVolume + "%");
        sfxValueLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        sfxValueLabel.setForeground(PRIMARY_GOLD);
        sfxValueLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        audioContent.add(sfxValueLabel, gbc);

        sfxSlider.addChangeListener(e -> {
            int vol = sfxSlider.getValue();
            sfxValueLabel.setText(vol + "%");
            soundEffects.setVolume(vol / 100.0f);
        });

        audioSection.add(audioContent, BorderLayout.CENTER);
        contentPanel.add(audioSection);
        contentPanel.add(Box.createVerticalStrut(15));

        // === THEME SECTION ===
        JPanel themeSection = createOptionsSection("🎨 Darstellung", CARD_BG, CARD_BORDER, PRIMARY_GOLD_LIGHT);
        JPanel themeContent = new JPanel(new FlowLayout(FlowLayout.LEFT, 15, 10));
        themeContent.setOpaque(false);

        JLabel themeLabel = new JLabel("Theme:");
        themeLabel.setFont(new Font("SansSerif", Font.BOLD, 14));
        themeLabel.setForeground(TEXT_LIGHT);
        themeContent.add(themeLabel);

        JToggleButton themeToggle = new JToggleButton() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                int w = getWidth(), h = getHeight();
                g2.setColor(isSelected() ? new Color(60, 60, 80) : new Color(200, 180, 140));
                g2.fillRoundRect(0, 0, w, h, h, h);
                g2.setColor(CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, w - 2, h - 2, h - 2, h - 2);
                int knobSize = h - 6;
                int knobX = isSelected() ? w - knobSize - 3 : 3;
                g2.setColor(isSelected() ? new Color(100, 100, 140) : new Color(255, 220, 120));
                g2.fillOval(knobX, 3, knobSize, knobSize);
                g2.setFont(new Font("SansSerif", Font.PLAIN, 12));
                g2.setColor(TEXT_LIGHT);
                g2.drawString(isSelected() ? "🌙" : "☀", knobX + 4, h - 7);
                g2.dispose();
            }
        };
        themeToggle.setSelected(darkTheme);
        themeToggle.setPreferredSize(new Dimension(60, 28));
        themeToggle.setOpaque(false);
        themeToggle.setBorderPainted(false);
        themeToggle.setFocusPainted(false);
        themeToggle.setCursor(new Cursor(Cursor.HAND_CURSOR));
        themeContent.add(themeToggle);

        JLabel themeStatusLabel = new JLabel(darkTheme ? "Dunkel" : "Hell");
        themeStatusLabel.setFont(new Font("SansSerif", Font.PLAIN, 14));
        themeStatusLabel.setForeground(TEXT_LIGHT);
        themeContent.add(themeStatusLabel);

        themeToggle.addActionListener(e -> themeStatusLabel.setText(themeToggle.isSelected() ? "Dunkel" : "Hell"));

        themeSection.add(themeContent, BorderLayout.CENTER);
        contentPanel.add(themeSection);

        mainPanel.add(contentPanel, BorderLayout.CENTER);

        // === BUTTONS ===
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 15, 0));
        buttonPanel.setOpaque(false);

        // Exit Button
        JButton exitButton = createStyledDialogButton("🚪 Spiel beenden", new Color(120, 50, 50), new Color(180, 80, 80));
        exitButton.addActionListener(e -> {
            int choice = JOptionPane.showConfirmDialog(dialog,
                    "Möchtest du das Spiel wirklich beenden?\nDein Fortschritt geht verloren.",
                    "Spiel beenden?",
                    JOptionPane.YES_NO_OPTION,
                    JOptionPane.WARNING_MESSAGE);
            if (choice == JOptionPane.YES_OPTION) {
                dialog.dispose();
                if (onExitGame != null) {
                    onExitGame.run();
                }
            }
        });
        buttonPanel.add(exitButton);

        // Save & Close Button
        JButton saveButton = createStyledDialogButton("💾 Speichern & Schließen", new Color(60, 100, 60), new Color(100, 160, 100));
        saveButton.addActionListener(e -> {
            // Einstellungen speichern
            prefs.putInt("musicVolume", musicSlider.getValue());
            prefs.putInt("sfxVolume", sfxSlider.getValue());
            prefs.putBoolean("darkTheme", themeToggle.isSelected());
            dialog.dispose();
            toastManager.showSuccess("SETTINGS", "Gespeichert", "Einstellungen wurden gespeichert");
        });
        buttonPanel.add(saveButton);

        // Close Button
        JButton closeButton = createStyledDialogButton("✕ Schließen", STONE_DARK, new Color(90, 80, 70));
        closeButton.addActionListener(e -> dialog.dispose());
        buttonPanel.add(closeButton);

        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        dialog.setContentPane(mainPanel);
        dialog.setSize(420, 380);
        dialog.setLocationRelativeTo(this);
        dialog.setVisible(true);
    }

    private JPanel createOptionsSection(String title, Color bgColor, Color borderColor, Color titleColor) {
        JPanel section = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setColor(new Color(bgColor.getRed(), bgColor.getGreen(), bgColor.getBlue(), 150));
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 12, 12);
                g2.setColor(borderColor);
                g2.setStroke(new BasicStroke(1.5f));
                g2.drawRoundRect(0, 0, getWidth() - 1, getHeight() - 1, 12, 12);
                g2.dispose();
                super.paintComponent(g);
            }
        };
        section.setOpaque(false);
        section.setLayout(new BorderLayout(0, 8));
        section.setBorder(BorderFactory.createEmptyBorder(12, 15, 12, 15));

        JLabel titleLabel = new JLabel(title);
        titleLabel.setFont(new Font("Serif", Font.BOLD, 16));
        titleLabel.setForeground(titleColor);
        section.add(titleLabel, BorderLayout.NORTH);

        return section;
    }

    private JButton createStyledDialogButton(String text, Color bgColor, Color borderColor) {
        JButton btn = new JButton(text) {
            private boolean isHovered = false;
            {
                addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseEntered(MouseEvent e) { isHovered = true; repaint(); }
                    @Override
                    public void mouseExited(MouseEvent e) { isHovered = false; repaint(); }
                });
            }
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                int w = getWidth(), h = getHeight();

                Color bg = isHovered ? borderColor : bgColor;
                g2.setColor(bg);
                g2.fillRoundRect(0, 0, w, h, 8, 8);

                g2.setColor(borderColor);
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, w - 2, h - 2, 8, 8);

                g2.setFont(new Font("SansSerif", Font.BOLD, 12));
                g2.setColor(new Color(255, 248, 230));
                FontMetrics fm = g2.getFontMetrics();
                int textX = (w - fm.stringWidth(getText())) / 2;
                int textY = (h + fm.getAscent() - fm.getDescent()) / 2;
                g2.drawString(getText(), textX, textY);
                g2.dispose();
            }
        };
        btn.setPreferredSize(new Dimension(130, 35));
        btn.setOpaque(false);
        btn.setContentAreaFilled(false);
        btn.setBorderPainted(false);
        btn.setFocusPainted(false);
        btn.setCursor(new Cursor(Cursor.HAND_CURSOR));
        return btn;
    }

    /**
     * Called when the game ends to prevent further input
     */
    public void setGameOver(boolean gameOver) {
        this.gameIsOver = gameOver;
        this.inputLocked = gameOver; // Also lock input
    }
}