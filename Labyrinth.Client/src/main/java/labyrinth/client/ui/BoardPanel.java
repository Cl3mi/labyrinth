package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.audio.SoundEffects;
import labyrinth.client.messaging.GameClient;
import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.extensions.TreasureUtils;
import labyrinth.client.ui.Styles.StyledContextMenu;
import labyrinth.client.ui.Styles.StyledDialog;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Direction;
import labyrinth.contracts.models.Tile;
import labyrinth.contracts.models.Treasure;
import lombok.Getter;
import lombok.Setter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
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
 */
public class BoardPanel extends JPanel {

    private static final int PANEL_PADDING = 20;
    private static final int ARROW_MARGIN = 5;

    private static final Font PLAYER_MARKER_FONT = new Font("Arial", Font.BOLD, 30);
    private static final Font COORDINATE_FONT = new Font("Arial", Font.PLAIN, 10);

    private static final Font FONT_SANSSERIF_BOLD_20 = new Font("SansSerif", Font.BOLD, 20);
    private static final Font FONT_ARIAL_BOLD_16 = new Font("Arial", Font.BOLD, 16);
    private static final Font FONT_ARIAL_BOLD_15 = new Font("Arial", Font.BOLD, 15);
    private static final Font FONT_ARIAL_BOLD_11 = new Font("Arial", Font.BOLD, 11);
    private static final Font FONT_ARIAL_BOLD_9 = new Font("Arial", Font.BOLD, 9);
    private static final Font FONT_ARIAL_PLAIN_12 = new Font("Arial", Font.PLAIN, 12);
    private static final Font FONT_ARIAL_PLAIN_11 = new Font("Arial", Font.PLAIN, 11);


    private static final Map<String, Font> fontCache = new HashMap<>();

    private static Font getCachedFont(String family, int style, int size) {
        String key = family + "_" + style + "_" + size;
        return fontCache.computeIfAbsent(key, k -> new Font(family, style, size));
    }

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

    private Board previousBoard;

    private Integer lastPushedIndex = null;
    private Boolean lastPushedWasRow = null;
    private Direction lastPushDirection = null;
    private long lastPushTimestamp = 0;
    private static final long PUSH_HIGHLIGHT_DURATION = 2000; // 2 seconds

    private int lastTargetTreasureId = -1;

    private int selectedRow = 0;
    private int selectedCol = 0;
    private boolean keyboardNavigationActive = false;

    private Player currentPlayer;
    private List<Player> players;

    private java.time.OffsetDateTime gameEndTime;
    private java.time.OffsetDateTime turnEndTime;
    private labyrinth.contracts.models.TurnState currentTurnState;
    private final Map<Integer, BufferedImage> treasureImages = new HashMap<>();
    private final Map<String, BufferedImage> tileImages = new HashMap<>();
    private BufferedImage bonusBagImage;

    private final Set<Tile> reachableTiles = new HashSet<>();
    private final List<ArrowButton> arrowButtons = new ArrayList<>();
    private ArrowButton hoveredArrow = null;

    private int xOffset;
    private int yOffset;
    private int size;
    private int arrowSize = 30;

    private final Random random = new Random();

    private final AudioPlayer backgroundMusic;

    private boolean inputLocked = false;

    // Flag to prevent commands after game is over
    private boolean gameIsOver = false;

    // Bonus system state
    private BonusType activeBonusMode = null;  // Currently active bonus selection mode
    private final List<Rectangle> bonusButtonBounds = new ArrayList<>();  // Clickable bonus button areas
    private int hoveredBonusIndex = -1;  // Currently hovered bonus button

    // AI toggle button state
    private Rectangle aiMoveButtonBounds = null;
    private boolean aiMoveButtonHovered = false;
    private boolean aiModeEnabled = false;
    private boolean aiThinking = false;


    @Setter
    private Runnable onAiToggleRequested;

    private static final int ANIMATION_FPS = 60;
    private static final int ANIMATION_INTERVAL = 1000 / ANIMATION_FPS; // ~16ms


    public BoardPanel(GameClient client, Board board, Player currentPlayer, List<Player> players) {
        this.client = Objects.requireNonNull(client, "client must not be null");
        this.board = Objects.requireNonNull(board, "board must not be null");
        this.currentPlayer = currentPlayer;
        this.players = players != null ? players : List.of();

        loadBackgroundImage();
        loadTileImages();
        loadTreasureImages();
        loadPlayerIcons();

        ThemeManager.getInstance().addThemeChangeListener(() -> {
            loadBackgroundImage();
            repaint();
        });

        setBackground(BACKGROUND_COLOR);
        setPreferredSize(new Dimension(1920, 1080));

        setLayout(null); // Overlay-Button

        JButton optionsButton = createStyledOptionsButton();
        add(optionsButton);


        this.toastManager = new ToastManager(this);
        this.soundEffects = new SoundEffects();

        setupMouseListener();
        setupKeyboardListener();

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                invalidateLayout();
            }
        });

        java.util.prefs.Preferences prefs = java.util.prefs.Preferences.userNodeForPackage(getClass());
        int savedMusicVolume = prefs.getInt("musicVolume", 50);
        int savedSfxVolume = prefs.getInt("sfxVolume", 70);

        backgroundMusic = AudioPlayer.getInstance();


        backgroundMusic.setMusicVolume(savedMusicVolume / 100.0f);
        backgroundMusic.playMenuMusic();

        soundEffects.setVolume(savedSfxVolume / 100.0f);

        javax.swing.Timer sidebarTimer = new javax.swing.Timer(1000, e -> {
            if (gameEndTime != null || turnEndTime != null) {
                repaintSidebarRegion();
            }
        });
        sidebarTimer.start();

        var animationTimer = new javax.swing.Timer(ANIMATION_INTERVAL, e -> {
            if (needsAnimationUpdate()) {
                repaint();
            }
        });
        animationTimer.start();
    }

    /**
     * Check if any animations are currently active and need smooth updates.
     * This includes: your turn indicators, target treasure highlights, AI thinking, etc.
     */
    private boolean needsAnimationUpdate() {
        if (isLocalPlayerTurn()) {
            return true;
        }

        if (currentPlayer != null && currentPlayer.getCurrentTargetTreasure() != null) {
            return true;
        }

        if (aiThinking) {
            return true;
        }

        if (keyboardNavigationActive) {
            return true;
        }

        if (lastPushedIndex != null) {
            long elapsed = System.currentTimeMillis() - lastPushTimestamp;
            if (elapsed <= PUSH_HIGHLIGHT_DURATION) {
                return true;
            }
        }

        return false;
    }

    @Setter
    private Runnable onExitGame;

    /**
     * Sets whether AI mode is enabled (toggle state).
     */
    public void setAiModeEnabled(boolean enabled) {
        this.aiModeEnabled = enabled;
        repaint();
    }

    /**
     * Sets whether AI is currently thinking.
     */
    public void setAiThinking(boolean thinking) {
        this.aiThinking = thinking;
        if (thinking) {
            inputLocked = true;
        }
        repaint();
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
                g2.setFont(FONT_SANSSERIF_BOLD_20);
                g2.setColor(new Color(255, 215, 0));
                FontMetrics fm = g2.getFontMetrics();
                String text = "⚙";
                int textX = (w - fm.stringWidth(text)) / 2;
                int textY = (h + fm.getAscent() - fm.getDescent()) / 2;
                g2.drawString(text, textX, textY);

                g2.dispose();
            }
        };
        btn.setBounds(10, 10, 45, 40);
        btn.setOpaque(false);
        btn.setContentAreaFilled(false);
        btn.setBorderPainted(false);
        btn.setFocusPainted(false);
        btn.setCursor(new Cursor(Cursor.HAND_CURSOR));
        btn.addActionListener(e -> showOptionsDialog());
        StyledTooltipManager.setTooltip(btn, "Optionen", "Einstellungen öffnen (Esc/P)");
        StyledContextMenu.attachTo(btn);
        return btn;
    }

    // =================================================================================
    // IMAGE LOADING
    // =================================================================================

    private void loadBackgroundImage() {
        try {
            String imagePath = ThemeManager.getInstance().getBackgroundImagePath();
            var url = getClass().getResource(imagePath);
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
                System.out.println("[BoardPanel] Loaded background: " + imagePath);
            } else {
                System.err.println("Background image not found: " + imagePath);
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
        Map<Integer, String> treasureFileMapping = new HashMap<>();

        treasureFileMapping.put(1, "Ghost");
        treasureFileMapping.put(2, "Dragon");
        treasureFileMapping.put(3, "Witch");
        treasureFileMapping.put(4, "Owl");
        treasureFileMapping.put(5, "Rat");
        treasureFileMapping.put(6, "Bug");
        treasureFileMapping.put(7, "Spider");
        treasureFileMapping.put(8, "Snake");
        treasureFileMapping.put(9, "Bat");
        treasureFileMapping.put(10, "Crown");
        treasureFileMapping.put(11, "Key");
        treasureFileMapping.put(12, "Treasure");
        treasureFileMapping.put(13, "Helmet");
        treasureFileMapping.put(14, "Book");
        treasureFileMapping.put(15, "Candle");
        treasureFileMapping.put(16, "Ring");
        treasureFileMapping.put(17, "Bag");
        treasureFileMapping.put(18, "Skull");
        treasureFileMapping.put(19, "Map");
        treasureFileMapping.put(20, "Sword");
        treasureFileMapping.put(21, "chalice");
        treasureFileMapping.put(22, "Diamond");
        treasureFileMapping.put(23, "Jug");
        treasureFileMapping.put(24, "Mouse");

        for (var entry : treasureFileMapping.entrySet()) {
            var treasureId = entry.getKey();
            var fileName = entry.getValue();

            var img = loadImage("/images/tiles/" + fileName + ".png");

            if (img != null) {
                treasureImages.put(treasureId, img);
                System.out.println("Loaded treasure: " + treasureId + " -> " + fileName);
            } else {
                System.err.println("Failed to load treasure: " + treasureId + " (file: " + fileName + ")");
            }
        }

        System.out.println("Loaded " + treasureImages.size() + "/24 treasure images");
    }


    private void loadTileImages() {
        tileImages.put("I", loadImage("/images/tiles/I_tile.png"));
        tileImages.put("L", loadImage("/images/tiles/L_tile.png"));
        tileImages.put("T", loadImage("/images/tiles/T_tile.png"));

        bonusBagImage = loadImage("/images/tiles/BonusBag.png");

        System.out.println("Loaded tile images:");
        tileImages.forEach((type, img) -> {
            if (img != null) {
                System.out.println("  " + type + " tile");
            } else {
                System.err.println("  " + type + " tile FEHLT!");
            }
        });
        if (bonusBagImage != null) {
            System.out.println("  BonusBag loaded");
        } else {
            System.err.println("  BonusBag FEHLT!");
        }
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

        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.DOWN)) return new TileImageInfo("I", 0);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.RIGHT)) return new TileImageInfo("I", 90);
        }

        if (dirs.size() == 2) {
            if (dirs.contains(Direction.UP) && dirs.contains(Direction.RIGHT)) return new TileImageInfo("L", 0);
            if (dirs.contains(Direction.RIGHT) && dirs.contains(Direction.DOWN)) return new TileImageInfo("L", 90);
            if (dirs.contains(Direction.DOWN) && dirs.contains(Direction.LEFT)) return new TileImageInfo("L", 180);
            if (dirs.contains(Direction.LEFT) && dirs.contains(Direction.UP)) return new TileImageInfo("L", 270);
        }

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
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
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


    private void setupMouseListener() {
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (gameIsOver) return;  // Don't allow any input if game is over
                if (inputLocked) return;
                if (board == null) return;

                if (e.getButton() == MouseEvent.BUTTON3) {
                    if (activeBonusMode != null) {
                        cancelBonusMode();
                    }
                    return;
                }

                if (handleBonusButtonClick(e.getPoint())) return;
                if (handleAiButtonClick(e.getPoint())) return;

                if (activeBonusMode != null) {
                    if (handleActiveBonusModeClick(e.getPoint())) return;
                }

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

                hoveredBonusIndex = -1;
                for (int i = 0; i < bonusButtonBounds.size(); i++) {
                    if (bonusButtonBounds.get(i).contains(e.getPoint())) {
                        hoveredBonusIndex = i;
                        break;
                    }
                }

                aiMoveButtonHovered = aiMoveButtonBounds != null && aiMoveButtonBounds.contains(e.getPoint());
                repaint();
            }
        });
    }

    private void setupKeyboardListener() {
        setFocusable(true);
        requestFocusInWindow();

        addKeyListener(new java.awt.event.KeyAdapter() {
            @Override
            public void keyPressed(java.awt.event.KeyEvent e) {
                if (gameIsOver && e.getKeyCode() != java.awt.event.KeyEvent.VK_ESCAPE) return;
                handleKeyPress(e);
            }
        });

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
            case java.awt.event.KeyEvent.VK_R, java.awt.event.KeyEvent.VK_E -> {
                if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
                    soundEffects.playRotate();
                    client.sendRotateTile();
                    inputLocked = true;
                }
            }
            case java.awt.event.KeyEvent.VK_Q -> {
                if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
                    soundEffects.playRotate();
                    client.sendRotateTile();
                    try { Thread.sleep(50); } catch (InterruptedException ignored) {}
                    client.sendRotateTile();
                    try { Thread.sleep(50); } catch (InterruptedException ignored) {}
                    client.sendRotateTile();
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
                    inputLocked = true;
                    keyboardNavigationActive = false;
                } else if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
                    toastManager.showInfo("INFO", "Hinweis", "Erst Tile einschieben, dann Figur bewegen");
                }
            }

            // Escape - cancel bonus mode or show options
            case java.awt.event.KeyEvent.VK_ESCAPE -> {
                if (activeBonusMode != null) {
                    cancelBonusMode();
                } else {
                    showOptionsDialog();
                }
            }

            // P - pause/options
            case java.awt.event.KeyEvent.VK_P -> {
                showOptionsDialog();
            }

            // Tab or H - toggle keyboard navigation help
            case java.awt.event.KeyEvent.VK_TAB, java.awt.event.KeyEvent.VK_H -> {
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

            P / Esc: Optionen/Pause
            H / Tab: Diese Hilfe anzeigen
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
                    inputLocked = true;
                    break outer;
                }
            }
        }
    }

    // =================================================================================
    // BONUS HANDLING
    // =================================================================================

    /**
     * Handle click on bonus buttons in the sidebar.
     * @return true if a bonus button was clicked
     */
    private boolean handleBonusButtonClick(Point p) {
        if (currentPlayer == null) return false;

        List<BonusType> bonuses = currentPlayer.getAvailableBonuses();
        if (bonuses.isEmpty()) return false;

        // Check if it's the local player's turn
        if (!isLocalPlayerTurn()) {
            for (int i = 0; i < bonusButtonBounds.size() && i < bonuses.size(); i++) {
                if (bonusButtonBounds.get(i).contains(p)) {
                    toastManager.showWarning("NOT_TURN", "Nicht an der Reihe",
                            "Du kannst Boni nur während deines Zuges verwenden!");
                    return true;
                }
            }
            return false;
        }

        // Check if we're in WAITING_FOR_MOVE state (tile already pushed)
        if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_MOVE) {
            for (int i = 0; i < bonusButtonBounds.size() && i < bonuses.size(); i++) {
                if (bonusButtonBounds.get(i).contains(p)) {
                    toastManager.showWarning("BONUS_AFTER_PUSH", "Bonus nicht mehr verfügbar",
                            "Boni können nur VOR dem Schieben verwendet werden!");
                    return true;
                }
            }
            return false;
        }

        for (int i = 0; i < bonusButtonBounds.size() && i < bonuses.size(); i++) {
            if (bonusButtonBounds.get(i).contains(p)) {
                BonusType clickedBonus = bonuses.get(i);
                handleBonusActivation(clickedBonus);
                return true;
            }
        }
        return false;
    }

    /**
     * Check if it's the local player's turn.
     */
    private boolean isLocalPlayerTurn() {
        if (board == null || currentPlayer == null || players == null || players.isEmpty()) {
            return false;
        }
        int currentIdx = board.getCurrentPlayerIndex();
        if (currentIdx < 0 || currentIdx >= players.size()) {
            return false;
        }
        Player currentTurnPlayer = players.get(currentIdx);
        return currentTurnPlayer != null &&
               currentTurnPlayer.getId() != null &&
               currentTurnPlayer.getId().equals(currentPlayer.getId());
    }

    /**
     * Handle click on the AI toggle button.
     * @return true if the AI button was clicked
     */
    private boolean handleAiButtonClick(Point p) {
        if (aiMoveButtonBounds == null || !aiMoveButtonBounds.contains(p)) {
            return false;
        }
        // Toggle is always allowed (even when not your turn or AI is thinking)
        if (onAiToggleRequested != null) {
            soundEffects.playMove();
            onAiToggleRequested.run();
        }
        return true;
    }

    /**
     * Activate a bonus - either immediately send command or enter selection mode.
     */
    private void handleBonusActivation(BonusType bonus) {
        soundEffects.playMove();

        switch (bonus) {
            case PUSH_TWICE -> {
                // PUSH_TWICE activates immediately - server handles the double push state
                client.sendUsePushTwice();
                toastManager.showInfo("BONUS", "Push Twice aktiviert",
                        "Du kannst jetzt zweimal hintereinander schieben!");
                inputLocked = true;
            }
            case BEAM -> {
                // Enter beam selection mode - player clicks on target tile
                activeBonusMode = BonusType.BEAM;
                toastManager.showInfo("BONUS", "Beam aktiviert",
                        "Klicke auf ein beliebiges Feld um dich dorthin zu teleportieren!");
            }
            case SWAP -> {
                // Show player selection dialog for swap
                showSwapPlayerDialog();
            }
            case PUSH_FIXED -> {
                // Check if there are any valid fixed rows/columns to push
                if (!hasValidPushFixedTargets()) {
                    toastManager.showError("BONUS", "Nicht möglich",
                            "Keine fixierten Reihen/Spalten zum Schieben verfügbar auf diesem Spielfeld!");
                    return;
                }
                // Enter push fixed mode - player clicks on an arrow to push fixed row/column
                activeBonusMode = BonusType.PUSH_FIXED;
                toastManager.showInfo("BONUS", "Push Fixed aktiviert",
                        "Klicke auf einen goldenen Pfeil! (Rechtsklick oder ESC zum Abbrechen)");
            }
        }
        repaint();
    }

    /**
     * Handle clicks when a bonus selection mode is active.
     * @return true if the click was handled by the bonus mode
     */
    private boolean handleActiveBonusModeClick(Point p) {
        if (activeBonusMode == null) return false;

        switch (activeBonusMode) {
            case BEAM -> {
                // Check if clicked on a tile
                for (int row = 0; row < board.getHeight(); row++) {
                    for (int col = 0; col < board.getWidth(); col++) {
                        Rectangle tileRect = new Rectangle(xOffset + col * size, yOffset + row * size, size, size);
                        if (tileRect.contains(p)) {
                            soundEffects.playMove();
                            client.sendUseBeam(row, col);
                            toastManager.showInfo("BEAM", "Teleportiert!",
                                    "Du wurdest zu Position " + row + "/" + col + " teleportiert.");
                            activeBonusMode = null;
                            inputLocked = true;
                            return true;
                        }
                    }
                }
            }
            case PUSH_FIXED -> {
                // Check if clicked on an arrow button - only fixed arrows are valid for this bonus
                for (ArrowButton arrow : arrowButtons) {
                    if (arrow.contains(p)) {
                        if (!arrow.isFixed) {
                            toastManager.showError("PUSH_ERROR", "Nicht erlaubt",
                                    "Waehle einen goldenen Pfeil fuer eine fixierte Reihe/Spalte!");
                            return true;
                        }

                        soundEffects.playPush();
                        client.sendUsePushFixed(arrow.index, arrow.direction);
                        String rowCol = arrow.isRow ? "Zeile " + arrow.index : "Spalte " + arrow.index;
                        toastManager.showInfo("PUSH FIXED", "Fixierte Reihe geschoben",
                                rowCol + " wurde mit dem Bonus geschoben.");
                        activeBonusMode = null;
                        inputLocked = true;
                        return true;
                    }
                }
            }
            default -> {
                // Other bonus modes don't need click handling here
            }
        }

        // Right-click or ESC to cancel bonus mode
        return false;
    }

    /**
     * Show dialog to select a player for SWAP bonus.
     */
    private void showSwapPlayerDialog() {
        List<Player> allPlayers = (players != null && !players.isEmpty()) ? players :
                (board != null && board.getPlayers() != null) ? board.getPlayers() : List.of();

        // Filter out current player
        List<Player> otherPlayers = new ArrayList<>();
        for (Player p : allPlayers) {
            if (!p.getId().equals(currentPlayer.getId())) {
                otherPlayers.add(p);
            }
        }

        if (otherPlayers.isEmpty()) {
            toastManager.showError("SWAP_ERROR", "Kein Ziel", "Keine anderen Spieler zum Tauschen vorhanden!");
            return;
        }

        // Create custom styled dialog
        JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(this), "Position tauschen", Dialog.ModalityType.APPLICATION_MODAL);
        dialog.setUndecorated(true);
        dialog.setBackground(new Color(0, 0, 0, 0));

        JPanel mainPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Semi-transparent dark background with rounded corners
                g2.setColor(new Color(30, 30, 45, 240));
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 20, 20);

                // Border
                g2.setColor(new Color(100, 140, 200));
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, getWidth() - 3, getHeight() - 3, 20, 20);

                g2.dispose();
            }
        };
        mainPanel.setLayout(new BorderLayout(10, 10));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(20, 25, 20, 25));
        mainPanel.setOpaque(false);

        // Title
        JLabel titleLabel = new JLabel("Mit wem tauschen?", SwingConstants.CENTER);
        titleLabel.setFont(new Font("Arial", Font.BOLD, 18));
        titleLabel.setForeground(new Color(220, 220, 240));
        mainPanel.add(titleLabel, BorderLayout.NORTH);

        // Player buttons panel
        JPanel playersPanel = new JPanel(new GridLayout(0, 1, 0, 10));
        playersPanel.setOpaque(false);

        for (Player p : otherPlayers) {
            JButton playerBtn = createStyledPlayerButton(p, dialog);
            playersPanel.add(playerBtn);
        }

        JScrollPane scrollPane = new JScrollPane(playersPanel);
        scrollPane.setOpaque(false);
        scrollPane.getViewport().setOpaque(false);
        scrollPane.setBorder(null);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        mainPanel.add(scrollPane, BorderLayout.CENTER);

        // Cancel button
        JButton cancelBtn = new JButton("Abbrechen");
        cancelBtn.setFont(new Font("Arial", Font.PLAIN, 14));
        cancelBtn.setForeground(Color.WHITE);
        cancelBtn.setBackground(new Color(80, 80, 100));
        cancelBtn.setBorder(BorderFactory.createEmptyBorder(10, 20, 10, 20));
        cancelBtn.setFocusPainted(false);
        cancelBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        cancelBtn.addActionListener(e -> dialog.dispose());
        cancelBtn.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                cancelBtn.setBackground(new Color(100, 100, 120));
            }
            @Override
            public void mouseExited(MouseEvent e) {
                cancelBtn.setBackground(new Color(80, 80, 100));
            }
        });

        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.setOpaque(false);
        buttonPanel.add(cancelBtn);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        dialog.setContentPane(mainPanel);
        dialog.pack();
        dialog.setMinimumSize(new Dimension(280, 150));
        dialog.setLocationRelativeTo(this);
        dialog.setVisible(true);
    }

    /**
     * Create a styled button for player selection in SWAP dialog.
     */
    private JButton createStyledPlayerButton(Player player, JDialog dialog) {
        Color playerColor = getAwtColor(player.getColor());
        Color hoverColor = playerColor.brighter();

        JButton btn = new JButton() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Background
                if (getModel().isRollover()) {
                    g2.setColor(hoverColor);
                } else {
                    g2.setColor(playerColor);
                }
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 12, 12);

                // Player icon circle
                g2.setColor(Color.WHITE);
                g2.fillOval(15, (getHeight() - 30) / 2, 30, 30);
                g2.setColor(playerColor.darker());
                g2.setStroke(new BasicStroke(2));
                g2.drawOval(15, (getHeight() - 30) / 2, 30, 30);

                // Player initial
                g2.setColor(playerColor.darker());
                g2.setFont(FONT_ARIAL_BOLD_16);
                String initial = player.getName().substring(0, 1).toUpperCase();
                FontMetrics fm = g2.getFontMetrics();
                int textX = 15 + (30 - fm.stringWidth(initial)) / 2;
                int textY = (getHeight() - 30) / 2 + (30 + fm.getAscent() - fm.getDescent()) / 2;
                g2.drawString(initial, textX, textY);

                // Player name
                g2.setColor(Color.WHITE);
                g2.setFont(FONT_ARIAL_BOLD_15);
                g2.drawString(player.getName(), 55, getHeight() / 2 + 5);

                // Position info
                if (player.getCurrentPosition() != null) {
                    g2.setFont(FONT_ARIAL_PLAIN_11);
                    g2.setColor(new Color(255, 255, 255, 180));
                    String posText = "Position: " + player.getCurrentPosition().getRow() + "/" + player.getCurrentPosition().getColumn();
                    g2.drawString(posText, getWidth() - 100, getHeight() / 2 + 5);
                }

                g2.dispose();
            }
        };

        btn.setPreferredSize(new Dimension(250, 50));
        btn.setBorderPainted(false);
        btn.setContentAreaFilled(false);
        btn.setFocusPainted(false);
        btn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

        btn.addActionListener(e -> {
            dialog.dispose();
            soundEffects.playMove();
            client.sendUseSwap(player.getId());
            toastManager.showInfo("SWAP", "Position getauscht",
                    "Du hast die Position mit " + player.getName() + " getauscht!");
            inputLocked = true;
        });

        return btn;
    }

    /**
     * Cancel active bonus mode (e.g., when pressing ESC)
     */
    public void cancelBonusMode() {
        if (activeBonusMode != null) {
            toastManager.showInfo("ABGEBROCHEN", "Bonus abgebrochen",
                    activeBonusMode.getValue() + " Bonus wurde abgebrochen.");
            activeBonusMode = null;
            repaint();
        }
    }

    /**
     * Check if there are any valid fixed rows/columns that can be pushed with PUSH_FIXED bonus.
     * Valid targets are rows/columns with even index (fixed tiles) that are NOT on the edge.
     */
    private boolean hasValidPushFixedTargets() {
        if (board == null) return false;

        int rows = board.getHeight();
        int cols = board.getWidth();

        // Check for valid fixed rows (even index, not edge)
        for (int row = 2; row < rows - 1; row += 2) {
            return true; // Found at least one valid row
        }

        // Check for valid fixed columns (even index, not edge)
        for (int col = 2; col < cols - 1; col += 2) {
            return true; // Found at least one valid column
        }

        return false;
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
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED);

            calculateLayoutMetrics();
            drawYourTurnBoardHighlight(g2); // Draw glowing border when it's your turn
            drawBoardGrid(g2);
            createAndDrawArrowButtons(g2);
            drawExtraTile(g2);
            drawSidebar(g2);
            drawCurrentTargetOverlay(g2);
            drawYourTurnBanner(g2); // Draw "Your Turn" banner
        } finally {
            g2.dispose();
        }
    }

    /**
     * Draw a "DEIN ZUG" indicator in the sidebar area when it's the local player's turn.
     * Positioned to not obstruct the game board.
     */
    private void drawYourTurnBanner(Graphics2D g2) {
        if (!isLocalPlayerTurn()) return;

        long time = System.currentTimeMillis();
        int pulseAlpha = 180 + (int) (75 * Math.sin(time / 250.0));

        // Scale based on sidebar width (which scales with window size)
        float scale = Math.max(0.8f, cachedSidebarWidth / 320f);

        // Scaled banner dimensions
        int bannerWidth = Math.round(cachedSidebarWidth - 20);
        int bannerHeight = Math.round(26 * scale);
        int bannerX = cachedSidebarX;
        int bannerY = 60; // Below options button

        // Glowing background
        g2.setColor(new Color(0, 180, 80, Math.min(pulseAlpha - 30, 150)));
        g2.fillRoundRect(bannerX - 2, bannerY - 2, bannerWidth + 4, bannerHeight + 4, 10, 10);

        // Main background
        GradientPaint gradient = new GradientPaint(
                bannerX, bannerY, new Color(30, 160, 60, pulseAlpha),
                bannerX, bannerY + bannerHeight, new Color(20, 120, 40, pulseAlpha)
        );
        g2.setPaint(gradient);
        g2.fillRoundRect(bannerX, bannerY, bannerWidth, bannerHeight, 8, 8);

        // Border
        g2.setColor(new Color(100, 255, 150, pulseAlpha));
        g2.setStroke(new BasicStroke(1.5f));
        g2.drawRoundRect(bannerX, bannerY, bannerWidth, bannerHeight, 8, 8);

        // Scaled font
        int fontSize = Math.round(12 * scale);
        g2.setFont(getCachedFont("SansSerif", Font.BOLD, fontSize));
        g2.setColor(Color.WHITE);
        String text = "DEIN ZUG";
        FontMetrics fm = g2.getFontMetrics();
        int textX = bannerX + (bannerWidth - fm.stringWidth(text)) / 2;
        int textY = bannerY + (bannerHeight + fm.getAscent() - fm.getDescent()) / 2;
        g2.drawString(text, textX, textY);
    }

    /**
     * Draw a glowing border around the board when it's the local player's turn.
     */
    private void drawYourTurnBoardHighlight(Graphics2D g2) {
        if (!isLocalPlayerTurn()) return;

        long time = System.currentTimeMillis();
        int pulseAlpha = 80 + (int) (40 * Math.sin(time / 400.0));

        // Calculate board bounds
        int boardX = xOffset - 10;
        int boardY = yOffset - 10;
        int boardWidth = board.getWidth() * size + 20;
        int boardHeight = board.getHeight() * size + 20;

        // Outer glow
        g2.setColor(new Color(50, 200, 100, pulseAlpha / 2));
        g2.setStroke(new BasicStroke(8));
        g2.drawRoundRect(boardX - 4, boardY - 4, boardWidth + 8, boardHeight + 8, 20, 20);

        // Inner glow
        g2.setColor(new Color(100, 255, 150, pulseAlpha));
        g2.setStroke(new BasicStroke(4));
        g2.drawRoundRect(boardX, boardY, boardWidth, boardHeight, 15, 15);
    }

    private int cachedSidebarWidth = 280;
    private int cachedSidebarX = 10;
    private int cachedExtraTileX = 0;
    private int cachedExtraTileY = 0;
    private int cachedWindowWidth = 0;
    private int cachedWindowHeight = 0;
    private boolean layoutDirty = true;

    /**
     * Marks layout as dirty, requiring recalculation on next paint.
     * Called automatically on component resize.
     */
    public void invalidateLayout() {
        layoutDirty = true;
    }

    /**
     * Repaints only the sidebar region for efficient timer updates.
     * This avoids a full component repaint when only timers change.
     */
    private void repaintSidebarRegion() {
        // Repaint the sidebar area only (x=0 to cachedSidebarWidth + margin, full height)
        repaint(0, 0, cachedSidebarX + cachedSidebarWidth + 20, getHeight());
    }

    private void calculateLayoutMetrics() {
        int windowWidth = getWidth();
        int windowHeight = getHeight();

        // Skip recalculation if size hasn't changed
        if (!layoutDirty && windowWidth == cachedWindowWidth && windowHeight == cachedWindowHeight) {
            return;
        }

        cachedWindowWidth = windowWidth;
        cachedWindowHeight = windowHeight;
        layoutDirty = false;

        cachedSidebarWidth = Math.max(250, Math.min(320, windowWidth / 6));
        cachedSidebarX = 10;

        int extraTileMargin = 20;
        int estimatedExtraTileSize = Math.min(windowWidth / 12, windowHeight / 10);

        int sidebarRightEdge = cachedSidebarX + cachedSidebarWidth + 20; // 20px gap after sidebar
        int extraTileLeftEdge = windowWidth - estimatedExtraTileSize - extraTileMargin * 2;

        int availableWidth = extraTileLeftEdge - sidebarRightEdge - arrowSize * 2;
        int availableHeight = windowHeight - PANEL_PADDING * 2 - arrowSize * 2;

        size = Math.min(availableWidth / board.getWidth(), availableHeight / board.getHeight());
        size = Math.max(50, size); // Minimum playable size

        arrowSize = Math.max(20, Math.min(35, size / 3));

        int boardPixelWidth = size * board.getWidth();
        int boardPixelHeight = size * board.getHeight();

        xOffset = (windowWidth - boardPixelWidth) / 2;
        yOffset = (windowHeight - boardPixelHeight) / 2;

        int minBoardX = sidebarRightEdge + arrowSize + ARROW_MARGIN;
        if (xOffset < minBoardX) {
            xOffset = minBoardX;
        }

        cachedExtraTileX = windowWidth - size - extraTileMargin;
        cachedExtraTileY = windowHeight - size - extraTileMargin - 25;
    }

    private void drawBoardGrid(Graphics2D g2) {
        drawPushHighlight(g2);

        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Tile tile = board.getTiles()[row][col];
                if (tile == null) continue;

                int x = xOffset + col * size;
                int y = yOffset + row * size;

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

            // Bonus zeichnen (wenn vorhanden und kein Treasure)
            if (tile.getBonus() != null && tile.getTreasure() == null) {
                int cx = x + size / 2;
                int cy = y + size / 2;
                drawBonusOnTile(g2, tile.getBonus(), cx, cy);
            }

            // Koordinaten nur bei gültigen row/col
            if (row >= 0 && col >= 0) {
                drawCoordinates(g2, x, y, row, col);
            }
        }
    }

    private void drawBonusOnTile(Graphics2D g2, BonusType bonus, int centerX, int centerY) {
        if (bonus == null) return;

        int imgSize = (int) (size * 0.5);  // 50% der Tile-Groesse
        int imgX = centerX - imgSize / 2;
        int imgY = centerY - imgSize / 2;

        if (bonusBagImage != null) {
            // Zeichne das BonusBag-Bild
            g2.drawImage(bonusBagImage, imgX, imgY, imgSize, imgSize, null);
        } else {
            // Fallback: Farbiger Kreis mit Text
            g2.setColor(new Color(255, 215, 0, 200));  // Gold
            g2.fillOval(imgX, imgY, imgSize, imgSize);
            g2.setColor(Color.BLACK);
            g2.setFont(getCachedFont("Arial", Font.BOLD, imgSize / 3));
            String label = switch (bonus) {
                case BEAM -> "T";
                case SWAP -> "S";
                case PUSH_FIXED -> "F";
                case PUSH_TWICE -> "2";
            };
            FontMetrics fm = g2.getFontMetrics();
            int textX = centerX - fm.stringWidth(label) / 2;
            int textY = centerY + fm.getAscent() / 3;
            g2.drawString(label, textX, textY);
        }
    }

    private void drawTreasureOnTile(Graphics2D g2, Treasure treasure, int centerX, int centerY) {
        if (treasure == null) return;

        // Check if this is the current player's target treasure
        boolean isCurrentTarget = false;
        if (currentPlayer != null) {
            Treasure currentTarget = currentPlayer.getCurrentTargetTreasure();
            isCurrentTarget = currentTarget != null
                    && currentTarget.getId() == treasure.getId();
        }


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

        BufferedImage treasureImg = treasureImages.get(treasure.getId());
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
            g2.setFont(getCachedFont("Arial", Font.BOLD, fallbackSize / 2));
            g2.setColor(Color.WHITE);
            String letter = TreasureUtils.getLocalName(treasure.getId()).substring(0, 1);
            FontMetrics fm = g2.getFontMetrics();
            int letterWidth = fm.stringWidth(letter);
            g2.drawString(letter, centerX - letterWidth / 2, centerY + fm.getAscent() / 2 - 12);

            System.err.println("Using fallback for treasure: " + TreasureUtils.getLocalName(treasure.getId()));
        }

        // Draw treasure name UNDER the image with background
        g2.setFont(isCurrentTarget ? FONT_ARIAL_BOLD_11 : FONT_ARIAL_BOLD_9);
        FontMetrics fm = g2.getFontMetrics();
        String displayName = TreasureUtils.getLocalName(treasure.getId());

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

            // Check if this is the local player
            boolean isLocalPlayer = currentPlayer != null &&
                    i < players.size() &&
                    players.get(i) != null &&
                    players.get(i).getId() != null &&
                    players.get(i).getId().equals(currentPlayer.getId());

            BufferedImage icon = (i < playerIcons.size()) ? playerIcons.get(i) : null;
            int iconSize = count > 1 ? (int) (size * 0.25) : (int) (size * 0.4);

            // Draw glowing highlight around local player (similar to target treasure)
            if (isLocalPlayer) {
                long time = System.currentTimeMillis();
                // Make glow radius larger than icon size to fully surround the avatar
                int glowRadius = iconSize / 2 + 12 + (int) (4 * Math.sin(time / 300.0));
                int glowAlpha = 120 + (int) (60 * Math.sin(time / 300.0));

                // Outer glow - cyan/blue for player (larger)
                g2.setColor(new Color(0, 200, 255, Math.min(glowAlpha / 2, 80)));
                g2.fillOval(px - glowRadius - 4, py - glowRadius - 4, (glowRadius + 4) * 2, (glowRadius + 4) * 2);

                // Inner glow (surrounds avatar fully)
                g2.setColor(new Color(100, 220, 255, Math.min(glowAlpha, 120)));
                g2.fillOval(px - glowRadius, py - glowRadius, glowRadius * 2, glowRadius * 2);
            }

            if (icon != null) {
                // Calculate dimensions preserving aspect ratio
                int imgW = icon.getWidth();
                int imgH = icon.getHeight();
                int drawW, drawH;
                if (imgW >= imgH) {
                    // Landscape: fit to width
                    drawW = iconSize;
                    drawH = (int) (iconSize * ((double) imgH / imgW));
                } else {
                    // Portrait: fit to height
                    drawH = iconSize;
                    drawW = (int) (iconSize * ((double) imgW / imgH));
                }

                g2.drawImage(icon,
                        px - drawW / 2,
                        py - drawH / 2,
                        drawW,
                        drawH,
                        null);

                // Draw border around local player icon
                if (isLocalPlayer) {
                    g2.setColor(new Color(0, 200, 255, 200));
                    g2.setStroke(new BasicStroke(3));
                    int borderSize = Math.max(drawW, drawH);
                    g2.drawOval(px - borderSize / 2 - 2, py - borderSize / 2 - 2, borderSize + 4, borderSize + 4);
                }
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
        boolean pushFixedActive = activeBonusMode == BonusType.PUSH_FIXED;

        for (int row = 0; row < rows; row++) {
            // Normal: nur ungerade Reihen (1, 3, 5) sind schiebbar
            // Push Fixed: alle Reihen außer den äußersten (0 und rows-1)
            boolean isNormalPushable = row % 2 == 1;
            boolean isPushFixedPushable = pushFixedActive && row > 0 && row < rows - 1;

            if (!isNormalPushable && !isPushFixedPushable) continue;

            int y = yOffset + row * size + (size - arrowSize) / 2;
            boolean isFixedRow = row % 2 == 0; // gerade Indices sind fixierte Reihen

            Rectangle leftBounds = new Rectangle(xOffset - arrowSize - ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton leftArrow = new ArrowButton(leftBounds, Direction.RIGHT, row, true, isFixedRow);
            arrowButtons.add(leftArrow);
            drawArrowButton(g2, leftArrow);

            Rectangle rightBounds = new Rectangle(xOffset + cols * size + ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton rightArrow = new ArrowButton(rightBounds, Direction.LEFT, row, true, isFixedRow);
            arrowButtons.add(rightArrow);
            drawArrowButton(g2, rightArrow);
        }

        for (int col = 0; col < cols; col++) {
            // Normal: nur ungerade Spalten (1, 3, 5) sind schiebbar
            // Push Fixed: alle Spalten außer den äußersten (0 und cols-1)
            boolean isNormalPushable = col % 2 == 1;
            boolean isPushFixedPushable = pushFixedActive && col > 0 && col < cols - 1;

            if (!isNormalPushable && !isPushFixedPushable) continue;

            int x = xOffset + col * size + (size - arrowSize) / 2;
            boolean isFixedCol = col % 2 == 0; // gerade Indices sind fixierte Spalten

            Rectangle upBounds = new Rectangle(x, yOffset - arrowSize - ARROW_MARGIN, arrowSize, arrowSize);
            ArrowButton upArrow = new ArrowButton(upBounds, Direction.DOWN, col, false, isFixedCol);
            arrowButtons.add(upArrow);
            drawArrowButton(g2, upArrow);

            Rectangle downBounds = new Rectangle(x, yOffset + rows * size + ARROW_MARGIN, arrowSize, arrowSize);
            ArrowButton downArrow = new ArrowButton(downBounds, Direction.UP, col, false, isFixedCol);
            arrowButtons.add(downArrow);
            drawArrowButton(g2, downArrow);
        }
    }

    private static final Color ARROW_COLOR_FIXED = new Color(200, 140, 50);       // Gold für fixierte Reihen
    private static final Color ARROW_COLOR_FIXED_HOVER = new Color(230, 170, 80); // Helles Gold bei Hover

    private void drawArrowButton(Graphics2D g2, ArrowButton arrow) {
        Color baseColor;
        Color hoverColor;

        if (arrow.isFixed) {
            baseColor = ARROW_COLOR_FIXED;
            hoverColor = ARROW_COLOR_FIXED_HOVER;
        } else {
            baseColor = ARROW_COLOR;
            hoverColor = ARROW_COLOR_HOVER;
        }

        Color currentColor = (arrow == hoveredArrow) ? hoverColor : baseColor;

        // Shadow
        g2.setColor(currentColor);
        g2.fillRoundRect(arrow.bounds.x + 2, arrow.bounds.y + 2, arrow.bounds.width, arrow.bounds.height, 8, 8);
        g2.setColor(new Color(20, 20, 40, 180));
        g2.drawRoundRect(arrow.bounds.x + 2, arrow.bounds.y + 2, arrow.bounds.width, arrow.bounds.height, 8, 8);

        // Main button
        g2.setColor(currentColor);
        g2.fillRoundRect(arrow.bounds.x, arrow.bounds.y, arrow.bounds.width, arrow.bounds.height, 8, 8);

        // Arrow icon
        g2.setColor(Color.WHITE);
        g2.fill(arrow.arrowShape);
    }

    private void drawExtraTile(Graphics2D g2) {
        // Use cached position from calculateLayoutMetrics
        int x = cachedExtraTileX;
        int y = cachedExtraTileY;

        // Scale font size based on tile size
        int labelFontSize = Math.max(10, Math.min(14, size / 7));
        int hintFontSize = Math.max(8, Math.min(10, size / 9));

        // Label above the tile
        g2.setFont(getCachedFont("Arial", Font.BOLD, labelFontSize));
        g2.setColor(ThemeManager.getInstance().getTextLight());
        g2.drawString("Schiebekarte", x, y - 8);

        Tile extraTile = board.getExtraTile();
        if (extraTile == null) {
            // Debug: Fallback wenn null
            g2.setColor(new Color(255, 0, 0, 140));
            g2.drawRect(x, y, size, size);
            g2.setFont(FONT_ARIAL_PLAIN_12);
            g2.drawString("NULL", x + 5, y + 15);
            return;
        }

        // Draw tile with all details (including treasure)
        drawTileAt(g2, extraTile, x, y, -1, -1, true);

        // Optional hint text that it can be rotated
        if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_PUSH) {
            g2.setFont(getCachedFont("Arial", Font.ITALIC, hintFontSize));
            g2.setColor(new Color(255, 255, 255, 200));
            g2.drawString("R/Q/E: Drehen", x, y + size + 15);
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
        // Use cached values from calculateLayoutMetrics
        int sidebarWidth = cachedSidebarWidth;
        int sidebarX = cachedSidebarX;
        int sidebarY = 60;
        int padding = 12;

        // Calculate scale factor for fonts based on sidebar width (1.0 at 320px, min 0.8)
        float sidebarScale = Math.max(0.8f, sidebarWidth / 320f);

        // Scaled font sizes
        int headerFontSize = Math.round(22 * sidebarScale);
        int timerFontSize = Math.round(16 * sidebarScale);
        int playerNameFontSize = Math.round(18 * sidebarScale);
        int stateFontSize = Math.round(11 * sidebarScale);
        int turnTimerFontSize = Math.round(12 * sidebarScale);
        int hintFontSize = Math.round(10 * sidebarScale);

        // Scaled spacing
        int scaledPadding = Math.round(padding * sidebarScale);

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
        g2.setFont(getCachedFont("Arial", Font.BOLD, headerFontSize));
        g2.setColor(new Color(255, 215, 0)); // Gold color
        currentY += Math.round(35 * sidebarScale);

        // Game timer section
        if (gameEndTime != null) {
            drawSectionHeader(g2, "SPIEL-TIMER", sidebarX + padding, currentY, sidebarScale);
            currentY += Math.round(22 * sidebarScale);

            g2.setFont(getCachedFont("Arial", Font.BOLD, timerFontSize));
            g2.setColor(new Color(255, 200, 100));
            String timeRemaining = formatTimeRemaining(gameEndTime);
            g2.drawString(timeRemaining, sidebarX + padding + scaledPadding, currentY);
            currentY += Math.round(25 * sidebarScale);
        }

        // Divider
        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(15 * sidebarScale);

        // Current turn info
        List<Player> allPlayers = (players != null && !players.isEmpty()) ? players :
                (board != null && board.getPlayers() != null) ? board.getPlayers() : List.of();

        if (!allPlayers.isEmpty() && board != null) {
            Player currentTurnPlayer = allPlayers.get(board.getCurrentPlayerIndex());

            drawSectionHeader(g2, "AKTUELLER ZUG", sidebarX + padding, currentY, sidebarScale);
            currentY += Math.round(25 * sidebarScale);

            // Player name with larger font
            g2.setFont(getCachedFont("Arial", Font.BOLD, playerNameFontSize));
            g2.setColor(new Color(255, 255, 150));
            String turnText = currentTurnPlayer.getName();
            if (currentTurnPlayer.isAiControlled()) {
                turnText += " [AI]";
            }
            g2.drawString(turnText, sidebarX + padding + scaledPadding, currentY);
            currentY += Math.round(28 * sidebarScale);

            // Turn state - use server TurnState if available, otherwise client MoveState
            g2.setFont(getCachedFont("Arial", Font.PLAIN, stateFontSize));
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
            g2.drawString(stateText, sidebarX + padding + scaledPadding, currentY);
            currentY += Math.round(18 * sidebarScale);

            // Turn timer
            if (turnEndTime != null) {
                String turnTime = formatTimeRemaining(turnEndTime);
                g2.setFont(getCachedFont("Arial", Font.BOLD, turnTimerFontSize));
                g2.setColor(new Color(255, 150, 150));
                g2.drawString("Zeit: " + turnTime, sidebarX + padding + scaledPadding, currentY);
                currentY += Math.round(20 * sidebarScale);
            } else {
                currentY += Math.round(7 * sidebarScale);
            }

            // Hint for staying in place
            if (currentTurnState != null && currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_MOVE) {
                g2.setFont(getCachedFont("Arial", Font.ITALIC, hintFontSize));
                g2.setColor(new Color(150, 150, 170));
                g2.drawString("(Click your tile to stay in place)", sidebarX + padding + scaledPadding, currentY);
                currentY += Math.round(15 * sidebarScale);
            }
        }

        // Divider
        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(15 * sidebarScale);

        // AI Move button - for manual AI assist
        currentY = drawAiMoveButton(g2, sidebarX, sidebarWidth, padding, currentY, sidebarScale);
        currentY += Math.round(15 * sidebarScale);

        // Bonus section - only show if current player has bonuses
        if (currentPlayer != null && !currentPlayer.getAvailableBonuses().isEmpty()) {
            currentY = drawBonusSection(g2, sidebarX, sidebarWidth, padding, currentY, sidebarScale);
        }

        // Divider before players section
        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(20 * sidebarScale);

        // Players section
        drawSectionHeader(g2, "SPIELER", sidebarX + padding, currentY, sidebarScale);
        currentY += Math.round(25 * sidebarScale);

        // Draw each player
        for (int i = 0; i < allPlayers.size(); i++) {
            Player p = allPlayers.get(i);
            boolean isCurrentTurn = (board != null && i == board.getCurrentPlayerIndex());

            currentY = drawPlayerCard(g2, p, sidebarX + padding, currentY, sidebarWidth - 2 * padding, isCurrentTurn, i, sidebarScale);
            currentY += Math.round(12 * sidebarScale);
        }

        // Current player's treasure cards at the bottom
        if (currentPlayer != null && currentPlayer.getCurrentTargetTreasure() != null) {

            currentY += Math.round(10 * sidebarScale);
            drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
            currentY += Math.round(15 * sidebarScale);

            drawSectionHeader(g2, "DEINE ZIELE", sidebarX + padding, currentY, sidebarScale);
            currentY += Math.round(22 * sidebarScale);

            // Draw CURRENT TARGET treasure prominently
            Treasure currentTarget = currentPlayer.getCurrentTargetTreasure();

            // Current target box with pulsing effect
            long time = System.currentTimeMillis();
            int pulseAlpha = 200 + (int) (55 * Math.sin(time / 400.0));

            // Background box for current target - scaled height
            int boxHeight = Math.round(70 * sidebarScale);
            g2.setColor(new Color(255, 215, 0, pulseAlpha));
            g2.fillRoundRect(sidebarX + padding + 5, currentY - Math.round(15 * sidebarScale), sidebarWidth - 2 * padding - 10, boxHeight, 10, 10);

            // Border
            g2.setColor(new Color(255, 255, 255, 200));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(sidebarX + padding + 5, currentY - Math.round(15 * sidebarScale), sidebarWidth - 2 * padding - 10, boxHeight, 10, 10);

            // "AKTUELLES ZIEL" label
            g2.setFont(getCachedFont("Arial", Font.BOLD, Math.round(10 * sidebarScale)));
            g2.setColor(new Color(100, 70, 0));
            g2.drawString("AKTUELLES ZIEL:", sidebarX + padding + Math.round(15 * sidebarScale), currentY - 2);

            // Draw treasure image - scaled
            BufferedImage treasureImg = treasureImages.get(currentTarget.getId());
            int imgSize = Math.round(40 * sidebarScale);
            int imgX = sidebarX + padding + Math.round(15 * sidebarScale);
            int imgY = currentY + Math.round(5 * sidebarScale);

            if (treasureImg != null) {
                g2.drawImage(treasureImg, imgX, imgY, imgSize, imgSize, null);
            } else {
                // Fallback circle
                g2.setColor(new Color(180, 140, 70));
                g2.fillOval(imgX, imgY, imgSize, imgSize);
            }

            // Current target name next to image
            g2.setFont(getCachedFont("Arial", Font.BOLD, Math.round(16 * sidebarScale)));
            g2.setColor(new Color(0, 0, 0));
            g2.drawString(TreasureUtils.getLocalName(currentTarget.getId()), imgX + imgSize + Math.round(10 * sidebarScale), imgY + imgSize / 2 + 5);

            currentY += boxHeight - Math.round(5 * sidebarScale);
        }

        // Add keyboard hints at the bottom
        int hintsHeight = Math.round(80 * sidebarScale);
        currentY = Math.max(currentY + Math.round(10 * sidebarScale), getHeight() - hintsHeight);
        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(15 * sidebarScale);

        g2.setFont(getCachedFont("Arial", Font.ITALIC, hintFontSize));
        currentY += Math.round(15 * sidebarScale);
        g2.drawString("R/Q/E: Tile drehen", sidebarX + padding, currentY);
    }

    /**
     * Draws the AI toggle button.
     * @return the updated Y position after drawing
     */
    private int drawAiMoveButton(Graphics2D g2, int sidebarX, int sidebarWidth, int padding, int currentY, float scale) {
        int buttonWidth = sidebarWidth - 2 * padding - Math.round(20 * scale);
        int buttonHeight = Math.round(36 * scale);
        int buttonX = sidebarX + padding + Math.round(10 * scale);
        int buttonY = currentY;

        // Store bounds for click detection
        aiMoveButtonBounds = new Rectangle(buttonX, buttonY, buttonWidth, buttonHeight);

        // Determine button state
        boolean isHovered = aiMoveButtonHovered;

        // Button background - green when enabled, gray when disabled
        if (aiThinking) {
            // AI is thinking - pulsing orange effect
            long time = System.currentTimeMillis();
            int alpha = (int) (180 + 40 * Math.sin(time / 200.0));
            g2.setColor(new Color(200, 150, 50, alpha));
        } else if (aiModeEnabled) {
            // Enabled - green
            if (isHovered) {
                g2.setColor(new Color(60, 180, 80, 220));
            } else {
                g2.setColor(new Color(50, 150, 70, 200));
            }
        } else {
            // Disabled - gray/blue
            if (isHovered) {
                g2.setColor(new Color(80, 100, 140, 200));
            } else {
                g2.setColor(new Color(60, 80, 110, 180));
            }
        }
        g2.fillRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

        // Button border
        if (aiThinking) {
            g2.setColor(new Color(255, 200, 100, 255));
            g2.setStroke(new BasicStroke(2));
        } else if (aiModeEnabled) {
            g2.setColor(new Color(100, 220, 120, 255));
            g2.setStroke(new BasicStroke(2));
        } else if (isHovered) {
            g2.setColor(new Color(120, 150, 200, 200));
            g2.setStroke(new BasicStroke(2));
        } else {
            g2.setColor(new Color(90, 110, 140, 150));
            g2.setStroke(new BasicStroke(1));
        }
        g2.drawRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

        // Button text - scaled font
        int fontSize = Math.round(14 * scale);
        g2.setFont(getCachedFont("Arial", Font.BOLD, fontSize));
        String buttonText;
        if (aiThinking) {
            buttonText = "AI Denkt...";
            g2.setColor(new Color(255, 220, 100));
        } else if (aiModeEnabled) {
            buttonText = "AI - AN";
            g2.setColor(new Color(220, 255, 220));
        } else {
            buttonText = "AI - AUS";
            g2.setColor(new Color(200, 200, 210));
        }
        FontMetrics fm = g2.getFontMetrics();
        int textX = buttonX + (buttonWidth - fm.stringWidth(buttonText)) / 2;
        int textY = buttonY + (buttonHeight + fm.getAscent() - fm.getDescent()) / 2;
        g2.drawString(buttonText, textX, textY);

        return currentY + buttonHeight;
    }

    /**
     * Draws the bonus section with clickable bonus buttons.
     * @return the updated Y position after drawing
     */
    private int drawBonusSection(Graphics2D g2, int sidebarX, int sidebarWidth, int padding, int currentY, float scale) {
        drawSectionHeader(g2, "BONI", sidebarX + padding, currentY, scale);
        currentY += Math.round(25 * scale);

        List<BonusType> bonuses = currentPlayer.getAvailableBonuses();
        bonusButtonBounds.clear();

        int buttonWidth = sidebarWidth - 2 * padding - Math.round(20 * scale);
        int buttonHeight = Math.round(40 * scale);
        int buttonSpacing = Math.round(8 * scale);

        for (int i = 0; i < bonuses.size(); i++) {
            BonusType bonus = bonuses.get(i);

            int buttonX = sidebarX + padding + Math.round(10 * scale);
            int buttonY = currentY;

            Rectangle bounds = new Rectangle(buttonX, buttonY, buttonWidth, buttonHeight);
            bonusButtonBounds.add(bounds);

            // Determine button state
            boolean isHovered = (i == hoveredBonusIndex);
            boolean isActive = (bonus == activeBonusMode);

            // Button background
            if (isActive) {
                // Active bonus mode - glowing effect
                g2.setColor(new Color(100, 200, 100, 200));
            } else if (isHovered) {
                // Hovered
                g2.setColor(new Color(80, 120, 180, 200));
            } else {
                // Normal
                g2.setColor(new Color(60, 60, 80, 200));
            }
            g2.fillRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

            // Button border
            if (isActive) {
                g2.setColor(new Color(150, 255, 150, 255));
                g2.setStroke(new BasicStroke(2));
            } else if (isHovered) {
                g2.setColor(new Color(120, 180, 230, 200));
                g2.setStroke(new BasicStroke(2));
            } else {
                g2.setColor(new Color(100, 100, 120, 150));
                g2.setStroke(new BasicStroke(1));
            }
            g2.drawRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

            // Bonus icon and name - scaled font
            String bonusName = getBonusDisplayName(bonus);
            String bonusIcon = getBonusIcon(bonus);

            int fontSize = Math.round(14 * scale);
            g2.setFont(getCachedFont("Arial", Font.BOLD, fontSize));
            g2.setColor(isActive ? new Color(220, 255, 220) : new Color(255, 255, 255));
            g2.drawString(bonusIcon + " " + bonusName, buttonX + Math.round(10 * scale), buttonY + Math.round(26 * scale));

            currentY += buttonHeight + buttonSpacing;
        }

        // Show hint if a bonus mode is active
        if (activeBonusMode != null) {
            int hintFontSize = Math.round(10 * scale);
            g2.setFont(getCachedFont("Arial", Font.ITALIC, hintFontSize));
            g2.setColor(new Color(150, 255, 150));
            String hint = switch (activeBonusMode) {
                case BEAM -> "Klicke auf ein Zielfeld...";
                case PUSH_FIXED -> "Klicke auf einen Pfeil...";
                default -> "";
            };
            if (!hint.isEmpty()) {
                g2.drawString(hint, sidebarX + padding + Math.round(10 * scale), currentY);
                currentY += Math.round(15 * scale);
            }
        }

        return currentY;
    }

    /**
     * Get display name for a bonus type.
     */
    private String getBonusDisplayName(BonusType bonus) {
        return switch (bonus) {
            case BEAM -> "Teleportieren";
            case SWAP -> "Tauschen";
            case PUSH_FIXED -> "Fixiert schieben";
            case PUSH_TWICE -> "Doppelt schieben";
        };
    }

    /**
     * Get icon/emoji for a bonus type.
     */
    private String getBonusIcon(BonusType bonus) {
        return switch (bonus) {
            case BEAM -> "[T]";     // Teleport
            case SWAP -> "[S]";     // Swap
            case PUSH_FIXED -> "[F]"; // Fixed push
            case PUSH_TWICE -> "[2]"; // Double push
        };
    }

    /**
     * Draws a golden highlight on the target treasure tile
     */
    private void drawCurrentTargetOverlay(Graphics2D g2) {
        if (currentPlayer == null || currentPlayer.getCurrentTargetTreasure() == null) {
            return;
        }

        Treasure currentTarget = currentPlayer.getCurrentTargetTreasure();

        // Find the position of the target treasure on the board
        int targetRow = -1;
        int targetCol = -1;

        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Tile tile = board.getTiles()[row][col];
                if (tile != null && tile.getTreasure() != null
                        && tile.getTreasure().getId() == currentTarget.getId()) {
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
        drawSectionHeader(g2, text, x, y, 1.0f);
    }

    /**
     * Draws a section header with consistent styling and scale factor
     */
    private void drawSectionHeader(Graphics2D g2, String text, int x, int y, float scale) {
        int fontSize = Math.round(13 * scale);
        g2.setFont(getCachedFont("Arial", Font.BOLD, fontSize));
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
        return drawPlayerCard(g2, player, x, y, width, isCurrentTurn, playerIndex, 1.0f);
    }

    private int drawPlayerCard(Graphics2D g2, Player player, int x, int y, int width, boolean isCurrentTurn, int playerIndex, float scale) {
        int cardHeight = Math.round(95 * scale);
        int padding = Math.round(10 * scale);

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

        int currentY = y + padding + Math.round(15 * scale);

        // Player icon (left side) - scaled
        int iconSize = Math.round(32 * scale);
        if (playerIndex < playerIcons.size() && playerIcons.get(playerIndex) != null) {
            BufferedImage icon = playerIcons.get(playerIndex);
            // Calculate dimensions preserving aspect ratio
            int imgW = icon.getWidth();
            int imgH = icon.getHeight();
            int drawW, drawH;
            if (imgW >= imgH) {
                drawW = iconSize;
                drawH = (int) (iconSize * ((double) imgH / imgW));
            } else {
                drawH = iconSize;
                drawW = (int) (iconSize * ((double) imgW / imgH));
            }
            // Center the icon in the allocated space
            int offsetX = (iconSize - drawW) / 2;
            int offsetY = (iconSize - drawH) / 2;
            g2.drawImage(icon, x + 2 + offsetX, y + padding + offsetY, drawW, drawH, null);
        } else {
            // Fallback: Color indicator circle - scaled
            int circleSize = Math.round(24 * scale);
            if (player.getColor() != null) {
                Color playerColor = getAwtColor(player.getColor());
                g2.setColor(playerColor);
                g2.fillOval(x + padding, y + padding + 3, circleSize, circleSize);
                g2.setColor(Color.WHITE);
                g2.setStroke(new BasicStroke(1.5f));
                g2.drawOval(x + padding, y + padding + 3, circleSize, circleSize);
            }
        }

        // Player name - scaled font
        int nameFontSize = Math.round(14 * scale);
        g2.setFont(getCachedFont("Arial", Font.BOLD, nameFontSize));
        g2.setColor(Color.WHITE);
        String name = player.getName();
        if (name.length() > 15) {
            name = name.substring(0, 12) + "...";
        }
        g2.drawString(name, x + padding + Math.round(42 * scale), currentY);

        // Badges (admin, AI, disconnected) - scaled
        int badgeFontSize = Math.round(10 * scale);
        int badgeX = x + width - padding - Math.round(20 * scale);
        g2.setFont(getCachedFont("Arial", Font.PLAIN, badgeFontSize));

        // Show AI badge or OFFLINE badge (but not both - AI bots don't need connections)
        if (player.isAiControlled()) {
            g2.setColor(new Color(150, 150, 255));
            g2.drawString("AI", badgeX, currentY);
        } else if (!player.isConnected()) {
            g2.setColor(new Color(200, 80, 80));
            g2.drawString("OFFLINE", badgeX - Math.round(40 * scale), currentY);
        }

        if (player.isAdmin()) {
            g2.setColor(new Color(255, 215, 0));
            g2.drawString("*", x + width - padding - 5, currentY);  // Admin marker
        }

        currentY += Math.round(20 * scale);

        // Score: treasures found / total - scaled font
        int treasuresFound = player.getTreasuresFound() != null ? player.getTreasuresFound().size() : 0;
        int totalTreasures = treasuresFound + player.getRemainingTreasureCount();

        int scoreFontSize = Math.round(12 * scale);
        g2.setFont(getCachedFont("Arial", Font.PLAIN, scoreFontSize));
        g2.setColor(new Color(200, 200, 220));
        g2.drawString("Treasures: " + treasuresFound + "/" + totalTreasures, x + padding + Math.round(24 * scale), currentY);
        currentY += Math.round(18 * scale);

        // Progress bar - scaled
        if (totalTreasures > 0) {
            int barWidth = width - 2 * padding - Math.round(24 * scale);
            int barHeight = Math.round(8 * scale);
            int barX = x + padding + Math.round(24 * scale);

            // Background
            g2.setColor(new Color(60, 60, 70));
            g2.fillRoundRect(barX, currentY - Math.round(6 * scale), barWidth, barHeight, 4, 4);

            // Progress
            int progressWidth = (int) ((double) treasuresFound / totalTreasures * barWidth);
            if (progressWidth > 0) {
                g2.setColor(new Color(100, 200, 100));
                g2.fillRoundRect(barX, currentY - Math.round(6 * scale), progressWidth, barHeight, 4, 4);
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
        boolean isFixed; // true wenn es eine fixierte Reihe/Spalte ist (nur mit Push Fixed schiebbar)
        Path2D.Double arrowShape;

        ArrowButton(Rectangle bounds, Direction direction, int index, boolean isRow, boolean isFixed) {
            this.bounds = bounds;
            this.direction = direction;
            this.index = index;
            this.isRow = isRow;
            this.isFixed = isFixed;
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
        if (currentPlayer != null && currentPlayer.getCurrentTargetTreasure() != null) {
            Treasure currentTarget = currentPlayer.getCurrentTargetTreasure();
            var currentTreasureId = currentTarget.getId();

            if (lastTargetTreasureId == -1 || lastTargetTreasureId != currentTreasureId) {
                // Target changed or first time - show toast and banner
                showNewTargetToast(TreasureUtils.getLocalName(currentTarget.getId()));
                lastTargetTreasureId = currentTreasureId;
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

    /**
     * Batch update method to set multiple state values with a single repaint.
     * This significantly improves performance by avoiding multiple sequential repaints.
     *
     * @param board The game board (required)
     * @param players List of all players (required)
     * @param currentPlayer The current player whose turn it is (can be null)
     * @param gameEndTime Game end time (can be null)
     * @param turnEndTime Turn end time (can be null)
     * @param turnState Current turn state (can be null)
     */
    public void updateGameState(Board board, List<Player> players, Player currentPlayer,
                                java.time.OffsetDateTime gameEndTime,
                                java.time.OffsetDateTime turnEndTime,
                                labyrinth.contracts.models.TurnState turnState) {
        // Update board
        this.board = Objects.requireNonNull(board, "board must not be null");
        detectAndNotifyBoardChanges(previousBoard, board);
        this.previousBoard = copyBoardState(board);
        this.inputLocked = false;

        // Update players
        this.players = players != null ? players : List.of();

        // Update current player
        this.currentPlayer = currentPlayer;
        if (currentPlayer != null && currentPlayer.getCurrentPosition() != null) {
            selectedRow = currentPlayer.getCurrentPosition().getRow();
            selectedCol = currentPlayer.getCurrentPosition().getColumn();
        }

        // Check for target treasure changes
        if (currentPlayer != null && currentPlayer.getCurrentTargetTreasure() != null) {
            Treasure currentTarget = currentPlayer.getCurrentTargetTreasure();
            var currentTreasureId = currentTarget.getId();
            if (lastTargetTreasureId == -1 || lastTargetTreasureId != currentTreasureId) {
                showNewTargetToast(TreasureUtils.getLocalName(currentTarget.getId()));
                lastTargetTreasureId = currentTreasureId;
            }
        }

        // Update timers and turn state
        this.gameEndTime = gameEndTime;
        this.turnEndTime = turnEndTime;
        this.currentTurnState = turnState;

        // Single repaint for all updates
        repaint();
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
        boolean darkTheme = ThemeManager.getInstance().isDarkMode();

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

        themeToggle.addActionListener(e -> {
            themeStatusLabel.setText(themeToggle.isSelected() ? "Dunkel" : "Hell");
            ThemeManager.getInstance().setDarkMode(themeToggle.isSelected());
        });

        themeSection.add(themeContent, BorderLayout.CENTER);
        contentPanel.add(themeSection);

        mainPanel.add(contentPanel, BorderLayout.CENTER);

        // === BUTTONS ===
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 15, 0));
        buttonPanel.setOpaque(false);

        // Exit Button
        JButton exitButton = createStyledDialogButton("🚪 Spiel beenden", new Color(120, 50, 50), new Color(180, 80, 80));
        exitButton.addActionListener(e -> {
            boolean confirmed = StyledDialog.showConfirm(dialog,
                    "Spiel beenden?",
                    "Möchtest du das Spiel wirklich beenden?\nDein Fortschritt geht verloren.");
            if (confirmed) {
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
        dialog.setSize(580, 400);
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
        btn.setPreferredSize(new Dimension(170, 38));
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