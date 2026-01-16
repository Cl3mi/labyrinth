package labyrinth.server.game;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.Direction;
import labyrinth.server.game.models.*;
import labyrinth.server.game.models.records.Position;

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
 * Panel that draws a Labyrinth {@link Board} with optional reachable tile
 * highlighting
 * and clickable arrows to shift rows and columns.
 */
public class BoardPanel extends JPanel {

    // --- Drawing & Layout Constants ---
    private static final int CARD_PANEL_WIDTH = 40;
    private static final int PANEL_PADDING = 20;
    private static final int ARROW_MARGIN = 5;
    private static final Font DEBUG_INFO_FONT = new Font("Arial", Font.BOLD, 16);
    private static final Font PLAYER_MARKER_FONT = new Font("Arial", Font.BOLD, 30);
    private static final Font COORDINATE_FONT = new Font("Arial", Font.PLAIN, 10);
    private static final Color BACKGROUND_COLOR = Color.DARK_GRAY;
    private static final Color CORRIDOR_COLOR = new Color(235, 235, 220);
    private static final Color WALL_COLOR = new Color(50, 50, 50);
    private static final Color REACHABLE_TILE_HIGHLIGHT_COLOR = new Color(80, 160, 80);
    private static final Color NORMAL_TILE_BACKGROUND_COLOR = new Color(100, 100, 100);
    private static final Color FIXED_TILE_BACKGROUND_COLOR = new Color(160, 160, 0);
    private static final Color ARROW_COLOR = new Color(70, 130, 180);
    private static final Color[] PLAYER_COLORS = {
            new Color(200, 80, 80), new Color(80, 180, 80),
            new Color(80, 120, 200), new Color(230, 200, 80)
    };

    // --- Class Fields ---
    private final Game game;
    private Player currentPlayer;
    private final List<Player> players;
    private int currentPlayerIndex = 0;
    private final Set<Tile> reachableTiles;
    private final List<ArrowButton> arrowButtons = new ArrayList<>();
    private final List<BonusButton> bonusButtons = new ArrayList<>();

    // Pending bonus activation state
    private BonusTypes pendingBonusActivation = null;

    // --- Dynamic Layout Fields ---
    private int xOffset;
    private int yOffset;
    private int size;
    private int arrowSize = 30;



    /**
     * Creates a viewer panel for the given board and list of players.
     */
    public BoardPanel(Game game, Player currentPlayer, List<Player> players) {
        this.game = game;
        this.currentPlayer = currentPlayer;
        this.players = players != null ? players : List.of();
        this.reachableTiles = new HashSet<>();
        updateReachableTilesAndRepaint();
        setBackground(BACKGROUND_COLOR);
        setPreferredSize(new Dimension(1400, 800));
        setupMouseListener();
    }

    // =================================================================================
    // MOUSE EVENT HANDLING
    // =================================================================================

    private void setupMouseListener() {
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                // Right-click cancels pending bonus activation
                if (e.getButton() == MouseEvent.BUTTON3 && pendingBonusActivation != null) {
                    System.out.println("Cancelled bonus activation: " + pendingBonusActivation);
                    pendingBonusActivation = null;
                    repaint();
                    return;
                }

                // Prioritize bonus button clicks
                if (handleBonusButtonClick(e.getPoint())) {
                    return;
                }
                // Prioritize arrow clicks. If an arrow was clicked, the event is handled.
                if (handleArrowClick(e.getPoint())) {
                    return;
                }
                // Otherwise, check for a click on a game tile.
                handleTileClick(e.getPoint());
            }
        });
    }

    /**
     * Checks for and handles a click on a bonus activation button.
     *
     * @return true if a bonus button was clicked, false otherwise.
     */
    private boolean handleBonusButtonClick(Point p) {
        for (BonusButton button : bonusButtons) {
            if (button.contains(p)) {
                // Check if a bonus has already been used this turn
                if (game.isBonusUsedThisTurn()) {
                    System.out.println("Only one bonus can be used per turn!");
                    return true;
                }

                // Attempt to activate the bonus for the current player
                if (currentPlayer != null && currentPlayer.getBonuses().contains(button.bonusType)) {
                    // Check if this bonus requires additional input (coordinates or target player)
                    if (button.bonusType == BonusTypes.BEAM || button.bonusType == BonusTypes.SWAP) {
                        // Set pending state and wait for tile/player click
                        pendingBonusActivation = button.bonusType;
                        System.out.println("Click on a " +
                            (button.bonusType == BonusTypes.BEAM ? "tile" : "player") +
                            " to activate " + button.bonusType);
                        repaint();
                    } else {
                        // Activate immediately for bonuses that don't need extra args
                        try {
                            boolean success = game.useBonus(button.bonusType);
                            if (success) {
                                System.out.println("Activated bonus: " + button.bonusType + " for player: " + currentPlayer.getUsername());
                                updateReachableTilesAndRepaint();
                            } else {
                                System.out.println("Failed to activate bonus: " + button.bonusType);
                            }
                        } catch (IllegalStateException e) {
                            System.out.println("Error: " + e.getMessage());
                        }
                    }
                } else {
                    System.out.println("Player does not have bonus: " + button.bonusType);
                }
                return true;
            }
        }
        return false;
    }

    /**
     * Checks for and handles a click on a row/column shift arrow.
     *
     * @return true if an arrow was clicked, false otherwise.
     */
    private boolean handleArrowClick(Point p) {
        for (ArrowButton arrow : arrowButtons) {
            if (arrow.contains(p)) {
                if (arrow.isRow) {
                    game.shift(arrow.index, arrow.direction, currentPlayer);
                } else {
                    game.shift(arrow.index, arrow.direction, currentPlayer);
                }
                updateReachableTilesAndRepaint();
                return true;
            }
        }
        return false;
    }

    /**
     * Checks for and handles a click on a tile on the game board.
     */
    private void handleTileClick(Point p) {
        System.out.println("---------");
        System.out.println("Current player: " + currentPlayer);

        outer: for (int row = 0; row < game.getBoard().getHeight(); row++) {
            for (int col = 0; col < game.getBoard().getWidth(); col++) {
                Rectangle tileRect = new Rectangle(xOffset + col * size, yOffset + row * size, size, size);
                if (tileRect.contains(p)) {
                    // Check if we have a pending bonus activation
                    if (pendingBonusActivation != null) {
                        handlePendingBonusActivation(row, col);
                        break outer;
                    }

                    // Normal tile click for movement
                    Tile clickedTile = game.getBoard().getTileAt(row, col);
                    if (reachableTiles.contains(clickedTile)) {
                        boolean moved = game.movePlayerToTile(row, col, currentPlayer).moveSuccess();
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

    /**
     * Handles the completion of a pending bonus activation after the user has clicked on a tile.
     */
    private void handlePendingBonusActivation(int row, int col) {
        boolean success;

        try {
            if (pendingBonusActivation == BonusTypes.BEAM) {
                // BEAM bonus: teleport to the clicked tile
                success = game.useBonus(BonusTypes.BEAM, row, col);
                if (success) {
                    System.out.println("Activated BEAM bonus to tile (" + row + ", " + col + ")");
                } else {
                    System.out.println("Failed to activate BEAM bonus (tile may be occupied)");
                }
            } else if (pendingBonusActivation == BonusTypes.SWAP) {
                // SWAP bonus: find player at the clicked tile and swap with them
                Tile clickedTile = game.getBoard().getTileAt(row, col);
                Player targetPlayer = null;

                for (Player p : game.getPlayers()) {
                    if (p != currentPlayer && p.getCurrentTile() == clickedTile) {
                        targetPlayer = p;
                        break;
                    }
                }

                if (targetPlayer != null) {
                    success = game.useBonus(BonusTypes.SWAP, targetPlayer);
                    if (success) {
                        System.out.println("Activated SWAP bonus with player: " + targetPlayer.getUsername());
                    } else {
                        System.out.println("Failed to activate SWAP bonus");
                    }
                } else {
                    System.out.println("No player found at tile (" + row + ", " + col + ")");
                }
            }
        } catch (Exception e) {
            System.out.println("Error activating bonus: " + e.getMessage());
        } finally {
            // Clear pending state and update UI
            pendingBonusActivation = null;
            updateReachableTilesAndRepaint();
        }
    }

    /**
     * Recalculates the set of reachable tiles for the current player and repaints
     * the component.
     */
    private void updateReachableTilesAndRepaint() {
        reachableTiles.clear();
        if (currentPlayer != null) {
            reachableTiles.addAll(game.getBoard().getReachableTiles(currentPlayer));
        }

        repaint();
    }

    public void updateReachableTilesAndRepaintAuto() {
        currentPlayer = game.getPlayers().get(game.getCurrentPlayerIndex());
        reachableTiles.clear();
        if (currentPlayer != null) {
            reachableTiles.addAll(game.getBoard().getReachableTiles(currentPlayer));
        }

        repaint();
    }

    // =================================================================================
    // PAINTING LOGIC
    // =================================================================================

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        if (game.getBoard() == null)
            return;

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            calculateLayoutMetrics();
            drawBoardGrid(g2);
            createAndDrawArrowButtons(g2);
            drawExtraTile(g2);
            drawDebugInfo(g2);
            createAndDrawBonusButtons(g2);

            if (currentPlayer != null) {
                drawTreasureCards(g2, currentPlayer);
            }
        } finally {
            g2.dispose();
        }
    }

    /**
     * Calculates the primary layout metrics (tile size, board offset) based on the
     * panel's current size.
     */
    private void calculateLayoutMetrics() {
        int w = getWidth() - 2 * arrowSize - PANEL_PADDING - CARD_PANEL_WIDTH;
        int h = getHeight() - 2 * arrowSize - PANEL_PADDING;
        size = Math.min(w / game.getBoard().getWidth(), h / game.getBoard().getHeight());

        xOffset = CARD_PANEL_WIDTH + (getWidth() - size * game.getBoard().getWidth()) / 2;
        yOffset = (getHeight() - size * game.getBoard().getHeight()) / 2;
    }

    /**
     * Draws the main grid of tiles and the players on them.
     */
    private void drawBoardGrid(Graphics2D g2) {
        var board = game.getBoard();
        for (int row = 0; row < board.getHeight(); row++) {
            for (int col = 0; col < board.getWidth(); col++) {
                Tile tile = board.getTileAt(row, col);
                if (tile != null) {
                    int x = xOffset + col * size;
                    int y = yOffset + row * size;
                    drawTileAt(g2, tile, x, y, row, col, true);
                    drawPlayersOnTile(g2, row, col);
                }
            }
        }
    }

    /**
     * Draws a single, complete tile at a specified location.
     *
     * @param drawDetails Toggles drawing of player-specific details like treasures
     *                    and coordinates.
     */
    private void drawTileAt(Graphics2D g2, Tile tile, int x, int y, int row, int col, boolean drawDetails) {
        int cx = x + size / 2;
        int cy = y + size / 2;
        int corridorWidth = Math.max(4, size / 6);

        // Background
        g2.setColor(reachableTiles.contains(tile) ? REACHABLE_TILE_HIGHLIGHT_COLOR : NORMAL_TILE_BACKGROUND_COLOR);
        g2.fillRect(x, y, size, size);

        // Corridors
        g2.setColor(CORRIDOR_COLOR);
        if (tile.getEntrances().contains(Direction.UP))
            g2.fillRect(cx - corridorWidth / 2, y, corridorWidth, size / 2);
        if (tile.getEntrances().contains(Direction.DOWN))
            g2.fillRect(cx - corridorWidth / 2, cy, corridorWidth, size / 2);
        if (tile.getEntrances().contains(Direction.LEFT))
            g2.fillRect(x, cy - corridorWidth / 2, size / 2, corridorWidth);
        if (tile.getEntrances().contains(Direction.RIGHT))
            g2.fillRect(cx, cy - corridorWidth / 2, size / 2, corridorWidth);

        // Center dot
        int dotSize = Math.max(4, corridorWidth);
        g2.setColor(tile.isFixed() ? FIXED_TILE_BACKGROUND_COLOR : WALL_COLOR);
        g2.fillOval(cx - dotSize / 2, cy - dotSize / 2, dotSize, dotSize);

        if (drawDetails) {
            // Treasure Name
            if (tile.getTreasureCard() != null) {
                drawTreasureOnTile(g2, tile.getTreasureCard(), cx, cy);
            }
            if (tile.getBonus() != null) {
                drawBonusOnTile(g2, tile.getBonus(), cx, cy);
            }
            // Coordinates
            drawCoordinates(g2, x, y, row, col);
        }

        // Border
        g2.setColor(Color.BLACK);
        g2.drawRect(x, y, size, size);
    }

    /**
     * A helper to draw the treasure name on a tile.
     */
    private void drawTreasureOnTile(Graphics2D g2, TreasureCard card, int centerX, int centerY) {
        if (currentPlayer != null && currentPlayer.getCurrentTreasureCard() == card) {
            g2.setColor(Color.GREEN.darker());
        } else if (currentPlayer != null && currentPlayer.getAssignedTreasureCards().contains(card)) {
            g2.setColor(Color.RED);
        } else {
            g2.setColor(Color.BLACK);
        }

        FontMetrics fm = g2.getFontMetrics();
        int textWidth = fm.stringWidth(card.getTreasureName());
        g2.drawString(card.getTreasureName(), centerX - textWidth / 2, (centerY + fm.getAscent() / 2) - 20);
    }

    private void drawBonusOnTile(Graphics2D g2, BonusTypes bonus, int centerX, int centerY) {
        g2.setColor(new Color(0, 100, 100)); // Dark Cyan for bonus text
        FontMetrics fm = g2.getFontMetrics();
        String text = bonus.name(); // Or short name
        int textWidth = fm.stringWidth(text);
        g2.drawString(text, centerX - textWidth / 2, (centerY + fm.getAscent() / 2) + 20); // Shift down
    }

    /**
     * A helper to draw the (row, col) coordinates on a tile.
     */
    private void drawCoordinates(Graphics2D g2, int x, int y, int row, int col) {
        String coords = "(" + row + "," + col + ")";
        g2.setColor(Color.WHITE);
        Font oldFont = g2.getFont();
        g2.setFont(COORDINATE_FONT);
        g2.drawString(coords, x + 3, y + size - 3);
        g2.setFont(oldFont);
    }

    /**
     * Draws any players located on the specified tile.
     */
    private void drawPlayersOnTile(Graphics2D g2, int row, int col) {
        var board = game.getBoard();
        for (int i = 0; i < players.size(); i++) {
            Player p = players.get(i);
            if (p != null) {
                var tile = p.getCurrentTile();
                if (tile != null) {
                    Position pos = board.getPositionOfTile(tile);
                    if (pos != null && pos.row() == row && pos.column() == col) {
                        int cx = xOffset + col * size + size / 2;
                        int cy = yOffset + row * size + size / 2;

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
    }

    /**
     * Clears, recreates, and draws all the arrow buttons.
     */
    private void createAndDrawArrowButtons(Graphics2D g2) {
        var board = game.getBoard();
        arrowButtons.clear();
        g2.setColor(ARROW_COLOR);
        g2.setStroke(new BasicStroke(2));

        int rows = board.getHeight();
        int cols = board.getWidth();

        // Left and Right arrows
        for (int row = 0; row < rows; row++) {
            int y = yOffset + row * size + (size - arrowSize) / 2;
            Rectangle rightBounds = new Rectangle(xOffset - arrowSize - ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton rightArrow = new ArrowButton(rightBounds, Direction.RIGHT, row, true);
            arrowButtons.add(rightArrow);
            g2.fill(rightArrow.arrowShape);
            g2.draw(rightArrow.arrowShape);

            Rectangle leftBounds = new Rectangle(xOffset + cols * size + ARROW_MARGIN, y, arrowSize, arrowSize);
            ArrowButton leftArrow = new ArrowButton(leftBounds, Direction.LEFT, row, true);
            arrowButtons.add(leftArrow);
            g2.fill(leftArrow.arrowShape);
            g2.draw(leftArrow.arrowShape);
        }

        // Top and Bottom arrows
        for (int col = 0; col < cols; col++) {
            int x = xOffset + col * size + (size - arrowSize) / 2;
            Rectangle downBounds = new Rectangle(x, yOffset - arrowSize - ARROW_MARGIN, arrowSize, arrowSize);
            ArrowButton downArrow = new ArrowButton(downBounds, Direction.DOWN, col, false);
            arrowButtons.add(downArrow);
            g2.fill(downArrow.arrowShape);
            g2.draw(downArrow.arrowShape);

            Rectangle upBounds = new Rectangle(x, yOffset + rows * size + ARROW_MARGIN, arrowSize, arrowSize);
            ArrowButton upArrow = new ArrowButton(upBounds, Direction.UP, col, false);
            arrowButtons.add(upArrow);
            g2.fill(upArrow.arrowShape);
            g2.draw(upArrow.arrowShape);
        }
    }

    /**
     * Draws the extra tile in the corner of the panel.
     */
    private void drawExtraTile(Graphics2D g2) {
        var board = game.getBoard();
        Tile extraTile = board.getExtraTile();
        if (extraTile != null) {
            int margin = 20;
            int x = getWidth() - size - margin;
            int y = getHeight() - size - margin;
            // The extra tile doesn't need coordinates or other board-specific details
            drawTileAt(g2, extraTile, x, y, -1, -1, false);
        }
    }

    private void drawDebugInfo(Graphics2D g2) {
        var board = game.getBoard();
        int infoX = getWidth() - 250;
        int infoY = 40;

        g2.setFont(DEBUG_INFO_FONT);
        g2.setColor(Color.RED);

        List<String> infoLines = new ArrayList<>();
        var players = game.getPlayers();

        if (players != null && !players.isEmpty()) {
            var current = players.get(game.getCurrentPlayerIndex());
            infoLines.add("Player to move: " + current.getUsername());
        } else {
            infoLines.add("Player to move: None");
        }

        infoLines.add("Free roam: " + board.isFreeRoam());
        infoLines.add("Current move state: " + game.getCurrentMoveState());
        infoLines.add(""); // blank line
        infoLines.add("=== Players ===");

        if (players != null && !players.isEmpty()) {
            for (Player p : players) {
                Position pos = null;
                if (p.getCurrentTile() != null) {
                    pos = board.getPositionOfTile(p.getCurrentTile());
                }
                String positionText = (pos != null) ? "(" + pos.row() + "," + pos.column() + ")" : "(not placed)";
                infoLines.add(p.getUsername() + " at " + positionText);
                if (p.getStatistics() != null) {
                    var s = p.getStatistics();
                    infoLines.add(String.format(" - Score: %d, Steps: %d", s.getScore(), s.getStepsTaken()));
                    infoLines.add(String.format(" - Pushed: %d, Treasures: %d", s.getTilesPushed(),
                            s.getTreasuresCollected()));
                }
                if (!p.getBonuses().isEmpty()) {
                    infoLines.add(" - Bonuses: " + p.getBonuses());
                }
            }
        }

        int lineHeight = 25;
        for (int i = 0; i < infoLines.size(); i++) {
            g2.drawString(infoLines.get(i), infoX, infoY + i * lineHeight);
        }
    }

    /**
     * Creates and draws clickable bonus activation buttons for each bonus type.
     * Only shows buttons for bonuses that the current player has collected.
     */
    private void createAndDrawBonusButtons(Graphics2D g2) {
        bonusButtons.clear();

        if (currentPlayer == null || currentPlayer.getBonuses().isEmpty()) {
            return;
        }

        int buttonWidth = 120;
        int buttonHeight = 30;
        int buttonX = getWidth() - 260;
        int buttonY = getHeight() - 200;
        int buttonSpacing = 10;

        g2.setFont(new Font("Arial", Font.BOLD, 12));

        // Draw title
        g2.setColor(Color.CYAN);
        String title = "=== Activate Bonus ===";
        if (pendingBonusActivation != null) {
            title = ">>> Click a " +
                (pendingBonusActivation == BonusTypes.BEAM ? "tile" : "player") +
                " <<<";
        }
        g2.drawString(title, buttonX, buttonY - 15);

        int index = 0;
        for (BonusTypes bonusType : currentPlayer.getBonuses()) {
            int y = buttonY + index * (buttonHeight + buttonSpacing);

            Rectangle buttonBounds = new Rectangle(buttonX, y, buttonWidth, buttonHeight);
            BonusButton button = new BonusButton(buttonBounds, bonusType);
            bonusButtons.add(button);

            // Check if this bonus is pending activation
            boolean isPending = (pendingBonusActivation == bonusType);
            // Check if any bonus has been used this turn
            boolean bonusAlreadyUsed = game.isBonusUsedThisTurn();

            // Draw button background (highlight if pending, gray if disabled)
            if (bonusAlreadyUsed && !isPending) {
                g2.setColor(new Color(100, 100, 100)); // Gray for disabled
            } else if (isPending) {
                g2.setColor(new Color(255, 150, 0)); // Orange for pending
            } else {
                g2.setColor(new Color(50, 150, 200)); // Normal blue
            }
            g2.fillRoundRect(buttonX, y, buttonWidth, buttonHeight, 10, 10);

            // Draw button border (thicker if pending)
            if (isPending) {
                g2.setStroke(new BasicStroke(3));
                g2.setColor(new Color(200, 100, 0));
            } else {
                g2.setStroke(new BasicStroke(1));
                if (bonusAlreadyUsed) {
                    g2.setColor(new Color(60, 60, 60)); // Dark gray border for disabled
                } else {
                    g2.setColor(new Color(30, 100, 150));
                }
            }
            g2.drawRoundRect(buttonX, y, buttonWidth, buttonHeight, 10, 10);
            g2.setStroke(new BasicStroke(1)); // Reset stroke

            // Draw button text
            if (bonusAlreadyUsed && !isPending) {
                g2.setColor(new Color(150, 150, 150)); // Light gray text for disabled
            } else {
                g2.setColor(Color.WHITE);
            }
            String buttonText = bonusType.name();
            if (isPending) {
                buttonText += " (ACTIVE)";
            } else if (bonusAlreadyUsed) {
                buttonText += " (USED)";
            }
            FontMetrics fm = g2.getFontMetrics();
            int textWidth = fm.stringWidth(buttonText);
            int textX = buttonX + (buttonWidth - textWidth) / 2;
            int textY = y + (buttonHeight + fm.getAscent()) / 2 - 2;
            g2.drawString(buttonText, textX, textY);

            index++;
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

            g2.fillRoundRect(startX, y, cardWidth, cardHeight, 10, 10);

            // Text Color Logic: Green for current target, Red for others
            if (cards.get(i) == player.getCurrentTreasureCard()) {
                g2.setColor(Color.GREEN.darker());
            } else {
                g2.setColor(Color.RED);
            }

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
        // This print statement is useful for debugging player switches
        System.out.println("Switching player " + (currentPlayerIndex + 1));
        updateReachableTilesAndRepaint();
    }

    public void rotateExtraTile() {
        var board = game.getBoard();
        if (board.getExtraTile() != null) {
            board.getExtraTile().rotate();
            repaint();
        }
    }

    public void toggleFreeRoam() {
        var board = game.getBoard();
        board.setFreeRoam(!board.isFreeRoam());
        repaint();
    }

    // =================================================================================
    // INNER CLASSES
    // =================================================================================

    /**
     * Represents a clickable arrow button for shifting rows/columns.
     */
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
                case LEFT:
                    arrow.moveTo(cx + size / 2.0, cy - size / 2.0);
                    arrow.lineTo(cx - size / 2.0, cy);
                    arrow.lineTo(cx + size / 2.0, cy + size / 2.0);
                    break;
                case RIGHT:
                    arrow.moveTo(cx - size / 2.0, cy - size / 2.0);
                    arrow.lineTo(cx + size / 2.0, cy);
                    arrow.lineTo(cx - size / 2.0, cy + size / 2.0);
                    break;
                case UP:
                    arrow.moveTo(cx - size / 2.0, cy + size / 2.0);
                    arrow.lineTo(cx, cy - size / 2.0);
                    arrow.lineTo(cx + size / 2.0, cy + size / 2.0);
                    break;
                case DOWN:
                    arrow.moveTo(cx - size / 2.0, cy - size / 2.0);
                    arrow.lineTo(cx, cy + size / 2.0);
                    arrow.lineTo(cx + size / 2.0, cy - size / 2.0);
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
     * Represents a clickable button for activating a bonus.
     */
    private static class BonusButton {
        Rectangle bounds;
        BonusTypes bonusType;

        BonusButton(Rectangle bounds, BonusTypes bonusType) {
            this.bounds = bounds;
            this.bonusType = bonusType;
        }

        boolean contains(Point p) {
            return bounds.contains(p);
        }
    }
}