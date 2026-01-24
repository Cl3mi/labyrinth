package labyrinth.client.ui.board;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.client.models.extensions.TreasureUtils;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Treasure;
import labyrinth.contracts.models.TurnState;
import lombok.Setter;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Sidebar component displaying game state information.
 * Separated from BoardPanel for independent repaint cycles.
 */
public class SidebarPanel extends JPanel {

    private static final int PADDING = 12;

    private Board board;
    private Player currentPlayer;
    private List<Player> players = new ArrayList<>();
    private OffsetDateTime gameEndTime;
    private OffsetDateTime turnEndTime;
    private TurnState currentTurnState;

    private boolean aiModeEnabled = false;
    private boolean aiThinking = false;

    private final List<Rectangle> bonusButtonBounds = new ArrayList<>();
    private int hoveredBonusIndex = -1;
    private Rectangle aiMoveButtonBounds;
    private boolean aiMoveButtonHovered = false;

    @Setter
    private Consumer<BonusType> onBonusClicked;
    @Setter
    private Runnable onAiToggleRequested;

    private final Map<Integer, BufferedImage> treasureImages = new HashMap<>();
    private final Timer refreshTimer;

    public SidebarPanel() {
        setOpaque(false);
        setPreferredSize(new Dimension(280, 600));
        loadTreasureImages();
        setupMouseListener();

        refreshTimer = new Timer(1000, e -> repaint());
        refreshTimer.start();
    }

    private void loadTreasureImages() {
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
                System.err.println("Failed to load treasure image: " + entry.getValue());
            }
        }
    }

    private void setupMouseListener() {
        addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent e) {
                handleClick(e.getPoint());
            }
        });

        addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
            @Override
            public void mouseMoved(java.awt.event.MouseEvent e) {
                handleMouseMove(e.getPoint());
            }
        });
    }

    private void handleClick(Point p) {
        if (aiMoveButtonBounds != null && aiMoveButtonBounds.contains(p)) {
            if (onAiToggleRequested != null) {
                onAiToggleRequested.run();
            }
            return;
        }

        for (int i = 0; i < bonusButtonBounds.size(); i++) {
            if (bonusButtonBounds.get(i).contains(p)) {
                if (currentPlayer != null && i < currentPlayer.getAvailableBonuses().size()) {
                    BonusType bonus = currentPlayer.getAvailableBonuses().get(i);
                    if (onBonusClicked != null) {
                        onBonusClicked.accept(bonus);
                    }
                }
                return;
            }
        }
    }

    private void handleMouseMove(Point p) {
        boolean needsRepaint = false;

        boolean wasHovered = aiMoveButtonHovered;
        aiMoveButtonHovered = aiMoveButtonBounds != null && aiMoveButtonBounds.contains(p);
        if (wasHovered != aiMoveButtonHovered) needsRepaint = true;

        int oldHovered = hoveredBonusIndex;
        hoveredBonusIndex = -1;
        for (int i = 0; i < bonusButtonBounds.size(); i++) {
            if (bonusButtonBounds.get(i).contains(p)) {
                hoveredBonusIndex = i;
                break;
            }
        }
        if (oldHovered != hoveredBonusIndex) needsRepaint = true;

        if (needsRepaint) repaint();
    }

    public void updateGameState(Board board, Player currentPlayer, List<Player> players,
                                 OffsetDateTime gameEndTime, OffsetDateTime turnEndTime,
                                 TurnState turnState) {
        this.board = board;
        this.currentPlayer = currentPlayer;
        this.players = players != null ? players : new ArrayList<>();
        this.gameEndTime = gameEndTime;
        this.turnEndTime = turnEndTime;
        this.currentTurnState = turnState;
        repaint();
    }

    public void setAiState(boolean enabled, boolean thinking) {
        this.aiModeEnabled = enabled;
        this.aiThinking = thinking;
        repaint();
    }

    public void cleanup() {
        if (refreshTimer != null) {
            refreshTimer.stop();
        }
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int width = getWidth();
            int height = getHeight();
            float scale = Math.max(0.8f, width / 280f);

            drawBackground(g2, width, height);

            bonusButtonBounds.clear();
            int currentY = PADDING;

            currentY = drawTimerSection(g2, currentY, width, scale);
            currentY = drawCurrentTurnSection(g2, currentY, width, scale);
            currentY = drawAiButton(g2, currentY, width, scale);

            if (currentPlayer != null && !currentPlayer.getAvailableBonuses().isEmpty()) {
                currentY = drawBonusSection(g2, currentY, width, scale);
            }

            currentY = drawPlayersSection(g2, currentY, width, scale);
            currentY = drawTargetSection(g2, currentY, width, scale);
            drawKeyboardHints(g2, width, height, scale);
        } finally {
            g2.dispose();
        }
    }

    private void drawBackground(Graphics2D g2, int width, int height) {
        GradientPaint gradient = new GradientPaint(
                0, 0, new Color(40, 40, 50, 240),
                0, height, new Color(30, 30, 40, 240)
        );
        g2.setPaint(gradient);
        g2.fillRoundRect(0, 0, width, height, 15, 15);

        g2.setColor(new Color(100, 130, 180, 150));
        g2.setStroke(new BasicStroke(3));
        g2.drawRoundRect(0, 0, width - 1, height - 1, 15, 15);
    }

    private int drawTimerSection(Graphics2D g2, int y, int width, float scale) {
        if (gameEndTime == null) return y;

        int currentY = y + Math.round(10 * scale);
        drawSectionHeader(g2, "SPIEL-TIMER", PADDING, currentY, scale);
        currentY += Math.round(22 * scale);

        g2.setFont(FontManager.getFontForSize(16 * scale, Font.BOLD));
        g2.setColor(new Color(255, 200, 100));
        g2.drawString(formatTime(gameEndTime), PADDING + 10, currentY);
        currentY += Math.round(25 * scale);

        drawDivider(g2, PADDING, width - PADDING, currentY);
        return currentY + Math.round(15 * scale);
    }

    private int drawCurrentTurnSection(Graphics2D g2, int y, int width, float scale) {
        if (board == null || players.isEmpty()) return y;

        int currentY = y;
        Player turnPlayer = players.get(board.getCurrentPlayerIndex());

        drawSectionHeader(g2, "AKTUELLER ZUG", PADDING, currentY, scale);
        currentY += Math.round(25 * scale);

        g2.setFont(FontManager.getFontForSize(18 * scale, Font.BOLD));
        g2.setColor(new Color(255, 255, 150));
        String turnText = turnPlayer.getName();
        if (turnPlayer.isAiControlled()) turnText += " [AI]";
        g2.drawString(turnText, PADDING + 10, currentY);
        currentY += Math.round(28 * scale);

        g2.setFont(FontManager.getFontForSize(11 * scale, Font.PLAIN));
        g2.setColor(new Color(180, 180, 200));
        String stateText = currentTurnState != null
                ? (currentTurnState == TurnState.WAITING_FOR_PUSH ? "Warten auf Schieben" : "Warten auf Zug")
                : "WAITING";
        g2.drawString(stateText, PADDING + 10, currentY);
        currentY += Math.round(18 * scale);

        if (turnEndTime != null) {
            g2.setFont(FontManager.getFontForSize(12 * scale, Font.BOLD));
            g2.setColor(new Color(255, 150, 150));
            g2.drawString("Zeit: " + formatTime(turnEndTime), PADDING + 10, currentY);
            currentY += Math.round(20 * scale);
        }

        drawDivider(g2, PADDING, width - PADDING, currentY);
        return currentY + Math.round(15 * scale);
    }

    private int drawAiButton(Graphics2D g2, int y, int width, float scale) {
        int buttonWidth = width - 2 * PADDING - Math.round(20 * scale);
        int buttonHeight = Math.round(36 * scale);
        int buttonX = PADDING + Math.round(10 * scale);

        aiMoveButtonBounds = new Rectangle(buttonX, y, buttonWidth, buttonHeight);

        Color bgColor;
        if (aiThinking) {
            long time = System.currentTimeMillis();
            int alpha = (int) (180 + 40 * Math.sin(time / 200.0));
            bgColor = new Color(200, 150, 50, alpha);
        } else if (aiModeEnabled) {
            bgColor = aiMoveButtonHovered ? new Color(60, 180, 80, 220) : new Color(50, 150, 70, 200);
        } else {
            bgColor = aiMoveButtonHovered ? new Color(80, 100, 140, 200) : new Color(60, 80, 110, 180);
        }

        g2.setColor(bgColor);
        g2.fillRoundRect(buttonX, y, buttonWidth, buttonHeight, 10, 10);

        Color borderColor = aiThinking ? new Color(255, 200, 100) :
                aiModeEnabled ? new Color(100, 220, 120) : new Color(90, 110, 140, 150);
        g2.setColor(borderColor);
        g2.setStroke(new BasicStroke(2));
        g2.drawRoundRect(buttonX, y, buttonWidth, buttonHeight, 10, 10);

        g2.setFont(FontManager.getFontForSize(14 * scale, Font.BOLD));
        String text = aiThinking ? "AI Denkt..." : (aiModeEnabled ? "AI - AN" : "AI - AUS");
        g2.setColor(aiThinking ? new Color(255, 220, 100) :
                aiModeEnabled ? new Color(200, 255, 200) : new Color(200, 200, 220));
        FontMetrics fm = g2.getFontMetrics();
        int textX = buttonX + (buttonWidth - fm.stringWidth(text)) / 2;
        int textY = y + (buttonHeight + fm.getAscent() - fm.getDescent()) / 2;
        g2.drawString(text, textX, textY);

        return y + buttonHeight + Math.round(15 * scale);
    }

    private int drawBonusSection(Graphics2D g2, int y, int width, float scale) {
        drawSectionHeader(g2, "BONI", PADDING, y, scale);
        int currentY = y + Math.round(22 * scale);

        List<BonusType> bonuses = currentPlayer.getAvailableBonuses();
        int buttonSize = Math.round(45 * scale);
        int spacing = Math.round(8 * scale);
        int startX = PADDING + Math.round(10 * scale);

        for (int i = 0; i < bonuses.size(); i++) {
            BonusType bonus = bonuses.get(i);
            int bx = startX + i * (buttonSize + spacing);
            Rectangle bounds = new Rectangle(bx, currentY, buttonSize, buttonSize);
            bonusButtonBounds.add(bounds);

            boolean hovered = hoveredBonusIndex == i;
            g2.setColor(hovered ? new Color(100, 140, 200, 220) : new Color(60, 80, 120, 200));
            g2.fillRoundRect(bx, currentY, buttonSize, buttonSize, 8, 8);

            g2.setColor(hovered ? new Color(150, 180, 220) : new Color(100, 120, 160));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(bx, currentY, buttonSize, buttonSize, 8, 8);

            g2.setFont(FontManager.getFontForSize(12 * scale, Font.BOLD));
            g2.setColor(Color.WHITE);
            String label = switch (bonus) {
                case BEAM -> "T";
                case SWAP -> "S";
                case PUSH_FIXED -> "F";
                case PUSH_TWICE -> "2";
            };
            FontMetrics fm = g2.getFontMetrics();
            g2.drawString(label, bx + (buttonSize - fm.stringWidth(label)) / 2,
                    currentY + (buttonSize + fm.getAscent() - fm.getDescent()) / 2);
        }

        currentY += buttonSize + Math.round(15 * scale);
        drawDivider(g2, PADDING, width - PADDING, currentY);
        return currentY + Math.round(15 * scale);
    }

    private int drawPlayersSection(Graphics2D g2, int y, int width, float scale) {
        drawSectionHeader(g2, "SPIELER", PADDING, y, scale);
        int currentY = y + Math.round(25 * scale);

        for (int i = 0; i < players.size(); i++) {
            Player p = players.get(i);
            boolean isCurrentTurn = board != null && i == board.getCurrentPlayerIndex();
            currentY = drawPlayerCard(g2, p, currentY, width, isCurrentTurn, i, scale);
            currentY += Math.round(8 * scale);
        }

        return currentY;
    }

    private int drawPlayerCard(Graphics2D g2, Player p, int y, int width, boolean isCurrentTurn, int index, float scale) {
        int cardHeight = Math.round(50 * scale);
        int cardWidth = width - 2 * PADDING;

        Color bgColor = isCurrentTurn ? new Color(80, 100, 140, 200) : new Color(50, 50, 60, 180);
        g2.setColor(bgColor);
        g2.fillRoundRect(PADDING, y, cardWidth, cardHeight, 8, 8);

        if (isCurrentTurn) {
            g2.setColor(new Color(100, 150, 220));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(PADDING, y, cardWidth, cardHeight, 8, 8);
        }

        Color playerColor = GameTheme.Colors.getPlayerColor(index);
        int indicatorSize = Math.round(12 * scale);
        g2.setColor(playerColor);
        g2.fillOval(PADDING + Math.round(10 * scale), y + (cardHeight - indicatorSize) / 2, indicatorSize, indicatorSize);

        g2.setFont(FontManager.getFontForSize(14 * scale, Font.BOLD));
        g2.setColor(isCurrentTurn ? new Color(255, 255, 200) : new Color(200, 200, 220));
        String name = p.getName();
        if (p.isAiControlled()) name += " [AI]";
        g2.drawString(name, PADDING + Math.round(28 * scale), y + Math.round(22 * scale));

        g2.setFont(FontManager.getFontForSize(11 * scale, Font.PLAIN));
        g2.setColor(new Color(150, 150, 170));
        String stats = p.getTreasuresFound().size() + " gefunden";
        g2.drawString(stats, PADDING + Math.round(28 * scale), y + Math.round(38 * scale));

        return y + cardHeight;
    }

    private int drawTargetSection(Graphics2D g2, int y, int width, float scale) {
        if (currentPlayer == null || currentPlayer.getCurrentTargetTreasure() == null) return y;

        int currentY = y + Math.round(10 * scale);
        drawDivider(g2, PADDING, width - PADDING, currentY);
        currentY += Math.round(15 * scale);

        drawSectionHeader(g2, "DEIN ZIEL", PADDING, currentY, scale);
        currentY += Math.round(22 * scale);

        Treasure target = currentPlayer.getCurrentTargetTreasure();

        long time = System.currentTimeMillis();
        int pulseAlpha = 200 + (int) (55 * Math.sin(time / 400.0));

        int boxHeight = Math.round(60 * scale);
        g2.setColor(new Color(255, 215, 0, pulseAlpha));
        g2.fillRoundRect(PADDING + 5, currentY, width - 2 * PADDING - 10, boxHeight, 10, 10);

        g2.setColor(new Color(255, 255, 255, 200));
        g2.setStroke(new BasicStroke(2));
        g2.drawRoundRect(PADDING + 5, currentY, width - 2 * PADDING - 10, boxHeight, 10, 10);

        BufferedImage img = treasureImages.get(target.getId());
        int imgSize = Math.round(40 * scale);
        int imgX = PADDING + Math.round(15 * scale);
        int imgY = currentY + (boxHeight - imgSize) / 2;

        if (img != null) {
            g2.drawImage(img, imgX, imgY, imgSize, imgSize, null);
        } else {
            g2.setColor(new Color(180, 140, 70));
            g2.fillOval(imgX, imgY, imgSize, imgSize);
        }

        g2.setFont(FontManager.getFontForSize(14 * scale, Font.BOLD));
        g2.setColor(Color.BLACK);
        g2.drawString(TreasureUtils.getLocalName(target.getId()),
                imgX + imgSize + Math.round(10 * scale), imgY + imgSize / 2 + 5);

        return currentY + boxHeight + Math.round(10 * scale);
    }

    private void drawKeyboardHints(Graphics2D g2, int width, int height, float scale) {
        g2.setFont(FontManager.getFontForSize(10 * scale, Font.ITALIC));
        g2.setColor(new Color(255, 255, 255, 180));
        g2.drawString("R/Q/E: Karte drehen", PADDING, height - Math.round(15 * scale));
    }

    private void drawSectionHeader(Graphics2D g2, String text, int x, int y, float scale) {
        g2.setFont(FontManager.getFontForSize(12 * scale, Font.BOLD));
        g2.setColor(new Color(255, 215, 0));
        g2.drawString(text, x, y);
    }

    private void drawDivider(Graphics2D g2, int x1, int x2, int y) {
        g2.setColor(new Color(100, 100, 120, 100));
        g2.setStroke(new BasicStroke(1));
        g2.drawLine(x1, y, x2, y);
    }

    private String formatTime(OffsetDateTime endTime) {
        if (endTime == null) return "--:--";

        Duration remaining = Duration.between(OffsetDateTime.now(), endTime);
        if (remaining.isNegative()) return "00:00";

        long totalSeconds = remaining.getSeconds();
        long hours = totalSeconds / 3600;
        long minutes = (totalSeconds % 3600) / 60;
        long seconds = totalSeconds % 60;

        if (hours > 0) {
            return String.format("%d:%02d:%02d", hours, minutes, seconds);
        }
        return String.format("%d:%02d", minutes, seconds);
    }
}
