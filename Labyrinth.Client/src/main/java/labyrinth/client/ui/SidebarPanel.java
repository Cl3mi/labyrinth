package labyrinth.client.ui;

import labyrinth.client.models.Board;
import labyrinth.client.models.Player;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.Treasure;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Sidebar panel that displays game information, player list, and controls.
 * Separated from BoardPanel to improve performance by only repainting when needed.
 */
public class SidebarPanel extends JPanel {

    // Font cache for performance
    private static final Map<String, Font> fontCache = new HashMap<>();

    private static Font getCachedFont(String family, int style, int size) {
        String key = family + "_" + style + "_" + size;
        return fontCache.computeIfAbsent(key, k -> new Font(family, style, size));
    }

    // State
    private Board board;
    private Player currentPlayer;
    private List<Player> players;
    private OffsetDateTime gameEndTime;
    private OffsetDateTime turnEndTime;
    private labyrinth.contracts.models.TurnState currentTurnState;
    private Map<String, BufferedImage> treasureImages;

    // Bonus system state
    private BonusType activeBonusMode = null;
    private final List<Rectangle> bonusButtonBounds = new ArrayList<>();
    private int hoveredBonusIndex = -1;

    // AI button state
    private Rectangle aiMoveButtonBounds = null;
    private boolean aiMoveButtonHovered = false;
    private boolean aiModeEnabled = false;
    private boolean aiThinking = false;

    // Callbacks
    private Runnable onAiToggleRequested;
    private Consumer<BonusType> onBonusClicked;

    // Timers
    private final Timer countdownTimer;
    private final Timer animationTimer;

    // Animation state
    private static final int ANIMATION_INTERVAL = 16; // ~60fps

    public SidebarPanel() {
        setOpaque(false);
        setPreferredSize(new Dimension(300, 600));

        setupMouseListener();

        // Timer for countdown updates (1 second)
        countdownTimer = new Timer(1000, e -> {
            if (gameEndTime != null || turnEndTime != null) {
                repaint();
            }
        });
        countdownTimer.start();

        // Animation timer for smooth effects (60fps when needed)
        animationTimer = new Timer(ANIMATION_INTERVAL, e -> {
            if (needsAnimationUpdate()) {
                repaint();
            }
        });
        animationTimer.start();
    }

    private boolean needsAnimationUpdate() {
        // AI thinking animation
        if (aiThinking) {
            return true;
        }
        // Target treasure pulsing in sidebar
        if (currentPlayer != null && currentPlayer.getCurrentTargetTreasure() != null) {
            return true;
        }
        // Active bonus mode hint
        if (activeBonusMode != null) {
            return true;
        }
        return false;
    }

    private void setupMouseListener() {
        MouseAdapter mouseAdapter = new MouseAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                updateHoverStates(e.getPoint());
            }

            @Override
            public void mouseExited(MouseEvent e) {
                boolean needsRepaint = hoveredBonusIndex != -1 || aiMoveButtonHovered;
                hoveredBonusIndex = -1;
                aiMoveButtonHovered = false;
                if (needsRepaint) repaint();
            }

            @Override
            public void mouseClicked(MouseEvent e) {
                handleClick(e.getPoint());
            }
        };
        addMouseListener(mouseAdapter);
        addMouseMotionListener(mouseAdapter);
    }

    private void updateHoverStates(Point p) {
        boolean needsRepaint = false;

        // Check AI button
        boolean wasAiHovered = aiMoveButtonHovered;
        aiMoveButtonHovered = aiMoveButtonBounds != null && aiMoveButtonBounds.contains(p);
        if (wasAiHovered != aiMoveButtonHovered) needsRepaint = true;

        // Check bonus buttons
        int oldHoveredBonus = hoveredBonusIndex;
        hoveredBonusIndex = -1;
        for (int i = 0; i < bonusButtonBounds.size(); i++) {
            if (bonusButtonBounds.get(i).contains(p)) {
                hoveredBonusIndex = i;
                break;
            }
        }
        if (oldHoveredBonus != hoveredBonusIndex) needsRepaint = true;

        if (needsRepaint) repaint();
    }

    private void handleClick(Point p) {
        // AI button click
        if (aiMoveButtonBounds != null && aiMoveButtonBounds.contains(p)) {
            if (onAiToggleRequested != null) {
                onAiToggleRequested.run();
            }
            return;
        }

        // Bonus button click
        if (currentPlayer != null && onBonusClicked != null) {
            List<BonusType> bonuses = currentPlayer.getAvailableBonuses();
            for (int i = 0; i < bonusButtonBounds.size() && i < bonuses.size(); i++) {
                if (bonusButtonBounds.get(i).contains(p)) {
                    onBonusClicked.accept(bonuses.get(i));
                    return;
                }
            }
        }
    }


    public void dispose() {
        countdownTimer.stop();
        animationTimer.stop();
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            drawSidebar(g2);
        } finally {
            g2.dispose();
        }
    }

    private void drawSidebar(Graphics2D g2) {
        int width = getWidth();
        int height = getHeight();
        int padding = 12;

        // Calculate scale factor for fonts
        float scale = Math.max(0.8f, width / 320f);

        // Scaled font sizes
        int headerFontSize = Math.round(22 * scale);
        int timerFontSize = Math.round(16 * scale);
        int playerNameFontSize = Math.round(18 * scale);
        int stateFontSize = Math.round(11 * scale);
        int turnTimerFontSize = Math.round(12 * scale);
        int hintFontSize = Math.round(10 * scale);
        int scaledPadding = Math.round(padding * scale);

        // Background
        GradientPaint gradient = new GradientPaint(
                0, 0, new Color(40, 40, 50, 240),
                0, height, new Color(30, 30, 40, 240)
        );
        g2.setPaint(gradient);
        g2.fillRoundRect(0, 0, width, height, 15, 15);

        // Border
        g2.setColor(new Color(100, 130, 180, 150));
        g2.setStroke(new BasicStroke(3));
        g2.drawRoundRect(0, 0, width - 1, height - 1, 15, 15);

        int currentY = padding;

        // Header space
        g2.setFont(getCachedFont("Arial", Font.BOLD, headerFontSize));
        g2.setColor(new Color(255, 215, 0));
        currentY += Math.round(35 * scale);

        // Game timer section
        if (gameEndTime != null) {
            drawSectionHeader(g2, "SPIEL-TIMER", padding, currentY, scale);
            currentY += Math.round(22 * scale);

            g2.setFont(getCachedFont("Arial", Font.BOLD, timerFontSize));
            g2.setColor(new Color(255, 200, 100));
            g2.drawString(formatTimeRemaining(gameEndTime), padding + scaledPadding, currentY);
            currentY += Math.round(25 * scale);
        }

        // Divider
        drawDivider(g2, padding, width - padding, currentY);
        currentY += Math.round(15 * scale);

        // Current turn info
        List<Player> allPlayers = (players != null && !players.isEmpty()) ? players :
                (board != null && board.getPlayers() != null) ? board.getPlayers() : List.of();

        if (!allPlayers.isEmpty() && board != null) {
            Player currentTurnPlayer = allPlayers.get(board.getCurrentPlayerIndex());

            drawSectionHeader(g2, "AKTUELLER ZUG", padding, currentY, scale);
            currentY += Math.round(25 * scale);

            g2.setFont(getCachedFont("Arial", Font.BOLD, playerNameFontSize));
            g2.setColor(new Color(255, 255, 150));
            String turnText = currentTurnPlayer.getName();
            if (currentTurnPlayer.isAiControlled()) {
                turnText += " [AI]";
            }
            g2.drawString(turnText, padding + scaledPadding, currentY);
            currentY += Math.round(28 * scale);

            // Turn state
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
            g2.drawString(stateText, padding + scaledPadding, currentY);
            currentY += Math.round(18 * scale);

            // Turn timer
            if (turnEndTime != null) {
                g2.setFont(getCachedFont("Arial", Font.BOLD, turnTimerFontSize));
                g2.setColor(new Color(255, 150, 150));
                g2.drawString("Zeit: " + formatTimeRemaining(turnEndTime), padding + scaledPadding, currentY);
                currentY += Math.round(20 * scale);
            } else {
                currentY += Math.round(7 * scale);
            }

            // Hint for staying in place
            if (currentTurnState == labyrinth.contracts.models.TurnState.WAITING_FOR_MOVE) {
                g2.setFont(getCachedFont("Arial", Font.ITALIC, hintFontSize));
                g2.setColor(new Color(150, 150, 170));
                g2.drawString("(Click your tile to stay in place)", padding + scaledPadding, currentY);
                currentY += Math.round(15 * scale);
            }
        }

        // Divider
        drawDivider(g2, padding, width - padding, currentY);
        currentY += Math.round(15 * scale);

        // AI Move button
        currentY = drawAiMoveButton(g2, width, padding, currentY, scale);
        currentY += Math.round(15 * scale);

        // Bonus section
        if (currentPlayer != null && !currentPlayer.getAvailableBonuses().isEmpty()) {
            currentY = drawBonusSection(g2, width, padding, currentY, scale);
        }

        // Divider before players section
        drawDivider(g2, padding, width - padding, currentY);
        currentY += Math.round(20 * scale);

        // Players section
        drawSectionHeader(g2, "SPIELER", padding, currentY, scale);
        currentY += Math.round(25 * scale);

        for (int i = 0; i < allPlayers.size(); i++) {
            Player p = allPlayers.get(i);
            boolean isCurrentTurn = (board != null && i == board.getCurrentPlayerIndex());
            currentY = drawPlayerCard(g2, p, padding, currentY, width - 2 * padding, isCurrentTurn, i, scale);
            currentY += Math.round(12 * scale);
        }

        // Current target treasure section
        if (currentPlayer != null && currentPlayer.getCurrentTargetTreasure() != null) {
            currentY += Math.round(10 * scale);
            drawDivider(g2, padding, width - padding, currentY);
            currentY += Math.round(15 * scale);

            drawSectionHeader(g2, "DEINE ZIELE", padding, currentY, scale);
            currentY += Math.round(22 * scale);

            Treasure currentTarget = currentPlayer.getCurrentTargetTreasure();

            // Pulsing effect
            long time = System.currentTimeMillis();
            int pulseAlpha = 200 + (int) (55 * Math.sin(time / 400.0));

            int boxHeight = Math.round(70 * scale);
            g2.setColor(new Color(255, 215, 0, pulseAlpha));
            g2.fillRoundRect(padding + 5, currentY - Math.round(15 * scale), width - 2 * padding - 10, boxHeight, 10, 10);

            g2.setColor(new Color(255, 255, 255, 200));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(padding + 5, currentY - Math.round(15 * scale), width - 2 * padding - 10, boxHeight, 10, 10);

            g2.setFont(getCachedFont("Arial", Font.BOLD, Math.round(10 * scale)));
            g2.setColor(new Color(100, 70, 0));
            g2.drawString("AKTUELLES ZIEL:", padding + Math.round(15 * scale), currentY - 2);

            // Treasure image
            int imgSize = Math.round(40 * scale);
            int imgX = padding + Math.round(15 * scale);
            int imgY = currentY + Math.round(5 * scale);

            if (treasureImages != null) {
                BufferedImage treasureImg = treasureImages.get(currentTarget.getName());
                if (treasureImg != null) {
                    g2.drawImage(treasureImg, imgX, imgY, imgSize, imgSize, null);
                } else {
                    g2.setColor(new Color(180, 140, 70));
                    g2.fillOval(imgX, imgY, imgSize, imgSize);
                }
            }

            g2.setFont(getCachedFont("Arial", Font.BOLD, Math.round(16 * scale)));
            g2.setColor(new Color(0, 0, 0));
            g2.drawString(currentTarget.getName(), imgX + imgSize + Math.round(10 * scale), imgY + imgSize / 2 + 5);
        }

        // Keyboard hints at bottom
        int hintsHeight = Math.round(80 * scale);
        currentY = Math.max(currentY + Math.round(10 * scale), height - hintsHeight);
        drawDivider(g2, padding, width - padding, currentY);
        currentY += Math.round(15 * scale);

        g2.setFont(getCachedFont("Arial", Font.ITALIC, hintFontSize));
        g2.setColor(new Color(150, 150, 170));
        g2.drawString("Pfeiltasten: Navigation", padding, currentY);
        currentY += Math.round(15 * scale);
        g2.drawString("R/Q/E: Tile drehen", padding, currentY);
        currentY += Math.round(15 * scale);
        g2.drawString("Tab: Tastaturhilfe", padding, currentY);
    }

    private int drawAiMoveButton(Graphics2D g2, int width, int padding, int currentY, float scale) {
        int buttonWidth = width - 2 * padding - Math.round(20 * scale);
        int buttonHeight = Math.round(36 * scale);
        int buttonX = padding + Math.round(10 * scale);
        int buttonY = currentY;

        aiMoveButtonBounds = new Rectangle(buttonX, buttonY, buttonWidth, buttonHeight);

        // Button background
        if (aiThinking) {
            long time = System.currentTimeMillis();
            int alpha = (int) (180 + 40 * Math.sin(time / 200.0));
            g2.setColor(new Color(200, 150, 50, alpha));
        } else if (aiModeEnabled) {
            g2.setColor(aiMoveButtonHovered ? new Color(60, 180, 80, 220) : new Color(50, 150, 70, 200));
        } else {
            g2.setColor(aiMoveButtonHovered ? new Color(80, 100, 140, 200) : new Color(60, 80, 110, 180));
        }
        g2.fillRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

        // Border
        if (aiThinking) {
            g2.setColor(new Color(255, 200, 100, 255));
            g2.setStroke(new BasicStroke(2));
        } else if (aiModeEnabled) {
            g2.setColor(new Color(100, 220, 120, 255));
            g2.setStroke(new BasicStroke(2));
        } else if (aiMoveButtonHovered) {
            g2.setColor(new Color(120, 150, 200, 200));
            g2.setStroke(new BasicStroke(2));
        } else {
            g2.setColor(new Color(90, 110, 140, 150));
            g2.setStroke(new BasicStroke(1));
        }
        g2.drawRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

        // Text
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

    private int drawBonusSection(Graphics2D g2, int width, int padding, int currentY, float scale) {
        drawSectionHeader(g2, "BONI", padding, currentY, scale);
        currentY += Math.round(25 * scale);

        List<BonusType> bonuses = currentPlayer.getAvailableBonuses();
        bonusButtonBounds.clear();

        int buttonWidth = width - 2 * padding - Math.round(20 * scale);
        int buttonHeight = Math.round(40 * scale);
        int buttonSpacing = Math.round(8 * scale);

        for (int i = 0; i < bonuses.size(); i++) {
            BonusType bonus = bonuses.get(i);
            int buttonX = padding + Math.round(10 * scale);
            int buttonY = currentY;

            bonusButtonBounds.add(new Rectangle(buttonX, buttonY, buttonWidth, buttonHeight));

            boolean isHovered = (i == hoveredBonusIndex);
            boolean isActive = (bonus == activeBonusMode);

            // Background
            if (isActive) {
                g2.setColor(new Color(100, 200, 100, 200));
            } else if (isHovered) {
                g2.setColor(new Color(80, 120, 180, 200));
            } else {
                g2.setColor(new Color(60, 60, 80, 200));
            }
            g2.fillRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

            // Border
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

            // Text
            String bonusName = getBonusDisplayName(bonus);
            String bonusIcon = getBonusIcon(bonus);

            int fontSize = Math.round(14 * scale);
            g2.setFont(getCachedFont("Arial", Font.BOLD, fontSize));
            g2.setColor(isActive ? new Color(220, 255, 220) : Color.WHITE);
            g2.drawString(bonusIcon + " " + bonusName, buttonX + Math.round(10 * scale), buttonY + Math.round(26 * scale));

            currentY += buttonHeight + buttonSpacing;
        }

        // Active bonus hint
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
                g2.drawString(hint, padding + Math.round(10 * scale), currentY);
                currentY += Math.round(15 * scale);
            }
        }

        return currentY;
    }

    private int drawPlayerCard(Graphics2D g2, Player player, int x, int y, int width, boolean isCurrentTurn, int playerIndex, float scale) {
        int cardHeight = Math.round(95 * scale);
        int padding = Math.round(10 * scale);

        // Card background
        if (isCurrentTurn) {
            g2.setColor(new Color(60, 100, 60, 200));
        } else {
            g2.setColor(new Color(50, 50, 60, 180));
        }
        g2.fillRoundRect(x, y, width, cardHeight, 10, 10);

        // Border
        Color playerColor = getAwtColor(player.getColor());
        g2.setColor(isCurrentTurn ? new Color(100, 200, 100, 200) : new Color(playerColor.getRed(), playerColor.getGreen(), playerColor.getBlue(), 150));
        g2.setStroke(new BasicStroke(isCurrentTurn ? 2 : 1));
        g2.drawRoundRect(x, y, width, cardHeight, 10, 10);

        int currentY = y + padding + Math.round(15 * scale);

        // Player icon
        int circleSize = Math.round(32 * scale);
        g2.setColor(playerColor);
        g2.fillOval(x + padding, y + padding + 3, circleSize, circleSize);
        g2.setColor(Color.WHITE);
        g2.setStroke(new BasicStroke(1.5f));
        g2.drawOval(x + padding, y + padding + 3, circleSize, circleSize);

        // Player name
        int nameFontSize = Math.round(14 * scale);
        g2.setFont(getCachedFont("Arial", Font.BOLD, nameFontSize));
        g2.setColor(Color.WHITE);
        String name = player.getName();
        if (name.length() > 15) {
            name = name.substring(0, 12) + "...";
        }
        g2.drawString(name, x + padding + Math.round(42 * scale), currentY);

        // Badges
        int badgeFontSize = Math.round(10 * scale);
        int badgeX = x + width - padding - Math.round(20 * scale);
        g2.setFont(getCachedFont("Arial", Font.PLAIN, badgeFontSize));

        if (player.isAiControlled()) {
            g2.setColor(new Color(150, 150, 255));
            g2.drawString("AI", badgeX, currentY);
        } else if (!player.isConnected()) {
            g2.setColor(new Color(200, 80, 80));
            g2.drawString("OFFLINE", badgeX - Math.round(40 * scale), currentY);
        }

        if (player.isAdmin()) {
            g2.setColor(new Color(255, 215, 0));
            g2.drawString("*", x + width - padding - 5, currentY);
        }

        currentY += Math.round(20 * scale);

        // Score
        int treasuresFound = player.getTreasuresFound() != null ? player.getTreasuresFound().size() : 0;
        int totalTreasures = treasuresFound + player.getRemainingTreasureCount();

        int scoreFontSize = Math.round(12 * scale);
        g2.setFont(getCachedFont("Arial", Font.PLAIN, scoreFontSize));
        g2.setColor(new Color(200, 200, 220));
        g2.drawString("Treasures: " + treasuresFound + "/" + totalTreasures, x + padding + Math.round(24 * scale), currentY);
        currentY += Math.round(18 * scale);

        // Progress bar
        if (totalTreasures > 0) {
            int barWidth = width - 2 * padding - Math.round(24 * scale);
            int barHeight = Math.round(8 * scale);
            int barX = x + padding + Math.round(24 * scale);

            g2.setColor(new Color(60, 60, 70));
            g2.fillRoundRect(barX, currentY - Math.round(6 * scale), barWidth, barHeight, 4, 4);

            int progressWidth = (int) ((double) treasuresFound / totalTreasures * barWidth);
            if (progressWidth > 0) {
                g2.setColor(new Color(100, 200, 100));
                g2.fillRoundRect(barX, currentY - Math.round(6 * scale), progressWidth, barHeight, 4, 4);
            }
        }

        return y + cardHeight;
    }

    private void drawSectionHeader(Graphics2D g2, String text, int x, int y, float scale) {
        int fontSize = Math.round(13 * scale);
        g2.setFont(getCachedFont("Arial", Font.BOLD, fontSize));
        g2.setColor(new Color(180, 200, 255));
        g2.drawString(text, x, y);
    }

    private void drawDivider(Graphics2D g2, int x1, int x2, int y) {
        GradientPaint dividerGradient = new GradientPaint(
                x1, y, new Color(100, 130, 180, 50),
                (x1 + x2) / 2, y, new Color(100, 130, 180, 180)
        );
        g2.setPaint(dividerGradient);
        g2.setStroke(new BasicStroke(2));
        g2.drawLine(x1, y, x2, y);
    }

    private String formatTimeRemaining(OffsetDateTime endTime) {
        if (endTime == null) return "--:--";

        Duration remaining = Duration.between(OffsetDateTime.now(), endTime);
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

    private String getBonusDisplayName(BonusType bonus) {
        return switch (bonus) {
            case BEAM -> "Teleportieren";
            case SWAP -> "Tauschen";
            case PUSH_FIXED -> "Fixiert schieben";
            case PUSH_TWICE -> "Doppelt schieben";
        };
    }

    private String getBonusIcon(BonusType bonus) {
        return switch (bonus) {
            case BEAM -> "[T]";
            case SWAP -> "[S]";
            case PUSH_FIXED -> "[F]";
            case PUSH_TWICE -> "[2]";
        };
    }

    private Color getAwtColor(labyrinth.contracts.models.PlayerColor playerColor) {
        return switch (playerColor) {
            case RED -> new Color(220, 80, 80);
            case BLUE -> new Color(80, 140, 220);
            case GREEN -> new Color(80, 200, 120);
            case YELLOW -> new Color(230, 200, 80);
        };
    }
}
