package labyrinth.client.ui.components;

import labyrinth.client.models.Player;

import labyrinth.client.ui.utils.ResourceLoader;
import labyrinth.contracts.models.BonusType;
import labyrinth.contracts.models.PlayerColor;
import labyrinth.contracts.models.Treasure;
import labyrinth.contracts.models.TurnState;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class SidebarPanel extends JPanel {

    private final ResourceLoader resources;

    // State
    private List<Player> players = new ArrayList<>();
    private Player currentPlayer;
    private int currentPlayerIndex = -1;

    private OffsetDateTime gameEndTime;
    private OffsetDateTime turnEndTime;
    private TurnState currentTurnState;

    private BonusType activeBonusMode;
    private boolean aiModeEnabled;
    private boolean aiThinking;

    // Interaction State
    private final List<Rectangle> bonusButtonBounds = new ArrayList<>();
    private int hoveredBonusIndex = -1;

    private Rectangle aiMoveButtonBounds;
    private boolean aiMoveButtonHovered;

    // Callbacks
    private Consumer<BonusType> onBonusClicked;
    private Runnable onAiToggle;

    public SidebarPanel() {
        this.resources = ResourceLoader.getInstance();
        setOpaque(false);

        setupMouseListeners();
    }

    private void setupMouseListeners() {
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                handleClick(e.getPoint());
            }

            @Override
            public void mouseExited(MouseEvent e) {
                hoveredBonusIndex = -1;
                aiMoveButtonHovered = false;
                repaint();
            }
        });

        addMouseMotionListener(new MouseMotionAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                handleMouseMove(e.getPoint());
            }
        });
    }

    public void setCallbacks(Consumer<BonusType> onBonusClicked, Runnable onAiToggle) {
        this.onBonusClicked = onBonusClicked;
        this.onAiToggle = onAiToggle;
    }

    public void updateState(List<Player> players, Player currentPlayer, int currentPlayerIndex,
            OffsetDateTime gameEndTime, OffsetDateTime turnEndTime, TurnState turnState,
            BonusType activeBonusMode, boolean aiModeEnabled, boolean aiThinking) {
        this.players = players != null ? players : new ArrayList<>();
        this.currentPlayer = currentPlayer;
        this.currentPlayerIndex = currentPlayerIndex;
        this.gameEndTime = gameEndTime;
        this.turnEndTime = turnEndTime;
        this.currentTurnState = turnState;
        this.activeBonusMode = activeBonusMode;
        this.aiModeEnabled = aiModeEnabled;
        this.aiThinking = aiThinking;

        repaint();
    }

    // --- Input Handling ---

    private void handleClick(Point p) {
        // AI Button
        if (aiMoveButtonBounds != null && aiMoveButtonBounds.contains(p)) {
            if (onAiToggle != null)
                onAiToggle.run();
            return;
        }

        // Bonus Buttons
        if (currentPlayer != null) {
            List<BonusType> bonuses = currentPlayer.getAvailableBonuses();
            for (int i = 0; i < bonusButtonBounds.size() && i < bonuses.size(); i++) {
                if (bonusButtonBounds.get(i).contains(p)) {
                    if (onBonusClicked != null) {
                        onBonusClicked.accept(bonuses.get(i));
                    }
                    return;
                }
            }
        }
    }

    private void handleMouseMove(Point p) {
        boolean repaintNeeded = false;

        // AI Button Hover
        boolean newAiHover = aiMoveButtonBounds != null && aiMoveButtonBounds.contains(p);
        if (newAiHover != aiMoveButtonHovered) {
            aiMoveButtonHovered = newAiHover;
            repaintNeeded = true;
        }

        // Bonus Button Hover
        int newBonusHover = -1;
        if (currentPlayer != null) {
            List<BonusType> bonuses = currentPlayer.getAvailableBonuses();
            for (int i = 0; i < bonusButtonBounds.size() && i < bonuses.size(); i++) {
                if (bonusButtonBounds.get(i).contains(p)) {
                    newBonusHover = i;
                    break;
                }
            }
        }

        if (newBonusHover != hoveredBonusIndex) {
            hoveredBonusIndex = newBonusHover;
            repaintNeeded = true;
        }

        if (repaintNeeded) {
            repaint();
        }
    }

    // --- Drawing ---

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

        drawSidebar(g2);

        g2.dispose();
    }

    private void drawSidebar(Graphics2D g2) {
        int sidebarWidth = getWidth();
        int sidebarX = 0;
        int sidebarY = 60; // Offset from top (matches original design)
        int padding = 12;

        float sidebarScale = Math.max(0.8f, sidebarWidth / 320f);
        int scaledPadding = Math.round(padding * sidebarScale);

        // Background
        GradientPaint gradient = new GradientPaint(
                sidebarX, sidebarY,
                new Color(40, 40, 50, 240),
                sidebarX, getHeight(),
                new Color(30, 30, 40, 240));
        g2.setPaint(gradient);
        g2.fillRoundRect(sidebarX, sidebarY, sidebarWidth, getHeight() - sidebarY - 20, 15, 15);

        // Border
        g2.setColor(new Color(100, 130, 180, 150));
        g2.setStroke(new BasicStroke(3));
        g2.drawRoundRect(sidebarX, sidebarY, sidebarWidth, getHeight() - sidebarY - 20, 15, 15);

        int currentY = sidebarY + padding;

        // Header
        int headerFontSize = Math.round(22 * sidebarScale);
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, headerFontSize));
        g2.setColor(new Color(255, 215, 0));
        currentY += Math.round(35 * sidebarScale);

        // Game timer
        if (gameEndTime != null) {
            drawSectionHeader(g2, "SPIEL-TIMER", sidebarX + padding, currentY, sidebarScale);
            currentY += Math.round(22 * sidebarScale);

            int timerFontSize = Math.round(16 * sidebarScale);
            g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, timerFontSize));
            g2.setColor(new Color(255, 200, 100));
            g2.drawString(formatTimeRemaining(gameEndTime), sidebarX + padding + scaledPadding, currentY);
            currentY += Math.round(25 * sidebarScale);
        }

        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(15 * sidebarScale);

        // Current turn info
        if (!players.isEmpty() && currentPlayerIndex >= 0 && currentPlayerIndex < players.size()) {
            Player currentTurnPlayer = players.get(currentPlayerIndex);

            drawSectionHeader(g2, "AKTUELLER ZUG", sidebarX + padding, currentY, sidebarScale);
            currentY += Math.round(25 * sidebarScale);

            int playerNameFontSize = Math.round(18 * sidebarScale);
            g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, playerNameFontSize));
            g2.setColor(new Color(255, 255, 150));
            String turnText = currentTurnPlayer.getName();
            if (currentTurnPlayer.isAiControlled()) {
                turnText += " [AI]";
            }
            g2.drawString(turnText, sidebarX + padding + scaledPadding, currentY);
            currentY += Math.round(28 * sidebarScale);

            int stateFontSize = Math.round(11 * sidebarScale);
            g2.setFont(ResourceLoader.getCachedFont("Arial", Font.PLAIN, stateFontSize));
            g2.setColor(new Color(180, 180, 200));
            String stateText = (currentTurnState != null) ? switch (currentTurnState) {
                case WAITING_FOR_PUSH -> "Waiting for tile push";
                case WAITING_FOR_MOVE -> "Waiting for pawn move";
            } : "WAITING";

            g2.drawString(stateText, sidebarX + padding + scaledPadding, currentY);
            currentY += Math.round(18 * sidebarScale);

            if (turnEndTime != null) {
                int turnTimerFontSize = Math.round(12 * sidebarScale);
                g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, turnTimerFontSize));
                g2.setColor(new Color(255, 150, 150));
                g2.drawString("Zeit: " + formatTimeRemaining(turnEndTime), sidebarX + padding + scaledPadding,
                        currentY);
                currentY += Math.round(20 * sidebarScale);
            } else {
                currentY += Math.round(7 * sidebarScale);
            }
        }

        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(15 * sidebarScale);

        // AI Button
        currentY = drawAiMoveButton(g2, sidebarX, sidebarWidth, padding, currentY, sidebarScale);
        currentY += Math.round(15 * sidebarScale);

        // Bonuses
        if (currentPlayer != null && !currentPlayer.getAvailableBonuses().isEmpty()) {
            currentY = drawBonusSection(g2, sidebarX, sidebarWidth, padding, currentY, sidebarScale);
        }

        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(20 * sidebarScale);

        // Players List
        drawSectionHeader(g2, "SPIELER", sidebarX + padding, currentY, sidebarScale);
        currentY += Math.round(25 * sidebarScale);

        for (int i = 0; i < players.size(); i++) {
            Player p = players.get(i);
            boolean isCurrentTurn = (i == currentPlayerIndex);
            currentY = drawPlayerCard(g2, p, sidebarX + padding, currentY, sidebarWidth - 2 * padding, isCurrentTurn, i,
                    sidebarScale);
            currentY += Math.round(12 * sidebarScale);
        }

        // Current Target
        if (currentPlayer != null && currentPlayer.getCurrentTargetTreasure() != null) {
            currentY += Math.round(10 * sidebarScale);
            drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
            currentY += Math.round(15 * sidebarScale);

            drawTargetSection(g2, sidebarX, sidebarWidth, padding, currentY, sidebarScale);

            // Note: We need to calculate height consumed by drawTargetSection
            // For now, it's at the bottom so it's fine.
        }

        // Key hints
        drawKeyHints(g2, sidebarX, sidebarWidth, padding, sidebarScale);
    }

    private void drawKeyHints(Graphics2D g2, int sidebarX, int sidebarWidth, int padding, float scale) {
        int hintsHeight = Math.round(80 * scale);
        int currentY = getHeight() - hintsHeight;

        drawDivider(g2, sidebarX + padding, sidebarX + sidebarWidth - padding, currentY);
        currentY += Math.round(15 * scale);

        int hintFontSize = Math.round(10 * scale);
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.ITALIC, hintFontSize));
        g2.setColor(new Color(150, 150, 170));
        g2.drawString("Pfeiltasten: Navigation", sidebarX + padding, currentY);
        currentY += Math.round(15 * scale);
        g2.drawString("R/Q/E: Tile drehen", sidebarX + padding, currentY);
        currentY += Math.round(15 * scale);
        g2.drawString("Tab: Tastaturhilfe", sidebarX + padding, currentY);
    }

    private void drawTargetSection(Graphics2D g2, int sidebarX, int sidebarWidth, int padding, int currentY,
            float scale) {
        drawSectionHeader(g2, "DEINE ZIELE", sidebarX + padding, currentY, scale);
        currentY += Math.round(22 * scale);

        Treasure currentTarget = currentPlayer.getCurrentTargetTreasure();
        long time = System.currentTimeMillis();
        int pulseAlpha = 200 + (int) (55 * Math.sin(time / 400.0));

        int boxHeight = Math.round(70 * scale);
        g2.setColor(new Color(255, 215, 0, pulseAlpha));
        g2.fillRoundRect(sidebarX + padding + 5, currentY - Math.round(15 * scale), sidebarWidth - 2 * padding - 10,
                boxHeight, 10, 10);

        g2.setColor(new Color(255, 255, 255, 200));
        g2.setStroke(new BasicStroke(2));
        g2.drawRoundRect(sidebarX + padding + 5, currentY - Math.round(15 * scale), sidebarWidth - 2 * padding - 10,
                boxHeight, 10, 10);

        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, Math.round(10 * scale)));
        g2.setColor(new Color(100, 70, 0));
        g2.drawString("AKTUELLES ZIEL:", sidebarX + padding + Math.round(15 * scale), currentY - 2);

        java.awt.image.BufferedImage treasureImg = resources.getTreasureImage(currentTarget.getName());
        int imgSize = Math.round(40 * scale);
        int imgX = sidebarX + padding + Math.round(15 * scale);
        int imgY = currentY + Math.round(5 * scale);

        if (treasureImg != null) {
            g2.drawImage(treasureImg, imgX, imgY, imgSize, imgSize, null);
        } else {
            g2.setColor(new Color(180, 140, 70));
            g2.fillOval(imgX, imgY, imgSize, imgSize);
        }

        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, Math.round(16 * scale)));
        g2.setColor(new Color(0, 0, 0));
        g2.drawString(currentTarget.getName(), imgX + imgSize + Math.round(10 * scale), imgY + imgSize / 2 + 5);
    }

    private int drawAiMoveButton(Graphics2D g2, int sidebarX, int sidebarWidth, int padding, int currentY,
            float scale) {
        int buttonWidth = sidebarWidth - 2 * padding - Math.round(20 * scale);
        int buttonHeight = Math.round(36 * scale);
        int buttonX = sidebarX + padding + Math.round(10 * scale);
        int buttonY = currentY;

        aiMoveButtonBounds = new Rectangle(buttonX, buttonY, buttonWidth, buttonHeight);

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

        int fontSize = Math.round(14 * scale);
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, fontSize));
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

    private int drawBonusSection(Graphics2D g2, int sidebarX, int sidebarWidth, int padding, int currentY,
            float scale) {
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

            boolean isHovered = (i == hoveredBonusIndex);
            boolean isActive = (bonus == activeBonusMode);

            if (isActive) {
                g2.setColor(new Color(100, 200, 100, 200));
            } else if (isHovered) {
                g2.setColor(new Color(80, 120, 180, 200));
            } else {
                g2.setColor(new Color(60, 60, 80, 200));
            }
            g2.fillRoundRect(buttonX, buttonY, buttonWidth, buttonHeight, 10, 10);

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

            String bonusName = getBonusDisplayName(bonus);
            String bonusIcon = getBonusIcon(bonus);

            int fontSize = Math.round(14 * scale);
            g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, fontSize));
            g2.setColor(isActive ? new Color(220, 255, 220) : new Color(255, 255, 255));
            g2.drawString(bonusIcon + " " + bonusName, buttonX + Math.round(10 * scale),
                    buttonY + Math.round(26 * scale));

            currentY += buttonHeight + buttonSpacing;
        }

        if (activeBonusMode != null) {
            int hintFontSize = Math.round(10 * scale);
            g2.setFont(ResourceLoader.getCachedFont("Arial", Font.ITALIC, hintFontSize));
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

    private void drawSectionHeader(Graphics2D g2, String text, int x, int y, float scale) {
        int fontSize = Math.round(13 * scale);
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, fontSize));
        g2.setColor(new Color(180, 200, 255));
        g2.drawString(text, x, y);
    }

    private void drawDivider(Graphics2D g2, int x1, int x2, int y) {
        GradientPaint dividerGradient = new GradientPaint(
                x1, y,
                new Color(100, 130, 180, 50),
                (x1 + x2) / 2, y,
                new Color(100, 130, 180, 180));
        g2.setPaint(dividerGradient);
        g2.setStroke(new BasicStroke(2));
        g2.drawLine(x1, y, x2, y);
    }

    private int drawPlayerCard(Graphics2D g2, Player player, int x, int y, int width, boolean isCurrentTurn,
            int playerIndex, float scale) {
        int cardHeight = Math.round(95 * scale);
        int padding = Math.round(10 * scale);

        Color bgColor = isCurrentTurn ? new Color(80, 100, 140, 200) : new Color(50, 50, 60, 180);
        g2.setColor(bgColor);
        g2.fillRoundRect(x, y, width, cardHeight, 10, 10);

        if (isCurrentTurn) {
            g2.setColor(new Color(255, 220, 100));
            g2.setStroke(new BasicStroke(2));
            g2.drawRoundRect(x, y, width, cardHeight, 10, 10);
        }

        int currentY = y + padding + Math.round(15 * scale);

        int iconSize = Math.round(32 * scale);
        java.awt.image.BufferedImage icon = resources.getPlayerIcon(playerIndex);
        if (icon != null) {
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
            int offsetX = (iconSize - drawW) / 2;
            int offsetY = (iconSize - drawH) / 2;
            g2.drawImage(icon, x + 2 + offsetX, y + padding + offsetY, drawW, drawH, null);
        } else {
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

        int nameFontSize = Math.round(14 * scale);
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.BOLD, nameFontSize));
        g2.setColor(Color.WHITE);
        String name = player.getName();
        if (name.length() > 15) {
            name = name.substring(0, 12) + "...";
        }
        g2.drawString(name, x + padding + Math.round(42 * scale), currentY);

        int badgeFontSize = Math.round(10 * scale);
        int badgeX = x + width - padding - Math.round(20 * scale);
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.PLAIN, badgeFontSize));

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

        int treasuresFound = player.getTreasuresFound() != null ? player.getTreasuresFound().size() : 0;
        int totalTreasures = treasuresFound + player.getRemainingTreasureCount();

        int scoreFontSize = Math.round(12 * scale);
        g2.setFont(ResourceLoader.getCachedFont("Arial", Font.PLAIN, scoreFontSize));
        g2.setColor(new Color(200, 200, 220));
        g2.drawString("Treasures: " + treasuresFound + "/" + totalTreasures, x + padding + Math.round(24 * scale),
                currentY);
        currentY += Math.round(18 * scale);

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

    private Color getAwtColor(PlayerColor playerColor) {
        return switch (playerColor) {
            case RED -> new Color(220, 80, 80);
            case BLUE -> new Color(80, 140, 220);
            case GREEN -> new Color(80, 200, 120);
            case YELLOW -> new Color(230, 200, 80);
        };
    }

    private String formatTimeRemaining(java.time.OffsetDateTime endTime) {
        if (endTime == null)
            return "--:--";

        java.time.Duration remaining = java.time.Duration.between(
                java.time.OffsetDateTime.now(),
                endTime);

        if (remaining.isNegative())
            return "00:00";

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
}
