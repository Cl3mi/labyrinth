package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import labyrinth.client.ui.Styles.StyledButton;
import labyrinth.client.ui.Styles.StyledContextMenu;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.RankingEntry;

import javax.swing.*;
import javax.swing.Timer;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;
import java.util.List;

/**
 * Game Over UI showing winner and leaderboard with fantasy/medieval theme.
 * Implements responsive layout that adapts to different screen sizes.
 */
public class GameOverPanel extends JPanel {

    private static final Logger log = LoggerFactory.getLogger(GameOverPanel.class);

    private static final int MIN_WIDTH = 600;
    private static final int MIN_HEIGHT = 400;
    private static final int COMPACT_THRESHOLD = 800;
    private static final int MEDIUM_THRESHOLD = 1200;

    private JLabel winnerLabel;
    private JLabel titleLabel;
    private JTable leaderboardTable;
    private DefaultTableModel tableModel;
    private JScrollPane scrollPane;
    private JPanel achievementsPanel;
    private JPanel header;
    private StyledButton backToLobbyButton;

    private Image backgroundImage;
    private int animationFrame = 0;
    private Timer animationTimer;

    private final Map<String, List<String>> playerAchievements = new HashMap<>();
    private final Map<String, String> playerIdToName = new HashMap<>();

    public GameOverPanel(Runnable onBackToLobby) {
        loadBackgroundImage();

        setOpaque(false);
        setLayout(new BorderLayout());
        setMinimumSize(new Dimension(MIN_WIDTH, MIN_HEIGHT));

        header = createHeader();
        scrollPane = createLeaderboardScrollPane();
        achievementsPanel = createAchievementsPanel();
        backToLobbyButton = createBackButton(onBackToLobby);

        JPanel footer = new JPanel(new FlowLayout(FlowLayout.CENTER));
        footer.setOpaque(false);
        footer.add(backToLobbyButton);

        JPanel centerContainer = new JPanel(new BorderLayout());
        centerContainer.setOpaque(false);
        centerContainer.add(scrollPane, BorderLayout.CENTER);
        centerContainer.add(achievementsPanel, BorderLayout.SOUTH);

        add(header, BorderLayout.NORTH);
        add(centerContainer, BorderLayout.CENTER);
        add(footer, BorderLayout.SOUTH);

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                updateResponsiveLayout();
            }
        });

        updateResponsiveLayout();
        setupKeyboardNavigation(onBackToLobby);
    }

    private void setupKeyboardNavigation(Runnable onBackToLobby) {
        KeyListener navListener = new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                int keyCode = e.getKeyCode();

                if (keyCode == KeyEvent.VK_ESCAPE || keyCode == KeyEvent.VK_ENTER || keyCode == KeyEvent.VK_SPACE) {
                    e.consume();
                    if (onBackToLobby != null) {
                        onBackToLobby.run();
                    }
                    return;
                }

                Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();

                // Navigate between table and button
                if (keyCode == KeyEvent.VK_DOWN || keyCode == KeyEvent.VK_S) {
                    if (focused == leaderboardTable || isDescendant(scrollPane, focused)) {
                        // If at last row, go to button
                        if (leaderboardTable.getSelectedRow() >= leaderboardTable.getRowCount() - 1) {
                            e.consume();
                            backToLobbyButton.requestFocusInWindow();
                        }
                    }
                } else if (keyCode == KeyEvent.VK_UP || keyCode == KeyEvent.VK_W) {
                    if (focused == backToLobbyButton) {
                        e.consume();
                        leaderboardTable.requestFocusInWindow();
                        if (leaderboardTable.getRowCount() > 0) {
                            leaderboardTable.setRowSelectionInterval(leaderboardTable.getRowCount() - 1, leaderboardTable.getRowCount() - 1);
                        }
                    } else if (focused == leaderboardTable) {
                        if (leaderboardTable.getSelectedRow() <= 0) {
                            // Already at top, stay there
                        }
                    }
                } else if (keyCode == KeyEvent.VK_TAB) {
                    e.consume();
                    if (focused == backToLobbyButton) {
                        leaderboardTable.requestFocusInWindow();
                    } else {
                        backToLobbyButton.requestFocusInWindow();
                    }
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {}

            @Override
            public void keyReleased(KeyEvent e) {}
        };

        addKeyListener(navListener);
        backToLobbyButton.addKeyListener(navListener);
        leaderboardTable.addKeyListener(navListener);

        // Disable default Tab traversal so our listener handles it
        backToLobbyButton.setFocusTraversalKeysEnabled(false);
        leaderboardTable.setFocusTraversalKeysEnabled(false);

        setFocusable(true);
        setFocusTraversalKeysEnabled(false);
    }

    private boolean isDescendant(Component ancestor, Component descendant) {
        if (ancestor == null || descendant == null) return false;
        Component parent = descendant;
        while (parent != null) {
            if (parent == ancestor) return true;
            parent = parent.getParent();
        }
        return false;
    }

    @Override
    public void addNotify() {
        super.addNotify();
        SwingUtilities.invokeLater(() -> backToLobbyButton.requestFocusInWindow());
    }

    /**
     * Creates the header panel with title and winner announcement.
     */
    private JPanel createHeader() {
        JPanel headerPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                int margin = getResponsiveMargin();
                int bannerY = margin / 2;
                int bannerHeight = Math.max(60, getHeight() - margin);

                GradientPaint bannerGradient = new GradientPaint(
                        0, bannerY, GameTheme.Colors.SURFACE_PRIMARY,
                        0, bannerY + bannerHeight, GameTheme.Colors.SURFACE_SECONDARY
                );
                g2.setPaint(bannerGradient);
                g2.fillRoundRect(margin, bannerY, getWidth() - margin * 2, bannerHeight,
                        GameTheme.Spacing.RADIUS_LARGE, GameTheme.Spacing.RADIUS_LARGE);

                ThemeEffects.drawOrnateBorder(g2, margin, bannerY, getWidth() - margin * 2, bannerHeight);
                ThemeEffects.drawCornerOrnaments(g2, margin, bannerY, getWidth() - margin * 2, bannerHeight, 12);
            }
        };
        headerPanel.setOpaque(false);
        headerPanel.setLayout(new BoxLayout(headerPanel, BoxLayout.Y_AXIS));

        titleLabel = new JLabel("QUEST COMPLETE", SwingConstants.CENTER);
        titleLabel.setForeground(GameTheme.Colors.ACCENT_GOLD);
        titleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        JLabel trophyLabel = new JLabel("", SwingConstants.CENTER) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                int centerX = getWidth() / 2;
                int centerY = getHeight() / 2;
                int glowRadius = Math.min(30, getHeight() / 2);

                RadialGradientPaint glow = new RadialGradientPaint(
                        centerX, centerY, glowRadius,
                        new float[]{0.0f, 1.0f},
                        new Color[]{
                                ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 200),
                                ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 0)
                        }
                );
                g2.setPaint(glow);
                g2.fillOval(centerX - glowRadius, centerY - glowRadius, glowRadius * 2, glowRadius * 2);

                super.paintComponent(g);
            }
        };
        trophyLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        winnerLabel = new JLabel("", SwingConstants.CENTER);
        winnerLabel.setForeground(GameTheme.Colors.TEXT_PRIMARY);
        winnerLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        headerPanel.add(Box.createVerticalGlue());
        headerPanel.add(titleLabel);
        headerPanel.add(Box.createVerticalStrut(8));
        headerPanel.add(trophyLabel);
        headerPanel.add(Box.createVerticalStrut(8));
        headerPanel.add(winnerLabel);
        headerPanel.add(Box.createVerticalGlue());

        return headerPanel;
    }

    /**
     * Creates the leaderboard table and scroll pane.
     */
    private JScrollPane createLeaderboardScrollPane() {
        String[] columnNames = {"Rang", "Spieler", "Schätze", "Züge", "Verschiebungen", "Punkte"};
        tableModel = new DefaultTableModel(columnNames, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        leaderboardTable = new JTable(tableModel) {
            @Override
            public Component prepareRenderer(javax.swing.table.TableCellRenderer renderer, int row, int col) {
                Component comp = super.prepareRenderer(renderer, row, col);

                if (!isRowSelected(row)) {
                    if (row % 2 == 0) {
                        comp.setBackground(GameTheme.Colors.SURFACE_PRIMARY);
                    } else {
                        comp.setBackground(GameTheme.Colors.SURFACE_SECONDARY);
                    }

                    if (row < 3) {
                        Color highlightColor = ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 30);
                        comp.setBackground(ThemeEffects.blendColors(comp.getBackground(), highlightColor, 0.3f));
                    }

                    comp.setForeground(GameTheme.Colors.TEXT_PRIMARY);
                } else {
                    comp.setBackground(GameTheme.Colors.ACCENT_GOLD);
                    comp.setForeground(GameTheme.Colors.BACKGROUND_PRIMARY);
                }

                return comp;
            }
        };

        leaderboardTable.setShowGrid(true);
        leaderboardTable.setGridColor(ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_COPPER, 100));
        leaderboardTable.setSelectionBackground(GameTheme.Colors.ACCENT_GOLD);
        leaderboardTable.setSelectionForeground(GameTheme.Colors.BACKGROUND_PRIMARY);
        leaderboardTable.setIntercellSpacing(new Dimension(10, 5));

        leaderboardTable.getTableHeader().setBackground(GameTheme.Colors.BACKGROUND_SECONDARY);
        leaderboardTable.getTableHeader().setForeground(GameTheme.Colors.ACCENT_GOLD);
        leaderboardTable.getTableHeader().setReorderingAllowed(false);

        DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value,
                                                           boolean isSelected, boolean hasFocus, int row, int column) {
                Component comp = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                setHorizontalAlignment(SwingConstants.CENTER);
                return comp;
            }
        };
        centerRenderer.setOpaque(true);

        for (int i = 0; i < leaderboardTable.getColumnCount(); i++) {
            leaderboardTable.getColumnModel().getColumn(i).setCellRenderer(centerRenderer);
        }

        JScrollPane pane = new JScrollPane(leaderboardTable) {
            @Override
            public Dimension getPreferredSize() {
                int rowCount = leaderboardTable.getRowCount();
                int headerHeight = leaderboardTable.getTableHeader().getPreferredSize().height;
                int rowHeight = leaderboardTable.getRowHeight();
                int totalHeight = headerHeight + (rowCount * rowHeight) + 30;
                int maxHeight = (int) (GameOverPanel.this.getHeight() * 0.5);
                totalHeight = Math.min(totalHeight, Math.max(200, maxHeight));

                return new Dimension(getWidth(), totalHeight);
            }

            @Override
            protected void paintBorder(Graphics g) {
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                ThemeEffects.drawOrnateBorder(g2, 0, 0, getWidth(), getHeight());
            }
        };

        pane.setOpaque(false);
        pane.getViewport().setOpaque(false);
        pane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        pane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        return pane;
    }

    private JPanel createAchievementsPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setOpaque(false);
        return panel;
    }

    private StyledButton createBackButton(Runnable onBackToLobby) {
        StyledButton button = new StyledButton("Return to Tavern", StyledButton.Style.PRIMARY);
        button.addActionListener(e -> {
            if (onBackToLobby != null) {
                onBackToLobby.run();
            }
        });
        StyledTooltipManager.setTooltip(button, "Zurück zur Lobby", "Kehre zur Lobby zurück, um ein neues Spiel zu konfigurieren");
        StyledContextMenu.attachTo(button);
        return button;
    }

    /**
     * Updates the layout based on current panel dimensions.
     */
    private void updateResponsiveLayout() {
        int width = getWidth();
        int height = getHeight();

        if (width == 0 || height == 0) return;

        boolean isCompact = width < COMPACT_THRESHOLD;
        boolean isMedium = width < MEDIUM_THRESHOLD;

        int horizontalPadding = isCompact ? 20 : (isMedium ? 40 : 60);
        int verticalPadding = isCompact ? 20 : 40;
        setBorder(new EmptyBorder(verticalPadding, horizontalPadding, verticalPadding, horizontalPadding));

        Font titleFont = isCompact ? FontManager.getHeadingMedium() : FontManager.getHeadingLarge();
        Font winnerFont = isCompact ? FontManager.getBodyMedium(Font.BOLD) : FontManager.getBodyLarge(Font.BOLD);
        Font tableFont = isCompact ? FontManager.getBodySmall() : FontManager.getBodyLarge();
        Font headerFont = isCompact ? FontManager.getBodyMedium(Font.BOLD) : FontManager.getHeadingMedium();

        titleLabel.setFont(titleFont);
        winnerLabel.setFont(winnerFont);
        leaderboardTable.setFont(tableFont);
        leaderboardTable.getTableHeader().setFont(headerFont);

        int rowHeight = isCompact ? 40 : 55;
        int headerHeight = isCompact ? 35 : 50;
        leaderboardTable.setRowHeight(rowHeight);
        leaderboardTable.getTableHeader().setPreferredSize(new Dimension(0, headerHeight));

        int panelHeaderHeight = isCompact ? 120 : (isMedium ? 150 : 180);
        header.setPreferredSize(new Dimension(0, panelHeaderHeight));

        updateColumnWidths(isCompact, isMedium);

        int buttonWidth = isCompact ? 200 : 280;
        int buttonHeight = isCompact ? 45 : 60;
        backToLobbyButton.setPreferredSize(new Dimension(buttonWidth, buttonHeight));

        int scrollPadding = isCompact ? 5 : 10;
        scrollPane.setBorder(new EmptyBorder(scrollPadding, scrollPadding, scrollPadding, scrollPadding));

        int achievementPadding = isCompact ? 5 : 10;
        achievementsPanel.setBorder(new EmptyBorder(achievementPadding, 0, 0, 0));

        revalidate();
        repaint();
    }

    private void updateColumnWidths(boolean isCompact, boolean isMedium) {
        if (leaderboardTable.getColumnCount() < 6) return;

        if (isCompact) {
            leaderboardTable.getColumnModel().getColumn(0).setPreferredWidth(50);
            leaderboardTable.getColumnModel().getColumn(1).setPreferredWidth(120);
            leaderboardTable.getColumnModel().getColumn(2).setPreferredWidth(60);
            leaderboardTable.getColumnModel().getColumn(3).setPreferredWidth(50);
            leaderboardTable.getColumnModel().getColumn(4).setPreferredWidth(80);
            leaderboardTable.getColumnModel().getColumn(5).setPreferredWidth(60);
        } else if (isMedium) {
            leaderboardTable.getColumnModel().getColumn(0).setPreferredWidth(60);
            leaderboardTable.getColumnModel().getColumn(1).setPreferredWidth(150);
            leaderboardTable.getColumnModel().getColumn(2).setPreferredWidth(75);
            leaderboardTable.getColumnModel().getColumn(3).setPreferredWidth(65);
            leaderboardTable.getColumnModel().getColumn(4).setPreferredWidth(100);
            leaderboardTable.getColumnModel().getColumn(5).setPreferredWidth(75);
        } else {
            leaderboardTable.getColumnModel().getColumn(0).setPreferredWidth(70);
            leaderboardTable.getColumnModel().getColumn(1).setPreferredWidth(180);
            leaderboardTable.getColumnModel().getColumn(2).setPreferredWidth(90);
            leaderboardTable.getColumnModel().getColumn(3).setPreferredWidth(80);
            leaderboardTable.getColumnModel().getColumn(4).setPreferredWidth(120);
            leaderboardTable.getColumnModel().getColumn(5).setPreferredWidth(90);
        }
    }

    private int getResponsiveMargin() {
        int width = getWidth();
        if (width < COMPACT_THRESHOLD) return 20;
        if (width < MEDIUM_THRESHOLD) return 30;
        return 40;
    }

    /**
     * Updates the panel with game over data including winner and rankings.
     */
    public void updateGameOver(GameOverEventPayload payload) {
        if (animationTimer != null && animationTimer.isRunning()) {
            animationTimer.stop();
        }

        AudioPlayer.getInstance().playGameOverSequence();

        Map<String, String> payloadPlayerIdToName = new HashMap<>();
        if (payload.getRanking() != null) {
            for (RankingEntry entry : payload.getRanking()) {
                String playerName = getPlayerName(entry);
                payloadPlayerIdToName.put(entry.getPlayerId(), playerName);
            }
        }

        String winnerName = "Unknown";
        if (payload.getRanking() != null && payload.getRanking().length > 0) {
            for (RankingEntry entry : payload.getRanking()) {
                if (entry.getPlayerId().equals(payload.getWinnerId())) {
                    winnerName = getPlayerName(entry);
                    break;
                }
            }
        }

        final String winnerNameFinal = winnerName;
        animationFrame = 0;
        animationTimer = new Timer(150, e -> {
            animationFrame++;
            if (animationFrame % 2 == 0) {
                winnerLabel.setText(">> " + winnerNameFinal + " gewinnt! <<");
                winnerLabel.setForeground(GameTheme.Colors.ACCENT_GOLD);
            } else {
                winnerLabel.setText(">> " + winnerNameFinal + " gewinnt! <<");
                winnerLabel.setForeground(ThemeEffects.blendColors(
                        GameTheme.Colors.ACCENT_GOLD,
                        GameTheme.Colors.TEXT_PRIMARY,
                        0.3f
                ));
            }
            if (animationFrame > 8) {
                ((Timer) e.getSource()).stop();
                winnerLabel.setForeground(GameTheme.Colors.ACCENT_GOLD);
            }
        });
        animationTimer.start();

        tableModel.setRowCount(0);
        if (payload.getRanking() != null) {
            for (RankingEntry entry : payload.getRanking()) {
                String rankIcon = switch (entry.getRank()) {
                    case 1 -> " 1.";
                    case 2 -> " 2.";
                    case 3 -> " 3.";
                    default -> entry.getRank() + ".";
                };

                int treasuresCollected = entry.getStats() != null && entry.getStats().getTreasuresCollected() != null ?
                        entry.getStats().getTreasuresCollected() : 0;
                int stepsTaken = entry.getStats() != null && entry.getStats().getStepsTaken() != null ?
                        entry.getStats().getStepsTaken() : 0;
                int tilesPushed = entry.getStats() != null && entry.getStats().getTilesPushed() != null ?
                        entry.getStats().getTilesPushed() : 0;

                tableModel.addRow(new Object[]{
                        rankIcon,
                        getPlayerName(entry),
                        treasuresCollected,
                        stepsTaken,
                        tilesPushed,
                        entry.getScore()
                });
            }
        }

        updateAchievementsDisplayWithNames(payloadPlayerIdToName);
        updateResponsiveLayout();

        scrollPane.revalidate();
        scrollPane.repaint();
    }

    /**
     * Updates achievements display with proper player names from ranking data.
     */
    private void updateAchievementsDisplayWithNames(Map<String, String> playerNames) {
        achievementsPanel.removeAll();

        if (playerAchievements.isEmpty()) {
            achievementsPanel.setVisible(false);
            return;
        }

        achievementsPanel.setVisible(true);

        JLabel achievementTitle = new JLabel("Errungenschaften", SwingConstants.CENTER);
        achievementTitle.setFont(FontManager.getHeadingSmall());
        achievementTitle.setForeground(GameTheme.Colors.ACCENT_GOLD);
        achievementTitle.setAlignmentX(Component.CENTER_ALIGNMENT);
        achievementTitle.setBorder(new EmptyBorder(5, 0, 10, 0));
        achievementsPanel.add(achievementTitle);

        for (Map.Entry<String, List<String>> entry : playerAchievements.entrySet()) {
            String playerId = entry.getKey();
            List<String> achievements = entry.getValue();

            if (achievements.isEmpty()) continue;

            String playerName = playerNames.getOrDefault(playerId, playerId);
            JPanel playerAchievementCard = createPlayerAchievementCard(playerName, achievements);
            achievementsPanel.add(playerAchievementCard);
            achievementsPanel.add(Box.createVerticalStrut(5));
        }

        achievementsPanel.revalidate();
        achievementsPanel.repaint();
    }

    /**
     * Gets player name from entry, checking local map first, then additional properties.
     */
    private String getPlayerName(RankingEntry entry) {
        String playerId = entry.getPlayerId();

        if (playerIdToName.containsKey(playerId)) {
            return playerIdToName.get(playerId);
        }

        if (entry.getAdditionalProperties() != null && entry.getAdditionalProperties().containsKey("playerName")) {
            Object value = entry.getAdditionalProperties().get("playerName");
            if (value instanceof String) {
                return (String) value;
            }
        }

        return playerId;
    }

    private void loadBackgroundImage() {
        try {
            var url = getClass().getClassLoader().getResource("images/background.png");
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
            }
        } catch (Exception e) {
            log.error("Could not load background image: {}", e.getMessage());
        }
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);

        if (backgroundImage != null) {
            g2.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.BACKGROUND_PRIMARY, 100));
            g2.fillRect(0, 0, getWidth(), getHeight());
        } else {
            GradientPaint gradient = new GradientPaint(
                    0, 0, GameTheme.Colors.BACKGROUND_PRIMARY,
                    0, getHeight(), GameTheme.Colors.BACKGROUND_SECONDARY
            );
            g2.setPaint(gradient);
            g2.fillRect(0, 0, getWidth(), getHeight());
        }

        float vignetteRadius = Math.max(getWidth(), getHeight()) * 0.8f;
        RadialGradientPaint vignette = new RadialGradientPaint(
                getWidth() / 2f, getHeight() / 2f,
                vignetteRadius,
                new float[]{0.0f, 1.0f},
                new Color[]{
                        ThemeEffects.withAlpha(GameTheme.Colors.BACKGROUND_PRIMARY, 0),
                        ThemeEffects.withAlpha(GameTheme.Colors.BACKGROUND_PRIMARY, 150)
                }
        );
        g2.setPaint(vignette);
        g2.fillRect(0, 0, getWidth(), getHeight());
    }

    /**
     * Cleans up resources when panel is disposed.
     */
    public void cleanup() {
        if (animationTimer != null && animationTimer.isRunning()) {
            animationTimer.stop();
        }
        playerAchievements.clear();
        achievementsPanel.removeAll();
    }

    /**
     * Adds an achievement for a player. Call when ACHIEVEMENT_UNLOCKED events are received.
     */
    public void addAchievement(String playerId, String achievementName) {
        playerAchievements.computeIfAbsent(playerId, k -> new ArrayList<>()).add(achievementName);
    }

    private JPanel createPlayerAchievementCard(String playerName, List<String> achievements) {
        boolean isCompact = getWidth() < COMPACT_THRESHOLD;

        JPanel card = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.SURFACE_PRIMARY, 180));
                g2.fillRoundRect(0, 0, getWidth(), getHeight(), 10, 10);

                g2.setColor(GameTheme.Colors.ACCENT_GOLD);
                g2.setStroke(new BasicStroke(1.5f));
                g2.drawRoundRect(1, 1, getWidth() - 2, getHeight() - 2, 10, 10);

                g2.dispose();
                super.paintComponent(g);
            }
        };

        int padding = isCompact ? 6 : 10;
        card.setLayout(new FlowLayout(FlowLayout.LEFT, padding, padding / 2));
        card.setOpaque(false);

        int cardHeight = isCompact ? 35 : 45;
        card.setMaximumSize(new Dimension(Integer.MAX_VALUE, cardHeight));

        Font nameFont = isCompact ? FontManager.getBodySmall(Font.BOLD) : FontManager.getBodyMedium(Font.BOLD);
        JLabel nameLabel = new JLabel(playerName + ": ");
        nameLabel.setFont(nameFont);
        nameLabel.setForeground(GameTheme.Colors.TEXT_PRIMARY);
        card.add(nameLabel);

        for (String achievement : achievements) {
            JLabel badge = createAchievementBadge(achievement, isCompact);
            card.add(badge);
        }

        return card;
    }

    private JLabel createAchievementBadge(String achievementName, boolean isCompact) {
        String icon = getAchievementIcon(achievementName);
        String displayName = formatAchievementName(achievementName);

        JLabel badge = new JLabel(icon + " " + displayName);
        Font badgeFont = isCompact ? FontManager.getBodySmall() : FontManager.getBodySmall(Font.BOLD);
        badge.setFont(badgeFont);
        badge.setForeground(GameTheme.Colors.ACCENT_GOLD);
        badge.setOpaque(true);
        badge.setBackground(ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 30));

        int vPad = isCompact ? 2 : 3;
        int hPad = isCompact ? 5 : 8;
        badge.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(GameTheme.Colors.ACCENT_GOLD, 1),
                BorderFactory.createEmptyBorder(vPad, hPad, vPad, hPad)
        ));

        return badge;
    }

    private String getAchievementIcon(String achievementName) {
        return switch (achievementName.toUpperCase()) {
            case "RUNNER" -> "🏃";
            case "PUSHER" -> "💪";
            default -> "🏅";
        };
    }

    private String formatAchievementName(String achievementName) {
        return switch (achievementName.toUpperCase()) {
            case "RUNNER" -> "Runner";
            case "PUSHER" -> "Pusher";
            default -> achievementName;
        };
    }

    /**
     * Sets the callback for starting a new round.
     */
    public void setOnStartNewRound(Runnable callback) {
    }

    /**
     * Sets the player ID to name mapping. Call before updateGameOver for correct name display.
     */
    public void setPlayerNames(Map<String, String> idToNameMap) {
        this.playerIdToName.clear();
        if (idToNameMap != null) {
            this.playerIdToName.putAll(idToNameMap);
        }
    }
}