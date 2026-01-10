package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.RankingEntry;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import java.awt.*;

/**
 * Game Over UI showing winner and leaderboard with fantasy/medieval theme.
 */
public class GameOverPanel extends JPanel {

    private final JLabel winnerLabel;
    private final JTable leaderboardTable;
    private final DefaultTableModel tableModel;
    private final JButton backToLobbyButton;
    private final JScrollPane scrollPane;

    private Image backgroundImage;
    private int animationFrame = 0;
    private Timer animationTimer;

    public GameOverPanel(Runnable onBackToLobby) {
        loadBackgroundImage();

        setOpaque(false);
        setLayout(new BorderLayout(20, 20));
        setBorder(new EmptyBorder(40, 60, 40, 60));

        // ===== Header =====
        JPanel header = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Draw ornate title banner
                int bannerY = 20;
                int bannerHeight = 80;

                // Parchment banner background
                GradientPaint bannerGradient = new GradientPaint(
                    0, bannerY, GameTheme.Colors.SURFACE_PRIMARY,
                    0, bannerY + bannerHeight, GameTheme.Colors.SURFACE_SECONDARY
                );
                g2.setPaint(bannerGradient);
                g2.fillRoundRect(40, bannerY, getWidth() - 80, bannerHeight,
                    GameTheme.Spacing.RADIUS_LARGE, GameTheme.Spacing.RADIUS_LARGE);

                // Ornate border
                ThemeEffects.drawOrnateBorder(g2, 40, bannerY, getWidth() - 80, bannerHeight);

                // Corner ornaments
                ThemeEffects.drawCornerOrnaments(g2, 40, bannerY, getWidth() - 80, bannerHeight, 12);
            }
        };
        header.setOpaque(false);
        header.setLayout(new BoxLayout(header, BoxLayout.Y_AXIS));
        header.setPreferredSize(new Dimension(0, 180));

        JLabel titleLabel = new JLabel("QUEST COMPLETE", SwingConstants.CENTER);
        titleLabel.setFont(FontManager.getDisplayFont(48, Font.BOLD));
        titleLabel.setForeground(GameTheme.Colors.ACCENT_GOLD);
        titleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        titleLabel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 60));

        // Trophy icon with bright golden glow background for visibility
        JLabel trophyLabel = new JLabel("🏆", SwingConstants.CENTER) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Golden glow behind trophy for visibility
                int centerX = getWidth() / 2;
                int centerY = getHeight() / 2;
                RadialGradientPaint glow = new RadialGradientPaint(
                    centerX, centerY, 30f,
                    new float[]{0.0f, 1.0f},
                    new Color[]{
                        ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 200),
                        ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 0)
                    }
                );
                g2.setPaint(glow);
                g2.fillOval(centerX - 30, centerY - 30, 60, 60);

                super.paintComponent(g);
            }
        };
        trophyLabel.setFont(new Font("Serif", Font.PLAIN, 48));
        trophyLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        trophyLabel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 60));

        winnerLabel = new JLabel("", SwingConstants.CENTER);
        winnerLabel.setFont(FontManager.getMediumDisplay()); // Increased size
        winnerLabel.setForeground(GameTheme.Colors.TEXT_PRIMARY);
        winnerLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        winnerLabel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 40));

        header.add(Box.createVerticalStrut(38));
        header.add(titleLabel);
        header.add(Box.createVerticalStrut(8));
        header.add(trophyLabel);
        header.add(Box.createVerticalStrut(12));
        header.add(winnerLabel);
        header.add(Box.createVerticalStrut(20));

        // ===== Leaderboard =====
        String[] columnNames = {"Rank", "Player", "Treasures", "Score"};
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

                // Alternating row colors for better readability
                if (!isRowSelected(row)) {
                    if (row % 2 == 0) {
                        comp.setBackground(GameTheme.Colors.SURFACE_PRIMARY);
                    } else {
                        comp.setBackground(GameTheme.Colors.SURFACE_SECONDARY);
                    }

                    // Highlight top 3 with subtle gold tint
                    if (row < 3) {
                        Color highlightColor = ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_GOLD, 30);
                        comp.setBackground(ThemeEffects.blendColors(comp.getBackground(), highlightColor, 0.3f));
                    }

                    comp.setForeground(GameTheme.Colors.TEXT_PRIMARY);
                } else {
                    comp.setBackground(GameTheme.Colors.ACCENT_GOLD);
                    comp.setForeground(GameTheme.Colors.BACKGROUND_PRIMARY);
                }

                // Increase font size for better readability
                comp.setFont(FontManager.getLargeUI());

                return comp;
            }
        };

        leaderboardTable.setFont(FontManager.getLargeUI()); // Larger font
        leaderboardTable.setRowHeight(55); // Taller rows for larger font
        leaderboardTable.setShowGrid(true);
        leaderboardTable.setGridColor(ThemeEffects.withAlpha(GameTheme.Colors.ACCENT_COPPER, 100));
        leaderboardTable.setSelectionBackground(GameTheme.Colors.ACCENT_GOLD);
        leaderboardTable.setSelectionForeground(GameTheme.Colors.BACKGROUND_PRIMARY);
        leaderboardTable.setIntercellSpacing(new Dimension(10, 5));

        // Header styling
        leaderboardTable.getTableHeader().setFont(FontManager.getMediumDisplay()); // Larger header font
        leaderboardTable.getTableHeader().setBackground(GameTheme.Colors.BACKGROUND_SECONDARY);
        leaderboardTable.getTableHeader().setForeground(GameTheme.Colors.ACCENT_GOLD);
        leaderboardTable.getTableHeader().setReorderingAllowed(false);
        leaderboardTable.getTableHeader().setPreferredSize(new Dimension(0, 50));

        // Center align all cells with proper background handling
        DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value,
                    boolean isSelected, boolean hasFocus, int row, int column) {
                Component comp = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

                // Don't override the prepareRenderer background/foreground
                // Just set alignment and font
                setHorizontalAlignment(SwingConstants.CENTER);
                setFont(FontManager.getLargeUI());

                return comp;
            }
        };
        centerRenderer.setOpaque(true);
        for (int i = 0; i < leaderboardTable.getColumnCount(); i++) {
            leaderboardTable.getColumnModel().getColumn(i).setCellRenderer(centerRenderer);
        }

        // Column widths
        leaderboardTable.getColumnModel().getColumn(0).setPreferredWidth(100);  // Rank
        leaderboardTable.getColumnModel().getColumn(1).setPreferredWidth(220);  // Player
        leaderboardTable.getColumnModel().getColumn(2).setPreferredWidth(140);  // Treasures
        leaderboardTable.getColumnModel().getColumn(3).setPreferredWidth(120);  // Score

        // Custom scroll pane with parchment styling
        scrollPane = new JScrollPane(leaderboardTable) {
            @Override
            public Dimension getPreferredSize() {
                // Calculate height based on table content (header + rows + insets)
                int rowCount = leaderboardTable.getRowCount();
                int headerHeight = leaderboardTable.getTableHeader().getPreferredSize().height;
                int rowHeight = leaderboardTable.getRowHeight();
                int totalHeight = headerHeight + (rowCount * rowHeight) + 30; // +30 for padding

                // Cap at reasonable max height
                totalHeight = Math.min(totalHeight, 400);

                return new Dimension(getWidth(), totalHeight);
            }

            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
            }

            @Override
            protected void paintBorder(Graphics g) {
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Draw ornate frame around leaderboard - uses actual component height
                ThemeEffects.drawOrnateBorder(g2, 0, 0, getWidth(), getHeight());
            }
        };
        scrollPane.setOpaque(false);
        scrollPane.getViewport().setOpaque(false);
        scrollPane.setBorder(new EmptyBorder(10, 10, 10, 10));
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        // ===== Footer =====
        JPanel footer = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
            }
        };
        footer.setOpaque(false);
        footer.setLayout(new FlowLayout(FlowLayout.CENTER, 20, 20));

        backToLobbyButton = new JButton("Return to Tavern") {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Wood texture background
                GradientPaint woodGradient = new GradientPaint(
                    0, 0, GameTheme.Colors.SURFACE_PRIMARY,
                    0, getHeight(), GameTheme.Colors.SURFACE_SECONDARY
                );
                g2.setPaint(woodGradient);
                g2.fillRoundRect(0, 0, getWidth(), getHeight(),
                    GameTheme.Spacing.RADIUS_MEDIUM, GameTheme.Spacing.RADIUS_MEDIUM);

                // Embossed button effect
                ThemeEffects.drawEmbossedButton(g2, 0, 0, getWidth(), getHeight(),
                    getModel().isPressed());

                // Copper border
                g2.setColor(GameTheme.Colors.ACCENT_COPPER);
                g2.setStroke(new BasicStroke(2f));
                g2.drawRoundRect(1, 1, getWidth() - 3, getHeight() - 3,
                    GameTheme.Spacing.RADIUS_MEDIUM, GameTheme.Spacing.RADIUS_MEDIUM);

                // Text with shadow
                g2.setFont(FontManager.getLargeUI());
                FontMetrics fm = g2.getFontMetrics();
                String text = getText();
                int textX = (getWidth() - fm.stringWidth(text)) / 2;
                int textY = (getHeight() + fm.getAscent() - fm.getDescent()) / 2;

                // Shadow
                g2.setColor(ThemeEffects.withAlpha(Color.BLACK, 100));
                g2.drawString(text, textX + 2, textY + 2);

                // Main text
                g2.setColor(GameTheme.Colors.TEXT_PRIMARY);
                g2.drawString(text, textX, textY);
            }
        };

        backToLobbyButton.setFont(FontManager.getLargeUI());
        backToLobbyButton.setPreferredSize(new Dimension(280, 60));
        backToLobbyButton.setContentAreaFilled(false);
        backToLobbyButton.setFocusPainted(false);
        backToLobbyButton.setBorderPainted(false);
        backToLobbyButton.setCursor(new Cursor(Cursor.HAND_CURSOR));
        backToLobbyButton.addActionListener(e -> {
            if (onBackToLobby != null) {
                onBackToLobby.run();
            }
        });

        footer.add(backToLobbyButton);

        // ===== Add to panel =====
        add(header, BorderLayout.NORTH);
        add(scrollPane, BorderLayout.CENTER);
        add(footer, BorderLayout.SOUTH);
    }

    public void updateGameOver(GameOverEventPayload payload) {
        // Stop any existing animation
        if (animationTimer != null && animationTimer.isRunning()) {
            animationTimer.stop();
        }

        AudioPlayer.getInstance().playGameOverSequence();

        // Find winner name
        String winnerName = "Unknown";
        if (payload.getRanking() != null && payload.getRanking().length > 0) {
            for (RankingEntry entry : payload.getRanking()) {
                if (entry.getPlayerId().equals(payload.getWinnerId())) {
                    // Get playerName from additionalProperties (not in standard Contracts)
                    winnerName = getPlayerName(entry);
                    break;
                }
            }
        }

        // Animate winner announcement with subtle pulsing
        final String winnerNameFinal = winnerName;
        animationFrame = 0;
        animationTimer = new Timer(150, e -> {
            animationFrame++;
            if (animationFrame % 2 == 0) {
                winnerLabel.setText("⚔ " + winnerNameFinal + " gewinnt! ⚔");
                winnerLabel.setForeground(GameTheme.Colors.ACCENT_GOLD);
            } else {
                winnerLabel.setText("⚔ " + winnerNameFinal + " gewinnt! ⚔");
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

        // Update leaderboard
        tableModel.setRowCount(0);
        if (payload.getRanking() != null) {
            for (RankingEntry entry : payload.getRanking()) {
                // Show medal emojis for top 3 ranks, numbers for others
                String rankIcon = switch (entry.getRank()) {
                    case 1 -> " 1.";
                    case 2 -> " 2.";
                    case 3 -> " 3.";
                    default -> entry.getRank() + ".";
                };

                int treasuresCollected = entry.getStats() != null ?
                        entry.getStats().getTreasuresCollected() : 0;

                tableModel.addRow(new Object[]{
                        rankIcon,
                        getPlayerName(entry),
                        treasuresCollected,
                        entry.getScore()
                });
            }
        }

        // Resize scroll pane to fit content and redraw border
        scrollPane.revalidate();
        scrollPane.repaint();
    }

    /**
     * Extract playerName from additionalProperties (not in standard Contracts)
     */
    private String getPlayerName(RankingEntry entry) {
        if (entry.getAdditionalProperties() != null && entry.getAdditionalProperties().containsKey("playerName")) {
            Object value = entry.getAdditionalProperties().get("playerName");
            if (value instanceof String) {
                return (String) value;
            }
        }
        return entry.getPlayerId(); // Fallback to player ID if name not available
    }

    private void loadBackgroundImage() {
        try {
            var url = getClass().getClassLoader().getResource("images/background.png");
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
            }
        } catch (Exception e) {
            System.err.println("Could not load background image: " + e.getMessage());
        }
    }
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        if (backgroundImage != null) {
            // Darken background image slightly for better contrast
            g2.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.BACKGROUND_PRIMARY, 100));
            g2.fillRect(0, 0, getWidth(), getHeight());
        } else {
            // Fallback: Dark earthy gradient background
            GradientPaint gradient = new GradientPaint(
                0, 0, GameTheme.Colors.BACKGROUND_PRIMARY,
                0, getHeight(), GameTheme.Colors.BACKGROUND_SECONDARY
            );
            g2.setPaint(gradient);
            g2.fillRect(0, 0, getWidth(), getHeight());
        }

        // Subtle vignette effect
        RadialGradientPaint vignette = new RadialGradientPaint(
            getWidth() / 2f, getHeight() / 2f,
            Math.max(getWidth(), getHeight()) * 0.8f,
            new float[]{0.0f, 1.0f},
            new Color[]{
                ThemeEffects.withAlpha(GameTheme.Colors.BACKGROUND_PRIMARY, 0),
                ThemeEffects.withAlpha(GameTheme.Colors.BACKGROUND_PRIMARY, 150)
            }
        );
        g2.setPaint(vignette);
        g2.fillRect(0, 0, getWidth(), getHeight());
    }

    public void cleanup() {
        if (animationTimer != null && animationTimer.isRunning()) {
            animationTimer.stop();
        }
    }
}
