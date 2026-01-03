package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.contracts.models.GameOverEventPayload;
import labyrinth.contracts.models.RankingEntry;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import java.awt.*;

/**
 * Game Over UI showing winner and leaderboard.
 */
public class GameOverPanel extends JPanel {

    private final JLabel winnerLabel;
    private final JTable leaderboardTable;
    private final DefaultTableModel tableModel;
    private final JButton backToLobbyButton;

    private Image backgroundImage;
    private int animationFrame = 0;
    private Timer animationTimer;

    public GameOverPanel(Runnable onBackToLobby) {
        loadBackgroundImage();

        setOpaque(false);
        setLayout(new BorderLayout(20, 20));
        setBorder(new EmptyBorder(40, 40, 40, 40));

        // ===== Header =====
        JPanel header = new JPanel();
        header.setOpaque(false);
        header.setLayout(new BoxLayout(header, BoxLayout.Y_AXIS));

        JLabel titleLabel = new JLabel("SPIEL BEENDET", SwingConstants.CENTER);
        titleLabel.setFont(new Font("Arial", Font.BOLD, 48));
        titleLabel.setForeground(new Color(255, 215, 0));
        titleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        winnerLabel = new JLabel("", SwingConstants.CENTER);
        winnerLabel.setFont(new Font("Arial", Font.BOLD, 32));
        winnerLabel.setForeground(new Color(255, 255, 100));
        winnerLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        header.add(Box.createVerticalStrut(20));
        header.add(titleLabel);
        header.add(Box.createVerticalStrut(30));
        header.add(winnerLabel);
        header.add(Box.createVerticalStrut(20));

        // ===== Leaderboard =====
        String[] columnNames = {"Rang", "Spieler", "Schätze gefunden", "Punkte"};
        tableModel = new DefaultTableModel(columnNames, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        leaderboardTable = new JTable(tableModel);
        leaderboardTable.setFont(new Font("Arial", Font.PLAIN, 18));
        leaderboardTable.setRowHeight(40);
        leaderboardTable.setBackground(new Color(30, 30, 50, 200));
        leaderboardTable.setForeground(new Color(220, 220, 255));
        leaderboardTable.setSelectionBackground(new Color(100, 100, 150));
        leaderboardTable.setGridColor(new Color(100, 120, 150));
        leaderboardTable.setShowGrid(true);

        // Header styling
        leaderboardTable.getTableHeader().setFont(new Font("Arial", Font.BOLD, 20));
        leaderboardTable.getTableHeader().setBackground(new Color(50, 50, 80));
        leaderboardTable.getTableHeader().setForeground(new Color(255, 215, 0));
        leaderboardTable.getTableHeader().setReorderingAllowed(false);

        // Center align all cells
        DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
        centerRenderer.setHorizontalAlignment(SwingConstants.CENTER);
        for (int i = 0; i < leaderboardTable.getColumnCount(); i++) {
            leaderboardTable.getColumnModel().getColumn(i).setCellRenderer(centerRenderer);
        }

        // Column widths
        leaderboardTable.getColumnModel().getColumn(0).setPreferredWidth(80);   // Rang
        leaderboardTable.getColumnModel().getColumn(1).setPreferredWidth(200);  // Spieler
        leaderboardTable.getColumnModel().getColumn(2).setPreferredWidth(180);  // Schätze
        leaderboardTable.getColumnModel().getColumn(3).setPreferredWidth(120);  // Punkte

        JScrollPane scrollPane = new JScrollPane(leaderboardTable);
        scrollPane.setOpaque(false);
        scrollPane.getViewport().setOpaque(false);
        scrollPane.setBorder(BorderFactory.createLineBorder(new Color(100, 120, 150), 2));

        // ===== Footer =====
        JPanel footer = new JPanel();
        footer.setOpaque(false);
        footer.setLayout(new FlowLayout(FlowLayout.CENTER));

        backToLobbyButton = new JButton("Zurück zur Lobby");
        backToLobbyButton.setFont(new Font("Arial", Font.BOLD, 18));
        backToLobbyButton.setPreferredSize(new Dimension(250, 50));
        backToLobbyButton.setBackground(new Color(70, 130, 180));
        backToLobbyButton.setForeground(Color.WHITE);
        backToLobbyButton.setFocusPainted(false);
        backToLobbyButton.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new Color(100, 150, 200), 2),
                new EmptyBorder(10, 20, 10, 20)
        ));
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
        String winnerName = "Unbekannt";
        if (payload.getRanking() != null && payload.getRanking().length > 0) {
            for (RankingEntry entry : payload.getRanking()) {
                if (entry.getPlayerId().equals(payload.getWinnerId())) {
                    winnerName = entry.getPlayerName();
                    break;
                }
            }
        }

        // Animate winner announcement
        final String winnerNameFinal = winnerName;
        animationFrame = 0;
        animationTimer = new Timer(100, e -> {
            animationFrame++;
            if (animationFrame % 2 == 0) {
                winnerLabel.setText(" " + winnerNameFinal + " gewinnt! ");
                winnerLabel.setForeground(new Color(255, 215, 0));
            } else {
                winnerLabel.setText(" " + winnerNameFinal + " gewinnt! ");
                winnerLabel.setForeground(new Color(255, 255, 100));
            }
            if (animationFrame > 10) {
                ((Timer) e.getSource()).stop();
                winnerLabel.setForeground(new Color(255, 215, 0));
            }
        });
        animationTimer.start();

        // Update leaderboard
        tableModel.setRowCount(0);
        if (payload.getRanking() != null) {
            for (RankingEntry entry : payload.getRanking()) {
                // Show medal emojis for top 3 ranks, numbers for others
                String rankIcon = switch (entry.getRank()) {
                    case 1 -> "🥇 1.";
                    case 2 -> "🥈 2.";
                    case 3 -> "🥉 3.";
                    default -> entry.getRank() + ".";
                };

                int treasuresCollected = entry.getStats() != null ?
                        entry.getStats().getTreasuresCollected() : 0;

                tableModel.addRow(new Object[]{
                        rankIcon,
                        entry.getPlayerName(),
                        treasuresCollected,
                        entry.getScore()
                });
            }
        }

    }

    private void loadBackgroundImage() {
        try {
            var url = getClass().getClassLoader().getResource("images/background.jpg");
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
        if (backgroundImage != null) {
            g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
        } else {
            // Fallback gradient background
            Graphics2D g2d = (Graphics2D) g;
            GradientPaint gradient = new GradientPaint(
                    0, 0, new Color(20, 20, 40),
                    0, getHeight(), new Color(60, 60, 100)
            );
            g2d.setPaint(gradient);
            g2d.fillRect(0, 0, getWidth(), getHeight());
        }
    }

    public void cleanup() {
        if (animationTimer != null && animationTimer.isRunning()) {
            animationTimer.stop();
        }
    }
}
