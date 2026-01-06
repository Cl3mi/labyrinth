package labyrinth.client.ui;

import labyrinth.managementclient.api.ServersApi;
import labyrinth.managementclient.model.GameServer;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

public class ServerBrowserPanel extends JPanel {

    private Runnable onBackToMenu;
    private Image backgroundImage;

    private final ServersApi serversApi;

    private final DefaultListModel<GameServer> serverListModel = new DefaultListModel<>();
    private final JList<GameServer> serverList;
    private final JLabel statusLabel;

    private static final Color PRIMARY_GOLD = new Color(218, 165, 32);
    private static final Color PRIMARY_GOLD_LIGHT = new Color(255, 215, 0);
    private static final Color TEXT_LIGHT = new Color(255, 248, 230);
    private static final Color TEXT_MUTED = new Color(180, 170, 155);
    private static final Color SHADOW_COLOR = new Color(0, 0, 0, 60);
    private static final Color CARD_BG = new Color(50, 45, 40, 200);
    private static final Color CARD_BORDER = new Color(80, 70, 55, 120);

    private final Font titleFont = new Font("Serif", Font.BOLD, 28);
    private final Font labelFont = new Font("Serif", Font.PLAIN, 14);
    private final Font nameFont = new Font("SansSerif", Font.BOLD, 15);
    private final Font uriFont = new Font("SansSerif", Font.PLAIN, 12);

    private ScheduledExecutorService poller;
    private final DateTimeFormatter timeFmt = DateTimeFormatter.ofPattern("HH:mm:ss");

    private Consumer<GameServer> onServerSelected;

    public ServerBrowserPanel(ServersApi serversApi) {
        this.serversApi = serversApi;

        loadBackgroundImage();

        setOpaque(false);
        setLayout(new BorderLayout());
        setBorder(new EmptyBorder(12, 12, 12, 12));

        JPanel header = new JPanel(new BorderLayout(20, 0));
        header.setOpaque(false);

        JButton backBtn = new JButton("← Zurück");
        backBtn.setFocusPainted(false);
        backBtn.addActionListener(e -> {
            onLeaveServerBrowser();
            if (onBackToMenu != null) onBackToMenu.run();
        });
        JPanel leftPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        leftPanel.setOpaque(false);
        leftPanel.add(backBtn);
        header.add(leftPanel, BorderLayout.WEST);

        JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setOpaque(false);

        JLabel headerTitle = new JLabel("Verfügbare Server");
        headerTitle.setFont(titleFont);
        headerTitle.setForeground(PRIMARY_GOLD_LIGHT);
        headerTitle.setAlignmentX(Component.CENTER_ALIGNMENT);

        statusLabel = new JLabel("Lade Server...");
        statusLabel.setFont(labelFont);
        statusLabel.setForeground(new Color(200, 160, 60));
        statusLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        centerPanel.add(Box.createVerticalStrut(5));
        centerPanel.add(headerTitle);
        centerPanel.add(Box.createVerticalStrut(6));
        centerPanel.add(statusLabel);

        header.add(centerPanel, BorderLayout.CENTER);

        JPanel rightPanel = new JPanel();
        rightPanel.setOpaque(false);
        rightPanel.setPreferredSize(new Dimension(140, 40));
        header.add(rightPanel, BorderLayout.EAST);

        add(header, BorderLayout.NORTH);

        serverList = new JList<>(serverListModel);
        serverList.setCellRenderer(new ServerListRenderer());
        serverList.setOpaque(false);
        serverList.setBackground(new Color(0, 0, 0, 0));
        serverList.setFixedCellHeight(90);
        serverList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        serverList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                int idx = serverList.locationToIndex(e.getPoint());
                if (idx >= 0) {
                    GameServer gs = serverListModel.getElementAt(idx);
                    Object[] options = {"Ja", "Nein"};
                    int res = JOptionPane.showOptionDialog(
                            ServerBrowserPanel.this,
                            "Wollen Sie sich mit dem Server \"" + gs.getName() + "\" verbinden?",
                            "Mit Server verbinden",
                            JOptionPane.YES_NO_OPTION,
                            JOptionPane.QUESTION_MESSAGE,
                            null,
                            options,
                            options[1]);

                    if (res == 0) {
                        if (onServerSelected != null) {
                            onServerSelected.accept(gs);
                        }
                    }
                }
            }
        });

        JPanel listCard = new JPanel(new BorderLayout(0, 15)) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                g2.setColor(CARD_BG);
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 15, 15));

                g2.setColor(CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 2, getHeight() - 2, 15, 15));

                g2.setColor(new Color(255, 255, 255, 10));
                g2.fill(new RoundRectangle2D.Float(2, 2, getWidth() - 4, 40, 13, 13));

                g2.dispose();
                super.paintComponent(g);
            }
        };
        listCard.setOpaque(false);
        listCard.setBorder(new EmptyBorder(20, 25, 20, 25));

        JLabel listTitle = new JLabel("📡 Verfügbare Server");
        listTitle.setFont(new Font("Serif", Font.BOLD, 18));
        listTitle.setForeground(PRIMARY_GOLD_LIGHT);
        listCard.add(listTitle, BorderLayout.NORTH);

        JScrollPane sp = new JScrollPane(serverList);
        sp.setOpaque(false);
        sp.getViewport().setOpaque(false);
        sp.setBorder(BorderFactory.createEmptyBorder());
        sp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        listCard.add(sp, BorderLayout.CENTER);

        add(listCard, BorderLayout.CENTER);

        JPanel footer = new JPanel(new FlowLayout(FlowLayout.LEFT));
        footer.setOpaque(false);
        footer.setBorder(new EmptyBorder(8, 8, 8, 8));
        JLabel hint = new JLabel("Nur Server im Status 'LOBBY' werden angezeigt");
        hint.setForeground(TEXT_MUTED);
        footer.add(hint);
        add(footer, BorderLayout.SOUTH);
    }

    public void setOnBackToMenu(Runnable callback) {
        this.onBackToMenu = callback;
    }

    public void setOnServerSelected(Consumer<GameServer> callback) {
        this.onServerSelected = callback;
    }

    public void onShow() {
        startPolling();
    }

    public void onLeaveServerBrowser() {
        stopPolling();
    }

    private void onStartGameClicked() {
    }

    @Override
    protected void paintComponent(Graphics g) {
        if (backgroundImage != null) {
            g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
        } else {
            super.paintComponent(g);
        }
    }

    private void loadBackgroundImage() {
        try {
            var url = getClass().getResource("/images/ui/background.jpg");
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
            } else {
                System.err.println("Lobby background not found: /images/ui/background.jpg");
            }
        } catch (Exception e) {
            System.err.println("Error loading lobby background: " + e.getMessage());
        }
    }

    private void startPolling() {
        if (poller != null && !poller.isShutdown()) return;
        poller = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "ServerBrowser-Poller");
            t.setDaemon(true);
            return t;
        });

        poller.scheduleAtFixedRate(() -> {
            try {
                List<GameServer> servers = serversApi.listServers();
                updateServerList(servers);
            } catch (Exception ex) {
                ex.printStackTrace();
                SwingUtilities.invokeLater(() -> statusLabel.setText("Fehler beim Laden"));
            }
        }, 0, 500, TimeUnit.MILLISECONDS);
    }

    private void stopPolling() {
        if (poller != null) {
            poller.shutdownNow();
            poller = null;
        }
    }

    private void updateServerList(List<GameServer> servers) {
        SwingUtilities.invokeLater(() -> {
            serverListModel.clear();
            if (servers == null || servers.isEmpty()) {
                statusLabel.setText("Keine Server gefunden");
                return;
            }

            int added = 0;
            for (GameServer s : servers) {
                if (s == null) continue;
                if (!isLobby(s)) continue;
                serverListModel.addElement(s);
                added++;
            }

            if (added == 0) {
                statusLabel.setText("Keine Lobbys verfügbar");
            } else {
                statusLabel.setText("Aktualisiert: " + LocalTime.now().format(timeFmt));
            }
        });
    }

    private boolean isLobby(GameServer s) {
        try {
            var st = s.getStatus();
            if (st == null) return false;
            String stStr;
            try {
                stStr = st.name();
            } catch (Throwable ignore) {
                stStr = st.toString();
            }
            return "LOBBY".equalsIgnoreCase(stStr);
        } catch (Throwable ex) {
            return false;
        }
    }

    private class ServerListRenderer extends JPanel implements ListCellRenderer<GameServer> {
        private final JLabel nameLabel = new JLabel();
        private final JLabel playersLabel = new JLabel();
        private final JPanel avatarPanel = new JPanel();

        private static final Color[] SERVER_COLORS = {
                new Color(200, 70, 70),
                new Color(70, 160, 70),
                new Color(70, 130, 200),
                new Color(200, 180, 70)
        };

        public ServerListRenderer() {
            setLayout(new BorderLayout(12, 0));
            setOpaque(false);
            setBorder(new EmptyBorder(8, 12, 8, 12));

            avatarPanel.setPreferredSize(new Dimension(50, 50));
            avatarPanel.setOpaque(false);

            JPanel infoPanel = new JPanel();
            infoPanel.setOpaque(false);
            infoPanel.setLayout(new BorderLayout());

            nameLabel.setFont(nameFont);
            nameLabel.setForeground(TEXT_LIGHT);
            nameLabel.setHorizontalAlignment(SwingConstants.CENTER);

            playersLabel.setFont(new Font("SansSerif", Font.BOLD, 13));
            playersLabel.setForeground(TEXT_MUTED);
            playersLabel.setHorizontalAlignment(SwingConstants.CENTER);

            infoPanel.add(nameLabel, BorderLayout.CENTER);
            infoPanel.add(playersLabel, BorderLayout.SOUTH);

            add(avatarPanel, BorderLayout.WEST);
            add(infoPanel, BorderLayout.CENTER);
        }

        @Override
        public Component getListCellRendererComponent(JList<? extends GameServer> list, GameServer value, int index,
                                                      boolean isSelected, boolean cellHasFocus) {
            nameLabel.setText(value.getName());
            playersLabel.setText(value.getCurrentPlayerCount() + " / " + value.getMaxPlayers());

            avatarPanel.setBackground(SERVER_COLORS[Math.abs(index) % SERVER_COLORS.length]);
            avatarPanel.setOpaque(true);

            if (isSelected) {
                setOpaque(true);
                setBackground(new Color(255, 255, 255, 20));
            } else {
                setOpaque(false);
            }

            return this;
        }

        @Override
        protected void paintComponent(Graphics g) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            int w = getWidth();
            int h = getHeight();
            int arc = 12;

            g2.setColor(SHADOW_COLOR);
            g2.fill(new RoundRectangle2D.Float(3, 4, w - 6, h - 6, arc, arc));

            g2.setColor(CARD_BG);
            g2.fill(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc, arc));

            g2.setColor(CARD_BORDER);
            g2.setStroke(new BasicStroke(1.2f));
            g2.draw(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc, arc));

            int ax = 12;
            int ay = (h - 46) / 2;
            g2.setColor(avatarPanel.getBackground());
            g2.fillOval(ax, ay, 46, 46);
            g2.setColor(new Color(255, 255, 255, 60));
            g2.setStroke(new BasicStroke(2));
            g2.drawOval(ax, ay, 46, 46);

            g2.dispose();
            super.paintComponent(g);
        }
    }
}
