package labyrinth.client.ui;

import labyrinth.client.ui.Styles.StyledButton;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import labyrinth.client.ui.Styles.StyledContextMenu;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.managementclient.api.ServersApi;
import labyrinth.managementclient.model.GameServer;
import lombok.Setter;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
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

    private static final Logger log = LoggerFactory.getLogger(ServerBrowserPanel.class);

    @Setter
    private Runnable onBackToMenu;
    private Image backgroundImage;
    private StyledButton backButton;

    private final ServersApi serversApi;

    private final DefaultListModel<GameServer> serverListModel = new DefaultListModel<>();
    private final JList<GameServer> serverList;
    private final JLabel statusLabel;

    private final Font nameFont = FontManager.getBodyMedium(Font.BOLD);

    private ScheduledExecutorService poller;
    private final DateTimeFormatter timeFmt = DateTimeFormatter.ofPattern("HH:mm:ss");

    @Setter
    private Consumer<GameServer> onServerSelected;

    public ServerBrowserPanel(ServersApi serversApi) {
        this.serversApi = serversApi;

        loadBackgroundImage();

        // Listen for theme changes
        ThemeManager.getInstance().addThemeChangeListener(() -> {
            loadBackgroundImage();
            repaint();
        });

        setOpaque(false);
        setLayout(new BorderLayout());
        setBorder(new EmptyBorder(12, 12, 12, 12));

        JPanel header = new JPanel(new BorderLayout(20, 0));
        header.setOpaque(false);

        backButton = new StyledButton("Zurück", StyledButton.Style.SECONDARY);
        backButton.setPreferredSize(new Dimension(140, 40));
        backButton.setFocusPainted(false);
        backButton.addActionListener(e -> {
            onLeaveServerBrowser();
            if (onBackToMenu != null) onBackToMenu.run();
        });
        StyledTooltipManager.setTooltip(backButton, "Zurück", "Zurück zum Hauptmenü");
        StyledContextMenu.attachTo(backButton);
        JPanel leftPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        leftPanel.setOpaque(false);
        leftPanel.add(backButton);
        header.add(leftPanel, BorderLayout.WEST);

        JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setOpaque(false);

        JLabel headerTitle = new JLabel("Verfügbare Server");
        headerTitle.setFont(FontManager.getHeadingSmall());
        headerTitle.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        headerTitle.setAlignmentX(Component.CENTER_ALIGNMENT);

        statusLabel = new JLabel("Lade Server...");
        statusLabel.setFont(FontManager.getBodyMedium());
        statusLabel.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
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
                if (e.getButton() == MouseEvent.BUTTON1) { // Only left-click triggers selection
                    int idx = serverList.locationToIndex(e.getPoint());
                    if (idx >= 0) {
                        GameServer gs = serverListModel.getElementAt(idx);

                        if (onServerSelected != null) {
                            onServerSelected.accept(gs);
                        }
                    }
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showServerContextMenu(e);
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showServerContextMenu(e);
                }
            }

            private void showServerContextMenu(MouseEvent e) {
                int idx = serverList.locationToIndex(e.getPoint());
                if (idx >= 0) {
                    serverList.setSelectedIndex(idx);
                    GameServer gs = serverListModel.getElementAt(idx);
                    String tooltip = "Server: " + gs.getName() + "\nSpieler: " + gs.getCurrentPlayerCount() + "/" + gs.getMaxPlayers();
                    StyledContextMenu menu = new StyledContextMenu(serverList, tooltip);
                    menu.show(e.getComponent(), e.getX(), e.getY());
                }
            }
        });

        JPanel listCard = getListPanel();

        JLabel listTitle = new JLabel("📡 Verfügbare Server");
        listTitle.setFont(FontManager.getBodyLarge(Font.BOLD));
        listTitle.setForeground(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
        listCard.add(listTitle, BorderLayout.NORTH);

        JScrollPane sp = new JScrollPane(serverList);
        sp.setOpaque(false);
        sp.getViewport().setOpaque(false);
        sp.setBorder(BorderFactory.createEmptyBorder());
        sp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        listCard.add(sp, BorderLayout.CENTER);

        add(listCard, BorderLayout.CENTER);

        setupKeyboardNavigation();
    }

    private void setupKeyboardNavigation() {
        // Add keyboard navigation for the server list
        KeyboardNavigationHelper.setupListNavigation(serverList, server -> {
            if (onServerSelected != null) {
                onServerSelected.accept(server);
            }
        });

        // Add key listener for navigation between back button and server list
        KeyListener navListener = new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                int keyCode = e.getKeyCode();

                if (keyCode == KeyEvent.VK_ESCAPE) {
                    e.consume();
                    onLeaveServerBrowser();
                    if (onBackToMenu != null) onBackToMenu.run();
                    return;
                }

                // Tab key wrapping
                if (keyCode == KeyEvent.VK_TAB) {
                    e.consume();
                    if (focused == backButton) {
                        // Tab from back button -> server list
                        serverList.requestFocusInWindow();
                        if (serverList.getModel().getSize() > 0 && serverList.getSelectedIndex() < 0) {
                            serverList.setSelectedIndex(0);
                        }
                    } else {
                        // Tab from server list -> back button (wrap)
                        backButton.requestFocusInWindow();
                    }
                    return;
                }

                // Navigate between back button and server list
                if (focused == backButton) {
                    if (keyCode == KeyEvent.VK_DOWN || keyCode == KeyEvent.VK_S) {
                        e.consume();
                        serverList.requestFocusInWindow();
                        if (serverList.getModel().getSize() > 0 && serverList.getSelectedIndex() < 0) {
                            serverList.setSelectedIndex(0);
                        }
                    } else if (keyCode == KeyEvent.VK_ENTER || keyCode == KeyEvent.VK_SPACE) {
                        e.consume();
                        backButton.doClick();
                    }
                } else if (focused == serverList || isDescendant(serverList, focused)) {
                    if (keyCode == KeyEvent.VK_UP || keyCode == KeyEvent.VK_W) {
                        if (serverList.getSelectedIndex() <= 0) {
                            e.consume();
                            backButton.requestFocusInWindow();
                        }
                    }
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {}

            @Override
            public void keyReleased(KeyEvent e) {}
        };

        addKeyListener(navListener);
        backButton.addKeyListener(navListener);
        serverList.addKeyListener(navListener);

        // Disable default Tab traversal so our listener handles it
        backButton.setFocusTraversalKeysEnabled(false);
        serverList.setFocusTraversalKeysEnabled(false);

        setFocusable(true);
        setFocusTraversalKeysEnabled(false);

        // Request focus on first component when panel gains focus
        addFocusListener(new java.awt.event.FocusAdapter() {
            @Override
            public void focusGained(java.awt.event.FocusEvent e) {
                if (serverList.getModel().getSize() > 0) {
                    serverList.requestFocusInWindow();
                    if (serverList.getSelectedIndex() < 0) {
                        serverList.setSelectedIndex(0);
                    }
                } else {
                    backButton.requestFocusInWindow();
                }
            }
        });
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
        SwingUtilities.invokeLater(() -> {
            if (serverList.getModel().getSize() > 0) {
                serverList.setSelectedIndex(0);
                serverList.requestFocusInWindow();
            } else {
                backButton.requestFocusInWindow();
            }
        });
    }

    private @NonNull JPanel getListPanel() {
        JPanel listCard = new JPanel(new BorderLayout(0, 15)) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                g2.setColor(GameTheme.Colors.CARD_BG);
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth(), getHeight(), 15, 15));

                g2.setColor(GameTheme.Colors.CARD_BORDER);
                g2.setStroke(new BasicStroke(2));
                g2.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 2, getHeight() - 2, 15, 15));

                g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 10));
                g2.fill(new RoundRectangle2D.Float(2, 2, getWidth() - 4, 40, 13, 13));

                g2.dispose();
                super.paintComponent(g);
            }
        };
        listCard.setOpaque(false);
        listCard.setBorder(new EmptyBorder(20, 25, 20, 25));
        return listCard;
    }

    public void onShow() {
        startPolling();
    }

    public void onLeaveServerBrowser() {
        stopPolling();
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        if (backgroundImage != null) {
            g2.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
        } else {
            super.paintComponent(g);
        }
    }

    private void loadBackgroundImage() {
        try {
            String imagePath = ThemeManager.getInstance().getBackgroundImagePath();
            var url = getClass().getResource(imagePath);
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
                log.info("[ServerBrowserPanel] Loaded background: {}", imagePath);
            } else {
                log.error("Background not found: {}", imagePath);
            }
        } catch (Exception e) {
            log.error("Error loading background: {}", e.getMessage());
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
            // Preserve selection before clearing - use URI as unique identifier
            String selectedServerUri = null;
            int previousIndex = serverList.getSelectedIndex();
            if (previousIndex >= 0 && previousIndex < serverListModel.size()) {
                GameServer selected = serverListModel.getElementAt(previousIndex);
                if (selected != null) {
                    selectedServerUri = selected.getUri();
                }
            }

            serverListModel.clear();
            if (servers == null || servers.isEmpty()) {
                statusLabel.setText("Keine Server gefunden");
                return;
            }

            int added = 0;
            int restoredIndexByUri = -1;
            for (GameServer s : servers) {
                if (s == null) continue;
                if (!isLobby(s)) continue;
                serverListModel.addElement(s);
                // Check if this is the previously selected server by URI
                if (selectedServerUri != null && selectedServerUri.equals(s.getUri())) {
                    restoredIndexByUri = added;
                }
                added++;
            }

            if (added == 0) {
                statusLabel.setText("Keine Lobbys verfügbar");
            } else {
                statusLabel.setText("Aktualisiert: " + LocalTime.now().format(timeFmt));
                // Restore selection: prefer same server by URI, fallback to same index
                if (restoredIndexByUri >= 0) {
                    serverList.setSelectedIndex(restoredIndexByUri);
                } else if (previousIndex >= 0 && previousIndex < added) {
                    serverList.setSelectedIndex(previousIndex);
                } else if (previousIndex >= added && added > 0) {
                    // Previous index is out of bounds, select last item
                    serverList.setSelectedIndex(added - 1);
                }
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
                GameTheme.Colors.getPlayerColor(0),
                GameTheme.Colors.getPlayerColor(1),
                GameTheme.Colors.getPlayerColor(2),
                GameTheme.Colors.getPlayerColor(3)
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
            nameLabel.setForeground(GameTheme.Colors.TEXT_LIGHT);
            nameLabel.setHorizontalAlignment(SwingConstants.CENTER);

            playersLabel.setFont(FontManager.getBodySmall());
            playersLabel.setForeground(GameTheme.Colors.TEXT_MUTED);
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
            playersLabel.setText(value.getCurrentPlayerCount() + " / " + value.getMaxPlayers() + " Spieler");

            avatarPanel.setBackground(SERVER_COLORS[Math.abs(index) % SERVER_COLORS.length]);
            avatarPanel.setOpaque(true);

            if (isSelected) {
                setOpaque(true);
                setBackground(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 20));
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

            g2.setColor(GameTheme.Colors.SHADOW_COLOR);
            g2.fill(new RoundRectangle2D.Float(3, 4, w - 6, h - 6, arc, arc));

            g2.setColor(GameTheme.Colors.CARD_BG);
            g2.fill(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc, arc));

            g2.setColor(GameTheme.Colors.CARD_BORDER);
            g2.setStroke(new BasicStroke(1.2f));
            g2.draw(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc, arc));

            int ax = 12;
            int ay = (h - 46) / 2;
            g2.setColor(avatarPanel.getBackground());
            g2.fillOval(ax, ay, 46, 46);
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), 60));
            g2.setStroke(new BasicStroke(2));
            g2.drawOval(ax, ay, 46, 46);

            g2.dispose();
            super.paintComponent(g);
        }
    }
}
