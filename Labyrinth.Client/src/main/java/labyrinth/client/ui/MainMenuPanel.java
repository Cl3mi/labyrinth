package labyrinth.client.ui;

import labyrinth.client.audio.AudioPlayer;
import labyrinth.client.ui.Styles.StyledButton;
import labyrinth.client.ui.Styles.StyledContextMenu;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.client.ui.theme.ThemeEffects;
import lombok.Getter;
import lombok.Setter;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;
import java.util.function.Consumer;


public class MainMenuPanel extends JPanel {

    @Setter
    private Runnable onMultiplayerClicked;
    @Setter
    private Runnable onOptionsClicked;
    @Setter
    private Runnable onExitClicked;

    @Getter
    private String multiplayerUsername = "Player";


    public void showMultiplayerUsernameDialog(Consumer<String> onUsernameEntered) {
        Window ownerWindow = SwingUtilities.getWindowAncestor(this);
        JDialog dialog = new JDialog(ownerWindow, "Mehrspieler - Spielername", Dialog.ModalityType.APPLICATION_MODAL);
        dialog.setUndecorated(true);
        dialog.setBackground(new Color(0, 0, 0, 0));

        // Setup glass pane overlay (focus trap with darkened background)
        JPanel glassPane = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                g.setColor(new Color(0, 0, 0, 180));
                g.fillRect(0, 0, getWidth(), getHeight());
            }
        };
        glassPane.setOpaque(false);
        glassPane.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                e.consume(); // Block clicks outside dialog
            }
        });
        if (ownerWindow instanceof JFrame jFrame) {
            jFrame.setGlassPane(glassPane);
            glassPane.setVisible(true);
        }

        // Create styled content panel
        JPanel mainPanel = new JPanel(new BorderLayout(10, 15)) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Shadow
                g2.setColor(new Color(0, 0, 0, 100));
                g2.fill(new RoundRectangle2D.Float(6, 6, getWidth() - 6, getHeight() - 6, 15, 15));

                // Background
                g2.setColor(ThemeManager.getInstance().getSurfacePrimary());
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth() - 6, getHeight() - 6, 15, 15));

                // Border
                g2.setColor(GameTheme.Colors.ACCENT_GOLD);
                g2.setStroke(new BasicStroke(2f));
                g2.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 8, getHeight() - 8, 15, 15));

                g2.dispose();
            }
        };
        mainPanel.setOpaque(false);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(25, 30, 25, 30));


        JLabel titleLabel = new JLabel("Gib deinen Spielernamen ein", SwingConstants.CENTER);
        titleLabel.setFont(FontManager.getHeadingMedium());
        titleLabel.setForeground(GameTheme.Colors.ACCENT_GOLD);
        mainPanel.add(titleLabel, BorderLayout.NORTH);

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setOpaque(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 5, 10, 5);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.3;
        JLabel nameLabel = createDialogLabel("Spielername:");
        inputPanel.add(nameLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 0.7;
        JTextField usernameField = new JTextField(multiplayerUsername, 15);
        usernameField.setFont(FontManager.getBodyMedium(Font.PLAIN));
        usernameField.setBackground(ThemeManager.getInstance().getSurfaceSecondary());
        usernameField.setForeground(ThemeManager.getInstance().getTextPrimary());
        usernameField.setCaretColor(ThemeManager.getInstance().getTextPrimary());
        usernameField.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(GameTheme.Colors.ACCENT_COPPER, 1),
                BorderFactory.createEmptyBorder(8, 10, 8, 10)
        ));
        usernameField.setPreferredSize(new Dimension(200, 40));
        StyledTooltipManager.setTooltip(usernameField, "Spielername", "Dein Anzeigename im Multiplayer-Spiel");
        inputPanel.add(usernameField, gbc);

        mainPanel.add(inputPanel, BorderLayout.CENTER);

        // Buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
        buttonPanel.setOpaque(false);

        StyledButton cancelButton = new StyledButton("Abbrechen", StyledButton.Style.SECONDARY);
        cancelButton.setPreferredSize(new Dimension(130, 42));
        cancelButton.addActionListener(e -> {
            glassPane.setVisible(false);
            dialog.dispose();
        });
        StyledTooltipManager.setTooltip(cancelButton, "Abbrechen", "Zurück zur Serverauswahl");
        StyledContextMenu.attachTo(cancelButton);

        StyledButton joinButton = new StyledButton("Beitreten", StyledButton.Style.PRIMARY);
        joinButton.setPreferredSize(new Dimension(130, 42));
        joinButton.addActionListener(e -> {
            String enteredUsername = usernameField.getText().trim();
            if (enteredUsername.isEmpty()) {
                usernameField.requestFocus();
                return;
            }
            multiplayerUsername = enteredUsername;

            glassPane.setVisible(false);
            dialog.dispose();

            if (onUsernameEntered != null) {
                onUsernameEntered.accept(enteredUsername);
            }
        });
        StyledTooltipManager.setTooltip(joinButton, "Beitreten", "Mit eingegebenem Namen dem Spiel beitreten");
        StyledContextMenu.attachTo(joinButton);

        buttonPanel.add(cancelButton);
        buttonPanel.add(joinButton);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);


        usernameField.addActionListener(e -> joinButton.doClick());


        dialog.getRootPane().registerKeyboardAction(
                e -> {
                    glassPane.setVisible(false);
                    dialog.dispose();
                },
                KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
                JComponent.WHEN_IN_FOCUSED_WINDOW
        );


        dialog.addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowOpened(java.awt.event.WindowEvent e) {
                usernameField.requestFocusInWindow();
                usernameField.selectAll();
            }
            @Override
            public void windowClosed(java.awt.event.WindowEvent e) {
                glassPane.setVisible(false);
            }
        });

        dialog.setContentPane(mainPanel);
        dialog.setSize(520, 220);
        dialog.setLocationRelativeTo(ownerWindow);
        dialog.setVisible(true);
    }

    private JLabel createDialogLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(FontManager.getBodyMedium(Font.PLAIN));
        label.setForeground(ThemeManager.getInstance().getTextPrimary());
        return label;
    }

    private Image backgroundImage;
    private Image logoImage;

    private JLabel subtitleLabel;

    public MainMenuPanel() {
        loadResources();
        initMusic();
        setupUI();

        ThemeManager.getInstance().addThemeChangeListener(() -> {
            loadBackgroundImage();
            subtitleLabel.setForeground(ThemeManager.getInstance().getSubtitleColor());
            repaint();
        });
    }

    @Override
    public void setVisible(boolean aFlag) {
        super.setVisible(aFlag);
        if (aFlag) {
            initMusic();
        }
    }





    private void loadResources() {
        loadBackgroundImage();

        try {
            var logoUrl = getClass().getResource("/images/ui/logo.png");
            if (logoUrl != null) {
                logoImage = new ImageIcon(logoUrl).getImage();
            } else {
                System.err.println("Logo not found: /images/ui/logo.png");
            }
        } catch (Exception e) {
            System.err.println("Error loading logo: " + e.getMessage());
        }
    }

    private void loadBackgroundImage() {
        try {
            String imagePath = ThemeManager.getInstance().getBackgroundImagePath();
            var url = getClass().getResource(imagePath);
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
                System.out.println("[MainMenuPanel] Loaded background: " + imagePath);
            } else {
                System.err.println("Background not found: " + imagePath);
            }
        } catch (Exception e) {
            System.err.println("Error loading background: " + e.getMessage());
        }
    }

    private void initMusic() {
        java.util.prefs.Preferences prefs =
                java.util.prefs.Preferences.userNodeForPackage(OptionsPanel.class);

        float volume = prefs.getInt("musicVolume", 50) / 100f;

        AudioPlayer.getInstance().setMusicVolume(volume);
        AudioPlayer.getInstance().playMenuMusic();
    }


    public void setMusicVolume(int volume) {
        float vol = Math.max(0, Math.min(100, volume)) / 100.0f;
        AudioPlayer.getInstance().setMusicVolume(vol);
    }

    public void stopMusic() {
        AudioPlayer.getInstance().stopMusic();
    }

    private void setupUI() {
        setOpaque(false);
        setLayout(new GridBagLayout());

        JPanel contentPanel = new JPanel();
        contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));
        contentPanel.setOpaque(false);
        contentPanel.setBorder(new EmptyBorder(20, 40, 40, 40));

        contentPanel.add(Box.createVerticalGlue());

        JPanel logoPanel = createLogoPanel();
        logoPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(logoPanel);

        contentPanel.add(Box.createVerticalStrut(15));

        subtitleLabel = new JLabel("Das mystische Abenteuer beginnt... DiBSE 2025") {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2d = (Graphics2D) g.create();
                g2d.setColor(ThemeManager.getInstance().getSubtitleColor());
                g2d.fillRoundRect(0, 0, getWidth(), getHeight(), 8, 8);
                g2d.dispose();
                super.paintComponent(g);
            }
        };
        subtitleLabel.setFont(FontManager.getBodyLarge(Font.BOLD));
        subtitleLabel.setForeground(ThemeManager.getInstance().getSubtitleColor());
        subtitleLabel.setBorder(BorderFactory.createEmptyBorder(4, 12, 4, 12));
        subtitleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(subtitleLabel);

        contentPanel.add(Box.createVerticalStrut(30));

        JPanel buttonContainer = createButtonPanel();
        buttonContainer.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(buttonContainer);

        contentPanel.add(Box.createVerticalGlue());

        // Footer
        JPanel footerPanel = createFooterPanel();
        footerPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        contentPanel.add(footerPanel);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        add(contentPanel, gbc);
    }


    private JPanel createLogoPanel() {
        JPanel panel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
                g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);

                int centerX = getWidth() / 2;
                int centerY = getHeight() / 2;

                if (logoImage != null) {
                    int originalWidth = logoImage.getWidth(null);
                    int originalHeight = logoImage.getHeight(null);

                    if (originalWidth > 0 && originalHeight > 0) {
                        int panelWidth = getWidth();
                        int panelHeight = getHeight();

                        int targetWidth = Math.min((int)(panelWidth * 0.8), 600);
                        int targetHeight = Math.min((int)(panelHeight * 0.9), 200);

                        double scaleX = (double) targetWidth / originalWidth;
                        double scaleY = (double) targetHeight / originalHeight;
                        double scale = Math.min(scaleX, scaleY);

                        int scaledWidth = (int) (originalWidth * scale);
                        int scaledHeight = (int) (originalHeight * scale);

                        int x = centerX - scaledWidth / 2;
                        int y = centerY - scaledHeight / 2;

                        g2.drawImage(logoImage, x, y, scaledWidth, scaledHeight, null);
                    }
                } else {
                    drawTextLogo(g2, centerX);
                }

                g2.dispose();
            }

            private void drawTextLogo(Graphics2D g2, int centerX) {
                String title = "Das Verrückte";
                String title2 = "LABYRINTH";

                g2.setFont(FontManager.getHeadingLarge());
                FontMetrics fm = g2.getFontMetrics();
                int titleWidth = fm.stringWidth(title);

                g2.setColor(GameTheme.Colors.shadow());
                g2.drawString(title, centerX - titleWidth / 2 + 3, 43);
                g2.setColor(GameTheme.Colors.PRIMARY_GOLD_LIGHT);
                g2.drawString(title, centerX - titleWidth / 2, 40);

                fm = g2.getFontMetrics();
                int title2Width = fm.stringWidth(title2);

                for (int i = 8; i > 0; i--) {
                    float alpha = 0.04f * i;
                    g2.setColor(new Color(GameTheme.Colors.PRIMARY_GOLD_LIGHT.getRed(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getGreen(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getBlue(), (int) (alpha * 255)));
                    g2.drawString(title2, centerX - title2Width / 2 - i / 2, 105);
                    g2.drawString(title2, centerX - title2Width / 2 + i / 2, 105);
                }

                g2.setColor(GameTheme.Colors.shadow());
                g2.drawString(title2, centerX - title2Width / 2 + 4, 109);

                GradientPaint goldGradient = new GradientPaint(
                        centerX - title2Width / 2, 60, GameTheme.Colors.PRIMARY_GOLD_LIGHT,
                        centerX + title2Width / 2, 115, GameTheme.Colors.PRIMARY_GOLD_DARK
                );
                g2.setPaint(goldGradient);
                g2.drawString(title2, centerX - title2Width / 2, 105);
            }
        };
        panel.setOpaque(false);

        panel.setPreferredSize(new Dimension(600, 200));
        panel.setMinimumSize(new Dimension(300, 100));
        return panel;
    }

    private JPanel createButtonPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setOpaque(false);


        MenuButton multiplayerBtn = new MenuButton("Spiel starten", "Spiele alleine gegen KI oder online mit Freunden");
        multiplayerBtn.addActionListener(e -> onMultiplayerClicked.run());
        multiplayerBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        StyledTooltipManager.setTooltip(multiplayerBtn, "Spiel starten", "Spiele alleine gegen KI oder online mit Freunden");
        StyledContextMenu.attachTo(multiplayerBtn);
        panel.add(multiplayerBtn);

        panel.add(Box.createVerticalStrut(12));

        MenuButton optionsBtn = new MenuButton("Einstellungen", "Grafik, Audio & mehr");
        optionsBtn.addActionListener(e -> {
            if (onOptionsClicked != null) onOptionsClicked.run();
        });
        optionsBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        StyledTooltipManager.setTooltip(optionsBtn, "Einstellungen", "Grafik, Audio und Spieloptionen anpassen");
        StyledContextMenu.attachTo(optionsBtn);
        panel.add(optionsBtn);

        panel.add(Box.createVerticalStrut(20));

        ExitButton exitBtn = new ExitButton("Beenden");
        exitBtn.addActionListener(e -> {
            if (onExitClicked != null) {
                onExitClicked.run();
            } else {
                System.exit(0);
            }
        });
        exitBtn.setAlignmentX(Component.CENTER_ALIGNMENT);
        StyledTooltipManager.setTooltip(exitBtn, "Beenden", "Spiel verlassen und Anwendung schließen");
        StyledContextMenu.attachTo(exitBtn);
        panel.add(exitBtn);

        return panel;
    }

    private JPanel createFooterPanel() {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel.setOpaque(false);

        JLabel versionLabel = new JLabel("Version 1.0  |  © 2025 Gruppe 1");
        versionLabel.setFont(FontManager.getBodySmall(Font.PLAIN));
        versionLabel.setForeground(ThemeEffects.withAlpha(GameTheme.Colors.TEXT_MUTED, 150));
        panel.add(versionLabel);

        return panel;
    }

    public void setMultiplayerUsername(String username) {
        if (username != null && !username.isBlank()) {
            this.multiplayerUsername = username;
        }
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        int w = getWidth();
        int h = getHeight();

        if (backgroundImage != null) {
            g2.drawImage(backgroundImage, 0, 0, w, h, this);
        } else {
            GradientPaint gradient = new GradientPaint(
                    0, 0, GameTheme.Colors.stoneDark(),
                    0, h, GameTheme.Colors.stoneMedium()
            );
            g2.setPaint(gradient);
            g2.fillRect(0, 0, w, h);
        }

        g2.setColor(ThemeManager.getInstance().getShadow());
        g2.fillRect(0, 0, w, h);

        drawVignette(g2, w, h);
        drawDecorativeCorners(g2, w, h);

        g2.dispose();
        super.paintComponent(g);
    }

    private void drawVignette(Graphics2D g2, int w, int h) {
        int centerX = w / 2;
        int centerY = h / 2;
        float radius = Math.max(w, h) * 0.8f;

        RadialGradientPaint vignette = new RadialGradientPaint(
                centerX, centerY, radius,
                new float[]{0.3f, 0.7f, 1.0f},
                new Color[]{
                        new Color(0, 0, 0, 0),
                        ThemeEffects.withAlpha(GameTheme.Colors.shadow(), 50),
                        ThemeEffects.withAlpha(GameTheme.Colors.shadow(), 130)
                }
        );
        g2.setPaint(vignette);
        g2.fillRect(0, 0, w, h);
    }

    private void drawDecorativeCorners(Graphics2D g2, int w, int h) {
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        Color gold = GameTheme.Colors.PRIMARY_GOLD_LIGHT;
        g2.setColor(new Color(gold.getRed(), gold.getGreen(), gold.getBlue(), 50));
        g2.setStroke(new BasicStroke(2f));

        int size = 50;

        // up left
        g2.drawLine(25, 25, 25 + size, 25);
        g2.drawLine(25, 25, 25, 25 + size);
        g2.fillOval(22, 22, 6, 6);

        // up right
        g2.drawLine(w - 25, 25, w - 25 - size, 25);
        g2.drawLine(w - 25, 25, w - 25, 25 + size);
        g2.fillOval(w - 28, 22, 6, 6);

        // bottom left
        g2.drawLine(25, h - 25, 25 + size, h - 25);
        g2.drawLine(25, h - 25, 25, h - 25 - size);
        g2.fillOval(22, h - 28, 6, 6);

        // bottom right
        g2.drawLine(w - 25, h - 25, w - 25 - size, h - 25);
        g2.drawLine(w - 25, h - 25, w - 25, h - 25 - size);
        g2.fillOval(w - 28, h - 28, 6, 6);
    }



    private class MenuButton extends JButton {
        private final String subtitle;
        private float hoverProgress = 0f;
        private boolean isHovered = false;
        private boolean isFocused = false;

        public MenuButton(String text, String subtitle) {
            super(text);
            this.subtitle = subtitle;

            setFont(FontManager.getBodyMedium(Font.BOLD));
            setForeground(GameTheme.Colors.textLight());
            setFocusPainted(false);
            setBorderPainted(false);
            setContentAreaFilled(false);
            setCursor(new Cursor(Cursor.HAND_CURSOR));
            setFocusable(true);

            setPreferredSize(new Dimension(320, 70));
            setMinimumSize(new Dimension(200, 50));
            setMaximumSize(new Dimension(450, 90));

            var animationTimer = new Timer(16, e -> {
                if (isHovered && hoverProgress < 1f) {
                    hoverProgress = Math.min(1f, hoverProgress + 0.12f);
                    repaint();
                } else if (!isHovered && hoverProgress > 0f) {
                    hoverProgress = Math.max(0f, hoverProgress - 0.12f);
                    repaint();
                }
            });
            animationTimer.start();

            addMouseListener(new MouseAdapter() {
                @Override
                public void mouseEntered(MouseEvent e) {
                    isHovered = true;
                }

                @Override
                public void mouseExited(MouseEvent e) {
                    isHovered = false;
                }
            });

            addFocusListener(new java.awt.event.FocusAdapter() {
                @Override
                public void focusGained(java.awt.event.FocusEvent e) {
                    isFocused = true;
                    repaint();
                }
                @Override
                public void focusLost(java.awt.event.FocusEvent e) {
                    isFocused = false;
                    repaint();
                }
            });
        }

        @Override
        protected void paintComponent(Graphics g) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int w = getWidth();
            int h = getHeight();
            int arc = 10;

            // shadow
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.shadow(), (int) (70 + 30 * hoverProgress)));
            g2.fill(new RoundRectangle2D.Float(4, 5, w - 8, h - 7, arc, arc));

            // background
            Color bgStart = interpolateColor(GameTheme.Colors.stoneDark(), GameTheme.Colors.stoneDark().darker(), hoverProgress);
            Color bgEnd = interpolateColor(GameTheme.Colors.stoneMedium(), GameTheme.Colors.stoneMedium().darker(), hoverProgress);
            GradientPaint bgGradient = new GradientPaint(0, 0, bgStart, 0, h, bgEnd);
            g2.setPaint(bgGradient);
            g2.fill(new RoundRectangle2D.Float(2, 2, w - 4, h - 4, arc, arc));

            // golden border
            Color borderColor = interpolateColor(GameTheme.Colors.PRIMARY_GOLD_DARK, GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress);
            g2.setColor(borderColor);
            g2.setStroke(new BasicStroke(2f + hoverProgress * 0.5f));
            g2.draw(new RoundRectangle2D.Float(2, 2, w - 5, h - 5, arc, arc));

            // Focus indicator - glowing outline
            if (isFocused) {
                // Outer glow
                g2.setColor(new Color(GameTheme.Colors.PRIMARY_GOLD_LIGHT.getRed(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getGreen(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getBlue(), 80));
                g2.setStroke(new BasicStroke(4f));
                g2.draw(new RoundRectangle2D.Float(0, 0, w - 1, h - 1, arc + 4, arc + 4));
                // Inner bright ring
                g2.setColor(new Color(GameTheme.Colors.PRIMARY_GOLD_LIGHT.getRed(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getGreen(), GameTheme.Colors.PRIMARY_GOLD_LIGHT.getBlue(), 200));
                g2.setStroke(new BasicStroke(2f));
                g2.draw(new RoundRectangle2D.Float(1, 1, w - 3, h - 3, arc + 2, arc + 2));
            }

            // glow
            g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textLight(), (int) (15 + 20 * hoverProgress)));
            g2.fill(new RoundRectangle2D.Float(4, 4, w - 8, (h - 8) / 3f, arc - 2, arc - 2));

            // text
            g2.setFont(FontManager.getBodyMedium(Font.BOLD));
            FontMetrics fm = g2.getFontMetrics();
            String text = getText();
            int textWidth = fm.stringWidth(text);
            int textX = (w - textWidth) / 2;
            int textY = h / 2 - 3;

            g2.setColor(GameTheme.Colors.shadow());
            g2.drawString(text, textX + 2, textY + 2);

            Color textColor = interpolateColor(GameTheme.Colors.textLight(), GameTheme.Colors.PRIMARY_GOLD_LIGHT, hoverProgress * 0.4f);
            g2.setColor(textColor);
            g2.drawString(text, textX, textY);

            // subtitle
            if (subtitle != null && !subtitle.isEmpty()) {
                g2.setFont(FontManager.getBodySmall(Font.PLAIN));
                fm = g2.getFontMetrics();
                int subWidth = fm.stringWidth(subtitle);
                g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.textMuted(), (int) (140 + 40 * hoverProgress)));
                g2.drawString(subtitle, (w - subWidth) / 2, textY + 18);
            }

            g2.dispose();
        }

        private Color interpolateColor(Color c1, Color c2, float t) {
            int r = (int) (c1.getRed() + (c2.getRed() - c1.getRed()) * t);
            int gr = (int) (c1.getGreen() + (c2.getGreen() - c1.getGreen()) * t);
            int b = (int) (c1.getBlue() + (c2.getBlue() - c1.getBlue()) * t);
            int a = (int) (c1.getAlpha() + (c2.getAlpha() - c1.getAlpha()) * t);
            return new Color(r, gr, b, a);
        }
    }



    private static class ExitButton extends JButton {
        private boolean isHovered = false;
        private boolean isFocused = false;

        public ExitButton(String text) {
            super(text);
            setFont(FontManager.getBodySmall(Font.BOLD));
            setForeground(GameTheme.Colors.PLAYER_RED);
            setFocusPainted(false);
            setBorderPainted(false);
            setContentAreaFilled(false);
            setCursor(new Cursor(Cursor.HAND_CURSOR));
            setFocusable(true);
            setPreferredSize(new Dimension(130, 38));
            setMaximumSize(new Dimension(150, 45));
            setMinimumSize(new Dimension(100, 32));

            addMouseListener(new MouseAdapter() {
                @Override
                public void mouseEntered(MouseEvent e) {
                    isHovered = true;
                    repaint();
                }

                @Override
                public void mouseExited(MouseEvent e) {
                    isHovered = false;
                    repaint();
                }
            });

            addFocusListener(new java.awt.event.FocusAdapter() {
                @Override
                public void focusGained(java.awt.event.FocusEvent e) {
                    isFocused = true;
                    repaint();
                }
                @Override
                public void focusLost(java.awt.event.FocusEvent e) {
                    isFocused = false;
                    repaint();
                }
            });
        }

        @Override
        protected void paintComponent(Graphics g) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int w = getWidth();
            int h = getHeight();

            if (isHovered) {
                Color red = GameTheme.Colors.PLAYER_RED;
                g2.setColor(new Color(red.getRed(), red.getGreen(), red.getBlue(), 180));
                g2.fillRoundRect(0, 0, w, h, 8, 8);
                g2.setColor(new Color(GameTheme.Colors.PLAYER_RED.getRed(), GameTheme.Colors.PLAYER_RED.getGreen(), GameTheme.Colors.PLAYER_RED.getBlue()));
                g2.setStroke(new BasicStroke(2));
                g2.drawRoundRect(1, 1, w - 2, h - 2, 8, 8);
                g2.setColor(new Color(GameTheme.Colors.PLAYER_RED.getRed(), GameTheme.Colors.PLAYER_RED.getGreen(), GameTheme.Colors.PLAYER_RED.getBlue(), 200));
            } else {
                Color base = GameTheme.Colors.stoneDark();
                g2.setColor(ThemeEffects.withAlpha(base, 120));
                g2.fillRoundRect(0, 0, w, h, 8, 8);
                g2.setColor(GameTheme.Colors.PLAYER_RED);
                g2.setStroke(new BasicStroke(1.5f));
                g2.drawRoundRect(1, 1, w - 2, h - 2, 8, 8);
                g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.PLAYER_RED, 200));
            }

            // Focus indicator - glowing outline
            if (isFocused) {
                // Outer glow (red-tinted for exit button)
                Color red = GameTheme.Colors.PLAYER_RED;
                g2.setColor(new Color(red.getRed(), red.getGreen(), red.getBlue(), 80));
                g2.setStroke(new BasicStroke(4f));
                g2.drawRoundRect(-1, -1, w + 1, h + 1, 12, 12);
                // Inner bright ring
                g2.setColor(new Color(red.getRed(), red.getGreen(), red.getBlue(), 200));
                g2.setStroke(new BasicStroke(2f));
                g2.drawRoundRect(0, 0, w - 1, h - 1, 10, 10);
            }

            g2.setFont(getFont());
            FontMetrics fm = g2.getFontMetrics();
            String text = getText();
            int textX = (w - fm.stringWidth(text)) / 2;
            int textY = (h + fm.getAscent() - fm.getDescent()) / 2;
            g2.drawString(text, textX, textY);

            g2.dispose();
        }
    }
}

