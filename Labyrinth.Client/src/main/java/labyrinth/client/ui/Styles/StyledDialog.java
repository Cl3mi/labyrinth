package labyrinth.client.ui.Styles;

import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeEffects;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.RoundRectangle2D;

/**
 * Custom themed dialog for the Labyrinth game.
 * Features a darkened overlay backdrop and medieval styling.
 */
public class StyledDialog extends JDialog {

    private static final int DIALOG_WIDTH = 480;
    private static final int DIALOG_MIN_HEIGHT = 220;
    private static final int PADDING = 25;
    private static final int ARC = 15;
    private static final Color OVERLAY_COLOR = new Color(0, 0, 0, 180);

    private JPanel glassPane;
    private boolean result = false;

    /**
     * Show a confirmation dialog with Yes/No buttons.
     *
     * @param parent  Parent component
     * @param title   Dialog title
     * @param message Dialog message
     * @return true if user clicked Yes, false otherwise
     */
    public static boolean showConfirm(Component parent, String title, String message) {
        Window window = getWindow(parent);
        StyledDialog dialog = new StyledDialog(window, title, message, true);
        dialog.setVisible(true);
        return dialog.result;
    }

    /**
     * Show a message dialog with OK button.
     *
     * @param parent  Parent component
     * @param title   Dialog title
     * @param message Dialog message
     */
    public static void showMessage(Component parent, String title, String message) {
        showMessage(parent, title, message, MessageType.INFO);
    }

    /**
     * Show a message dialog with OK button and specific type.
     *
     * @param parent  Parent component
     * @param title   Dialog title
     * @param message Dialog message
     * @param type    Message type (INFO, WARNING, ERROR)
     */
    public static void showMessage(Component parent, String title, String message, MessageType type) {
        Window window = getWindow(parent);
        StyledDialog dialog = new StyledDialog(window, title, message, false, type);
        dialog.setVisible(true);
    }

    /**
     * Show a warning message dialog.
     */
    public static void showWarning(Component parent, String title, String message) {
        showMessage(parent, title, message, MessageType.WARNING);
    }

    /**
     * Show an error message dialog.
     */
    public static void showError(Component parent, String title, String message) {
        showMessage(parent, title, message, MessageType.ERROR);
    }

    public enum MessageType {
        INFO("", GameTheme.Colors.ACCENT_GOLD),       // ASCII fallback
        WARNING("!", GameTheme.Colors.PRIMARY_GOLD_LIGHT),
        ERROR("X", GameTheme.Colors.PLAYER_RED);            // ASCII fallback

        final String icon;
        final Color color;

        MessageType(String icon, Color color) {
            this.icon = icon;
            this.color = color;
        }
    }

    private StyledDialog(Window owner, String title, String message, boolean isConfirm) {
        this(owner, title, message, isConfirm, MessageType.INFO);
    }

    private StyledDialog(Window owner, String title, String message, boolean isConfirm, MessageType type) {
        super(owner, Dialog.ModalityType.APPLICATION_MODAL);

        setUndecorated(true);
        setBackground(new Color(0, 0, 0, 0));

        // Setup glass pane overlay on owner window
        if (owner instanceof JFrame frame) {
            setupGlassPane(frame);
        } else if (owner instanceof JDialog dialog) {
            setupGlassPane(dialog);
        }

        // Create content panel
        JPanel contentPanel = createContentPanel(title, message, isConfirm, type);
        add(contentPanel);

        // Size and position
        pack();
        setLocationRelativeTo(owner);

        // Close on Escape
        getRootPane().registerKeyboardAction(
                e -> closeDialog(false),
                KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
                JComponent.WHEN_IN_FOCUSED_WINDOW
        );

        // Enter confirms for confirm dialogs
        if (isConfirm) {
            getRootPane().registerKeyboardAction(
                    e -> closeDialog(true),
                    KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0),
                    JComponent.WHEN_IN_FOCUSED_WINDOW
            );
        } else {
            getRootPane().registerKeyboardAction(
                    e -> closeDialog(true),
                    KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0),
                    JComponent.WHEN_IN_FOCUSED_WINDOW
            );
        }
    }

    private void setupGlassPane(RootPaneContainer container) {
        glassPane = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                g.setColor(OVERLAY_COLOR);
                g.fillRect(0, 0, getWidth(), getHeight());
            }
        };
        glassPane.setOpaque(false);
        glassPane.addMouseListener(new MouseAdapter() {
            // Consume all mouse events to create focus trap
            @Override
            public void mousePressed(MouseEvent e) {
                e.consume();
            }
        });

        container.setGlassPane(glassPane);
        glassPane.setVisible(true);
    }

    private JPanel createContentPanel(String title, String message, boolean isConfirm, MessageType type) {
        JPanel panel = new JPanel(new BorderLayout(0, 15)) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                int w = getWidth();
                int h = getHeight();

                // Shadow
                g2.setColor(new Color(0, 0, 0, 100));
                g2.fill(new RoundRectangle2D.Float(6, 6, w - 6, h - 6, ARC, ARC));

                // Background
                g2.setColor(ThemeManager.getInstance().getSurfacePrimary());
                g2.fill(new RoundRectangle2D.Float(0, 0, w - 6, h - 6, ARC, ARC));

                // Border
                g2.setColor(type.color);
                g2.setStroke(new BasicStroke(2f));
                g2.draw(new RoundRectangle2D.Float(1, 1, w - 8, h - 8, ARC, ARC));

                // Top accent line
                g2.setColor(ThemeEffects.withAlpha(type.color, 100));
                g2.fillRoundRect(PADDING, 3, w - PADDING * 2 - 6, 3, 2, 2);

                g2.dispose();
            }
        };
        panel.setOpaque(false);
        panel.setBorder(BorderFactory.createEmptyBorder(PADDING, PADDING, PADDING, PADDING));

        JPanel titlePanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 10, 0));
        titlePanel.setOpaque(false);

        JLabel iconLabel = new JLabel(type.icon);
        iconLabel.setFont(FontManager.getHeadingMedium());
        iconLabel.setForeground(type.color);
        titlePanel.add(iconLabel);

        JLabel titleLabel = new JLabel(title);
        titleLabel.setFont(FontManager.getHeadingSmall());
        titleLabel.setForeground(type.color);
        titlePanel.add(titleLabel);

        panel.add(titlePanel, BorderLayout.NORTH);

        // Message
        JLabel messageLabel = new JLabel("<html><center>" + message.replace("\n", "<br>") + "</center></html>");
        messageLabel.setFont(FontManager.getBodyMedium());
        messageLabel.setForeground(ThemeManager.getInstance().getTextPrimary());
        messageLabel.setHorizontalAlignment(SwingConstants.CENTER);
        panel.add(messageLabel, BorderLayout.CENTER);

        // Buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 15, 0));
        buttonPanel.setOpaque(false);

        if (isConfirm) {
            StyledButton yesButton = createStyledButton("Ja", true, type.color);
            yesButton.addActionListener(e -> closeDialog(true));
            buttonPanel.add(yesButton);

            StyledButton noButton = createStyledButton("Nein", false, null);
            noButton.addActionListener(e -> closeDialog(false));
            buttonPanel.add(noButton);

            SwingUtilities.invokeLater(yesButton::requestFocusInWindow);
        } else {
            StyledButton okButton = createStyledButton("OK", true, type.color);
            okButton.addActionListener(e -> closeDialog(true));
            buttonPanel.add(okButton);

            SwingUtilities.invokeLater(okButton::requestFocusInWindow);
        }

        panel.add(buttonPanel, BorderLayout.SOUTH);


        int height = DIALOG_MIN_HEIGHT;
        int messageLines = message.split("\n").length;
        if (messageLines > 2) {
            height += (messageLines - 2) * 20;
        }
        panel.setPreferredSize(new Dimension(DIALOG_WIDTH, height));

        return panel;
    }

    private StyledButton createStyledButton(String text, boolean isPrimary, Color accentColor) {
        StyledButton.Style style;
        if (isPrimary) {
            if (accentColor != null && accentColor.equals(GameTheme.Colors.PLAYER_RED)) {
                style = StyledButton.Style.DIALOG_DANGER;
            } else {
                style = StyledButton.Style.DIALOG_PRIMARY;
            }
        } else {
            style = StyledButton.Style.DIALOG_SECONDARY;
        }

        StyledButton button = new StyledButton(text, style);
        button.setPreferredSize(new Dimension(100, 40));
        button.setFont(FontManager.getBodySmall(Font.BOLD));
        return button;
    }

    private void closeDialog(boolean confirmed) {
        this.result = confirmed;
        if (glassPane != null) {
            glassPane.setVisible(false);
        }
        dispose();
    }

    private static Window getWindow(Component component) {
        if (component == null) {
            return null;
        }
        if (component instanceof Window window) {
            return window;
        }
        return SwingUtilities.getWindowAncestor(component);
    }
}
