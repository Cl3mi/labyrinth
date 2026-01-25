package labyrinth.client.ui;

import labyrinth.client.ui.Styles.StyledButton;
import labyrinth.client.ui.Styles.StyledDialog;
import labyrinth.client.ui.Styles.StyledTooltipManager;
import labyrinth.client.ui.theme.FontManager;
import labyrinth.client.ui.theme.GameTheme;
import labyrinth.client.ui.theme.ThemeManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Centralized factory for creating styled dialogs throughout the application.
 * Provides consistent theming and behavior for confirmation, error, warning,
 * and input dialogs.
 */
public final class DialogFactory {

    private static final int DIALOG_WIDTH = 520;
    private static final int DIALOG_MIN_HEIGHT = 220;
    private static final int PADDING = 25;
    private static final int ARC = 15;
    private static final Color OVERLAY_COLOR = new Color(0, 0, 0, 180);

    private DialogFactory() {
    }

    /**
     * Shows a confirmation dialog with Yes/No buttons.
     *
     * @param parent  the parent component
     * @param title   the dialog title
     * @param message the confirmation message
     * @return true if user clicked Yes, false otherwise
     */
    public static boolean showConfirm(Component parent, String title, String message) {
        return StyledDialog.showConfirm(parent, title, message);
    }

    /**
     * Shows an error dialog with OK button.
     *
     * @param parent  the parent component
     * @param title   the dialog title
     * @param message the error message
     */
    public static void showError(Component parent, String title, String message) {
        StyledDialog.showError(parent, title, message);
    }

    /**
     * Shows an error dialog for an exception.
     *
     * @param parent    the parent component
     * @param title     the dialog title
     * @param exception the exception to display
     */
    public static void showError(Component parent, String title, Throwable exception) {
        String message = exception.getMessage();
        if (message == null || message.isBlank()) {
            message = exception.getClass().getSimpleName();
        }
        StyledDialog.showError(parent, title, message);
    }

    /**
     * Shows a warning dialog with OK button.
     *
     * @param parent  the parent component
     * @param title   the dialog title
     * @param message the warning message
     */
    public static void showWarning(Component parent, String title, String message) {
        StyledDialog.showWarning(parent, title, message);
    }

    /**
     * Shows an info dialog with OK button.
     *
     * @param parent  the parent component
     * @param title   the dialog title
     * @param message the info message
     */
    public static void showInfo(Component parent, String title, String message) {
        StyledDialog.showMessage(parent, title, message);
    }

    /**
     * Shows an input dialog with a text field.
     *
     * @param parent       the parent component
     * @param title        the dialog title
     * @param label        the label for the input field
     * @param defaultValue the default value in the input field
     * @return Optional containing the entered value, or empty if cancelled
     */
    public static Optional<String> showInput(Component parent, String title,
                                              String label, String defaultValue) {
        return showInput(parent, title, label, defaultValue, null);
    }

    /**
     * Shows an input dialog with a text field and tooltip.
     *
     * @param parent       the parent component
     * @param title        the dialog title
     * @param label        the label for the input field
     * @param defaultValue the default value in the input field
     * @param tooltip      optional tooltip for the input field
     * @return Optional containing the entered value, or empty if cancelled
     */
    public static Optional<String> showInput(Component parent, String title, String label,
                                              String defaultValue, String tooltip) {
        Window ownerWindow = getWindow(parent);
        final String[] result = {null};

        JDialog dialog = new JDialog(ownerWindow, title, Dialog.ModalityType.APPLICATION_MODAL);
        dialog.setUndecorated(true);
        dialog.setBackground(new Color(0, 0, 0, 0));

        JPanel glassPane = createGlassPane();
        if (ownerWindow instanceof JFrame jFrame) {
            jFrame.setGlassPane(glassPane);
            glassPane.setVisible(true);
        }

        JPanel mainPanel = createInputDialogPanel(title, label, defaultValue, tooltip,
                value -> {
                    result[0] = value;
                    glassPane.setVisible(false);
                    dialog.dispose();
                },
                () -> {
                    glassPane.setVisible(false);
                    dialog.dispose();
                });

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
            public void windowClosed(java.awt.event.WindowEvent e) {
                glassPane.setVisible(false);
            }
        });

        dialog.setContentPane(mainPanel);
        dialog.setSize(DIALOG_WIDTH, DIALOG_MIN_HEIGHT);
        dialog.setLocationRelativeTo(ownerWindow);
        dialog.setVisible(true);

        return Optional.ofNullable(result[0]);
    }

    /**
     * Shows an input dialog and calls a callback with the result.
     * This is useful for non-blocking dialog flows.
     *
     * @param parent       the parent component
     * @param title        the dialog title
     * @param label        the label for the input field
     * @param defaultValue the default value in the input field
     * @param onConfirm    callback invoked with the entered value when confirmed
     */
    public static void showInputAsync(Component parent, String title, String label,
                                       String defaultValue, Consumer<String> onConfirm) {
        showInputAsync(parent, title, label, defaultValue, null, onConfirm);
    }

    /**
     * Shows an input dialog with tooltip and calls a callback with the result.
     *
     * @param parent       the parent component
     * @param title        the dialog title
     * @param label        the label for the input field
     * @param defaultValue the default value in the input field
     * @param tooltip      optional tooltip for the input field
     * @param onConfirm    callback invoked with the entered value when confirmed
     */
    public static void showInputAsync(Component parent, String title, String label,
                                       String defaultValue, String tooltip,
                                       Consumer<String> onConfirm) {
        Window ownerWindow = getWindow(parent);

        JDialog dialog = new JDialog(ownerWindow, title, Dialog.ModalityType.APPLICATION_MODAL);
        dialog.setUndecorated(true);
        dialog.setBackground(new Color(0, 0, 0, 0));

        JPanel glassPane = createGlassPane();
        if (ownerWindow instanceof JFrame jFrame) {
            jFrame.setGlassPane(glassPane);
            glassPane.setVisible(true);
        }

        JPanel mainPanel = createInputDialogPanel(title, label, defaultValue, tooltip,
                value -> {
                    glassPane.setVisible(false);
                    dialog.dispose();
                    if (onConfirm != null && value != null && !value.isBlank()) {
                        onConfirm.accept(value);
                    }
                },
                () -> {
                    glassPane.setVisible(false);
                    dialog.dispose();
                });

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
            public void windowClosed(java.awt.event.WindowEvent e) {
                glassPane.setVisible(false);
            }
        });

        dialog.setContentPane(mainPanel);
        dialog.setSize(DIALOG_WIDTH, DIALOG_MIN_HEIGHT);
        dialog.setLocationRelativeTo(ownerWindow);
        dialog.setVisible(true);
    }

    private static JPanel createGlassPane() {
        JPanel glassPane = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                g.setColor(OVERLAY_COLOR);
                g.fillRect(0, 0, getWidth(), getHeight());
            }
        };
        glassPane.setOpaque(false);
        glassPane.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                e.consume();
            }
        });
        return glassPane;
    }

    private static JPanel createInputDialogPanel(String title, String label,
                                                  String defaultValue, String tooltip,
                                                  Consumer<String> onConfirm,
                                                  Runnable onCancel) {
        JPanel mainPanel = new JPanel(new BorderLayout(10, 15)) {
            @Override
            protected void paintComponent(Graphics g) {
                Graphics2D g2 = (Graphics2D) g.create();
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                g2.setColor(new Color(0, 0, 0, 100));
                g2.fill(new RoundRectangle2D.Float(6, 6, getWidth() - 6, getHeight() - 6, ARC, ARC));

                g2.setColor(ThemeManager.getInstance().getSurfacePrimary());
                g2.fill(new RoundRectangle2D.Float(0, 0, getWidth() - 6, getHeight() - 6, ARC, ARC));

                g2.setColor(GameTheme.Colors.ACCENT_GOLD);
                g2.setStroke(new BasicStroke(2f));
                g2.draw(new RoundRectangle2D.Float(1, 1, getWidth() - 8, getHeight() - 8, ARC, ARC));

                g2.dispose();
            }
        };
        mainPanel.setOpaque(false);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(PADDING, PADDING + 5, PADDING, PADDING + 5));

        JLabel titleLabel = new JLabel(title, SwingConstants.CENTER);
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
        JLabel fieldLabel = new JLabel(label);
        fieldLabel.setFont(FontManager.getBodyMedium());
        fieldLabel.setForeground(ThemeManager.getInstance().getTextPrimary());
        inputPanel.add(fieldLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 0.7;
        JTextField inputField = new JTextField(defaultValue != null ? defaultValue : "", 15);
        inputField.setFont(FontManager.getBodyMedium());
        inputField.setBackground(ThemeManager.getInstance().getSurfaceSecondary());
        inputField.setForeground(ThemeManager.getInstance().getTextPrimary());
        inputField.setCaretColor(ThemeManager.getInstance().getTextPrimary());
        inputField.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(GameTheme.Colors.ACCENT_COPPER, 1),
                BorderFactory.createEmptyBorder(8, 10, 8, 10)
        ));
        inputField.setPreferredSize(new Dimension(200, 40));
        if (tooltip != null && !tooltip.isBlank()) {
            StyledTooltipManager.setTooltip(inputField, label, tooltip);
        }
        inputPanel.add(inputField, gbc);

        mainPanel.add(inputPanel, BorderLayout.CENTER);

        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
        buttonPanel.setOpaque(false);

        StyledButton cancelButton = new StyledButton("Abbrechen", StyledButton.Style.SECONDARY);
        cancelButton.setPreferredSize(new Dimension(130, 42));
        cancelButton.addActionListener(e -> onCancel.run());

        StyledButton confirmButton = new StyledButton("Bestätigen", StyledButton.Style.PRIMARY);
        confirmButton.setPreferredSize(new Dimension(130, 42));
        confirmButton.addActionListener(e -> {
            String value = inputField.getText().trim();
            if (!value.isEmpty()) {
                onConfirm.accept(value);
            } else {
                inputField.requestFocus();
            }
        });

        buttonPanel.add(cancelButton);
        buttonPanel.add(confirmButton);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        inputField.addActionListener(e -> confirmButton.doClick());

        // Add keyboard navigation between buttons
        KeyListener buttonNavListener = new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                int keyCode = e.getKeyCode();
                if (keyCode == KeyEvent.VK_LEFT || keyCode == KeyEvent.VK_RIGHT ||
                    keyCode == KeyEvent.VK_TAB) {
                    e.consume();
                    Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                    if (focused == cancelButton) {
                        confirmButton.requestFocusInWindow();
                    } else if (focused == confirmButton) {
                        cancelButton.requestFocusInWindow();
                    }
                } else if (keyCode == KeyEvent.VK_UP) {
                    e.consume();
                    inputField.requestFocusInWindow();
                }
            }
        };
        cancelButton.addKeyListener(buttonNavListener);
        confirmButton.addKeyListener(buttonNavListener);

        // Add down arrow key to go from input field to buttons
        inputField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_DOWN) {
                    e.consume();
                    confirmButton.requestFocusInWindow();
                }
            }
        });

        SwingUtilities.invokeLater(() -> {
            inputField.requestFocusInWindow();
            inputField.selectAll();
        });

        return mainPanel;
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
