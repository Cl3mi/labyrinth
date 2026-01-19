package labyrinth.client.ui;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Toast notification system for displaying temporary messages.
 * Supports INFO, SUCCESS, WARNING, ERROR types with auto-dismiss.
 */
public class ToastManager {

    public enum ToastType {
        INFO("I", new Color(52, 152, 219), "[i]"),      // Blue - ASCII fallback
        SUCCESS("S", new Color(46, 204, 113), "[OK]"),  // Green - ASCII fallback
        WARNING("W", new Color(241, 196, 15), "[!]"),   // Yellow - ASCII fallback
        ERROR("E", new Color(231, 76, 60), "[X]");      // Red - ASCII fallback

        public final String prefix;
        public final Color color;
        public final String icon;

        ToastType(String prefix, Color color, String icon) {
            this.prefix = prefix;
            this.color = color;
            this.icon = icon;
        }
    }

    private static class Toast {
        final String id;
        final String title;
        final String message;
        final ToastType type;
        final long createdAt;
        final int duration;
        JPanel panel;

        Toast(String id, String title, String message, ToastType type, int duration) {
            this.id = id;
            this.title = title;
            this.message = message;
            this.type = type;
            this.duration = duration;
            this.createdAt = System.currentTimeMillis();
        }

        boolean isExpired() {
            return System.currentTimeMillis() - createdAt > duration;
        }
    }

    private final JPanel parentPanel;
    private final List<Toast> activeToasts = new ArrayList<>();
    private final Timer updateTimer;

    private static final int TOAST_WIDTH = 320;
    private static final int TOAST_HEIGHT = 90;
    private static final int TOAST_SPACING = 10;
    private static final int TOAST_MARGIN = 20;

    public ToastManager(JPanel parentPanel) {
        this.parentPanel = parentPanel;

        // Update timer to refresh toast positions and remove expired ones
        updateTimer = new Timer(100, e -> updateToasts());
        updateTimer.start();
    }

    /**
     * Show an info toast
     */
    public void showInfo(String id, String title, String message) {
        show(id, title, message, ToastType.INFO, 4000);
    }

    /**
     * Show a success toast
     */
    public void showSuccess(String id, String title, String message) {
        show(id, title, message, ToastType.SUCCESS, 3000);
    }

    /**
     * Show a warning toast
     */
    public void showWarning(String id, String title, String message) {
        show(id, title, message, ToastType.WARNING, 5000);
    }

    /**
     * Show an error toast
     */
    public void showError(String id, String title, String message) {
        show(id, title, message, ToastType.ERROR, 5000);
    }

    /**
     * Show a toast with custom duration
     */
    public void show(String id, String title, String message, ToastType type, int duration) {
        SwingUtilities.invokeLater(() -> {
            Toast toast = new Toast(id, title, message, type, duration);
            toast.panel = createToastPanel(toast);

            activeToasts.add(toast);
            parentPanel.add(toast.panel);
            parentPanel.setComponentZOrder(toast.panel, 0); // Bring to front

            updateToastPositions();
            parentPanel.revalidate();
            parentPanel.repaint();
        });
    }

    private JPanel createToastPanel(Toast toast) {
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout(10, 5));
        panel.setBackground(new Color(40, 40, 50, 240));
        panel.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(toast.type.color, 2),
            BorderFactory.createEmptyBorder(12, 15, 12, 15)
        ));
        panel.setSize(TOAST_WIDTH, TOAST_HEIGHT);

        // Left: Icon and ID
        JPanel leftPanel = new JPanel(new BorderLayout(5, 0));
        leftPanel.setOpaque(false);

        JLabel iconLabel = new JLabel(toast.type.icon);
        iconLabel.setFont(new Font("SansSerif", Font.BOLD, 16));
        iconLabel.setForeground(toast.type.color);
        leftPanel.add(iconLabel, BorderLayout.WEST);

        JLabel idLabel = new JLabel(toast.type.prefix + "-" + toast.id);
        idLabel.setFont(new Font("Monospaced", Font.PLAIN, 5));
        idLabel.setForeground(new Color(150, 150, 170));
        leftPanel.add(idLabel, BorderLayout.SOUTH);

        panel.add(leftPanel, BorderLayout.WEST);

        // Center: Title and Message
        JPanel centerPanel = new JPanel(new BorderLayout(0, 3));
        centerPanel.setOpaque(false);

        JLabel titleLabel = new JLabel(toast.title);
        titleLabel.setFont(new Font("Arial", Font.BOLD, 14));
        titleLabel.setForeground(Color.WHITE);
        centerPanel.add(titleLabel, BorderLayout.NORTH);

        JLabel messageLabel = new JLabel("<html><body style='width: 280px'>" + toast.message + "</body></html>");
        messageLabel.setFont(new Font("SansSerif", Font.PLAIN, 12));
        messageLabel.setForeground(new Color(200, 200, 220));
        centerPanel.add(messageLabel, BorderLayout.CENTER);

        panel.add(centerPanel, BorderLayout.CENTER);

        // Make it clickable to dismiss
        panel.setCursor(new Cursor(Cursor.HAND_CURSOR));
        panel.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent e) {
                dismissToast(toast);
            }
        });

        return panel;
    }

    private void updateToasts() {
        boolean needsUpdate = false;

        // Remove expired toasts
        List<Toast> toRemove = new ArrayList<>();
        for (Toast toast : activeToasts) {
            if (toast.isExpired()) {
                toRemove.add(toast);
                needsUpdate = true;
            }
        }

        for (Toast toast : toRemove) {
            dismissToast(toast);
        }

        if (needsUpdate) {
            updateToastPositions();
        }
    }

    private void dismissToast(Toast toast) {
        SwingUtilities.invokeLater(() -> {
            if (toast.panel != null) {
                parentPanel.remove(toast.panel);
            }
            activeToasts.remove(toast);
            updateToastPositions();
            parentPanel.revalidate();
            parentPanel.repaint();
        });
    }

    private void updateToastPositions() {
        int yPosition = TOAST_MARGIN; // Start von oben

        // Stack from top to bottom
        for (int i = 0; i < activeToasts.size(); i++) {
            Toast toast = activeToasts.get(i);
            if (toast.panel != null) {
                int x = parentPanel.getWidth() - TOAST_WIDTH - TOAST_MARGIN;
                int y = yPosition;

                toast.panel.setBounds(x, y, TOAST_WIDTH, TOAST_HEIGHT);
                yPosition += (TOAST_HEIGHT + TOAST_SPACING);
            }
        }
    }



    /**
     * Clear all active toasts
     */
    public void clearAll() {
        SwingUtilities.invokeLater(() -> {
            for (Toast toast : new ArrayList<>(activeToasts)) {
                dismissToast(toast);
            }
        });
    }

    /**
     * Stop the toast manager
     */
    public void dispose() {
        updateTimer.stop();
        clearAll();
    }
}
