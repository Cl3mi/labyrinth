package labyrinth.client.ui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.List;
import java.util.function.Consumer;

/**
 * Utility class for adding keyboard navigation to panels.
 * Supports arrow keys for movement and Enter/Space for confirmation.
 */
public class KeyboardNavigationHelper {

    /**
     * Sets up vertical keyboard navigation for a list of components.
     * Arrow up/down moves focus between components.
     * Enter/Space activates buttons.
     *
     * @param panel      The panel to add keyboard navigation to
     * @param components The list of focusable components in order
     */
    public static void setupVerticalNavigation(JPanel panel, List<? extends Component> components) {
        setupNavigation(panel, components, true);
    }

    /**
     * Sets up horizontal keyboard navigation for a list of components.
     * Arrow left/right moves focus between components.
     * Enter/Space activates buttons.
     *
     * @param panel      The panel to add keyboard navigation to
     * @param components The list of focusable components in order
     */
    public static void setupHorizontalNavigation(JPanel panel, List<? extends Component> components) {
        setupNavigation(panel, components, false);
    }

    /**
     * Sets up keyboard navigation for a list of components.
     *
     * @param panel      The panel to add keyboard navigation to
     * @param components The list of focusable components in order
     * @param vertical   If true, uses up/down arrows; if false, uses left/right arrows
     */
    public static void setupNavigation(JPanel panel, List<? extends Component> components, boolean vertical) {
        if (components == null || components.isEmpty()) {
            return;
        }

        // Make all components focusable and disable default Tab traversal
        for (Component comp : components) {
            if (comp instanceof JComponent jComp) {
                jComp.setFocusable(true);
                jComp.setFocusTraversalKeysEnabled(false);
            }
        }

        // Add key listener to the panel
        KeyListener navListener = new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                int currentIndex = -1;

                for (int i = 0; i < components.size(); i++) {
                    if (components.get(i) == focused || isDescendant(components.get(i), focused)) {
                        currentIndex = i;
                        break;
                    }
                }

                int keyCode = e.getKeyCode();
                boolean isNext = vertical ? (keyCode == KeyEvent.VK_DOWN || keyCode == KeyEvent.VK_S)
                        : (keyCode == KeyEvent.VK_RIGHT || keyCode == KeyEvent.VK_D);
                boolean isPrev = vertical ? (keyCode == KeyEvent.VK_UP || keyCode == KeyEvent.VK_W)
                        : (keyCode == KeyEvent.VK_LEFT || keyCode == KeyEvent.VK_A);

                // Tab key wrapping
                if (keyCode == KeyEvent.VK_TAB) {
                    e.consume();
                    if (e.isShiftDown()) {
                        int prevIndex = currentIndex < 0 ? components.size() - 1 : (currentIndex - 1 + components.size()) % components.size();
                        requestFocusFor(components.get(prevIndex));
                    } else {
                        int nextIndex = currentIndex < 0 ? 0 : (currentIndex + 1) % components.size();
                        requestFocusFor(components.get(nextIndex));
                    }
                    return;
                }

                if (isNext) {
                    e.consume();
                    int nextIndex = currentIndex < 0 ? 0 : (currentIndex + 1) % components.size();
                    Component next = components.get(nextIndex);
                    requestFocusFor(next);
                } else if (isPrev) {
                    e.consume();
                    int prevIndex = currentIndex < 0 ? components.size() - 1 : (currentIndex - 1 + components.size()) % components.size();
                    Component prev = components.get(prevIndex);
                    requestFocusFor(prev);
                } else if (keyCode == KeyEvent.VK_ENTER || keyCode == KeyEvent.VK_SPACE) {
                    if (focused instanceof AbstractButton button) {
                        e.consume();
                        button.doClick();
                    }
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
            }
        };

        panel.addKeyListener(navListener);
        panel.setFocusable(true);
        panel.setFocusTraversalKeysEnabled(false);

        // Also add listener to each component for when they have focus
        for (Component comp : components) {
            comp.addKeyListener(navListener);
        }
    }

    /**
     * Sets up grid-based keyboard navigation for components arranged in a grid.
     *
     * @param panel      The panel to add keyboard navigation to
     * @param components 2D list of components (row-major order)
     */
    public static void setupGridNavigation(JPanel panel, List<List<? extends Component>> components) {
        if (components == null || components.isEmpty()) {
            return;
        }

        KeyListener navListener = new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                int currentRow = -1;
                int currentCol = -1;

                // Find current position
                outer:
                for (int row = 0; row < components.size(); row++) {
                    List<? extends Component> rowComponents = components.get(row);
                    for (int col = 0; col < rowComponents.size(); col++) {
                        Component comp = rowComponents.get(col);
                        if (comp == focused || isDescendant(comp, focused)) {
                            currentRow = row;
                            currentCol = col;
                            break outer;
                        }
                    }
                }

                if (currentRow < 0) {
                    currentRow = 0;
                    currentCol = 0;
                }

                int keyCode = e.getKeyCode();
                int newRow = currentRow;
                int newCol = currentCol;

                switch (keyCode) {
                    case KeyEvent.VK_DOWN, KeyEvent.VK_S -> newRow = (currentRow + 1) % components.size();
                    case KeyEvent.VK_UP, KeyEvent.VK_W -> newRow = (currentRow - 1 + components.size()) % components.size();
                    case KeyEvent.VK_RIGHT, KeyEvent.VK_D -> {
                        List<? extends Component> row = components.get(currentRow);
                        newCol = (currentCol + 1) % row.size();
                    }
                    case KeyEvent.VK_LEFT, KeyEvent.VK_A -> {
                        List<? extends Component> row = components.get(currentRow);
                        newCol = (currentCol - 1 + row.size()) % row.size();
                    }
                    case KeyEvent.VK_ENTER, KeyEvent.VK_SPACE -> {
                        if (focused instanceof AbstractButton button) {
                            e.consume();
                            button.doClick();
                        }
                        return;
                    }
                    default -> {
                        return;
                    }
                }

                e.consume();

                // Ensure valid position
                if (newRow >= 0 && newRow < components.size()) {
                    List<? extends Component> row = components.get(newRow);
                    if (newCol >= row.size()) {
                        newCol = row.size() - 1;
                    }
                    if (newCol >= 0 && newCol < row.size()) {
                        requestFocusFor(row.get(newCol));
                    }
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
            }
        };

        panel.addKeyListener(navListener);
        panel.setFocusable(true);

        for (List<? extends Component> row : components) {
            for (Component comp : row) {
                if (comp instanceof JComponent jComp) {
                    jComp.setFocusable(true);
                }
                comp.addKeyListener(navListener);
            }
        }
    }

    /**
     * Sets up keyboard navigation for a JList component.
     * Arrow keys navigate items, Enter/Space selects.
     *
     * @param list       The JList to add navigation to
     * @param onSelect   Callback when an item is selected with Enter/Space
     */
    public static <T> void setupListNavigation(JList<T> list, Consumer<T> onSelect) {
        list.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                int keyCode = e.getKeyCode();

                if (keyCode == KeyEvent.VK_ENTER || keyCode == KeyEvent.VK_SPACE) {
                    e.consume();
                    T selected = list.getSelectedValue();
                    if (selected != null && onSelect != null) {
                        onSelect.accept(selected);
                    }
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
            }
        });

        list.setFocusable(true);
    }

    /**
     * Requests focus for a component, handling special cases.
     */
    public static void requestFocusFor(Component comp) {
        if (comp == null) return;

        if (comp instanceof JComponent jComp) {
            jComp.requestFocusInWindow();
        } else {
            comp.requestFocus();
        }
    }

    /**
     * Focuses the first focusable component in the list.
     */
    public static void focusFirst(List<? extends Component> components) {
        if (components != null && !components.isEmpty()) {
            SwingUtilities.invokeLater(() -> requestFocusFor(components.get(0)));
        }
    }

    /**
     * Checks if 'descendant' is a descendant of 'ancestor'.
     */
    private static boolean isDescendant(Component ancestor, Component descendant) {
        if (ancestor == null || descendant == null) {
            return false;
        }
        Component parent = descendant;
        while (parent != null) {
            if (parent == ancestor) {
                return true;
            }
            parent = parent.getParent();
        }
        return false;
    }

    /**
     * Sets up Escape key to trigger a back action.
     */
    public static void setupEscapeAction(JPanel panel, Runnable onEscape) {
        panel.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
                .put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "escape");
        panel.getActionMap().put("escape", new AbstractAction() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent e) {
                if (onEscape != null) {
                    onEscape.run();
                }
            }
        });
    }

    /**
     * Creates a focus traversal policy based on a list of components.
     */
    public static FocusTraversalPolicy createFocusPolicy(List<? extends Component> components) {
        return new FocusTraversalPolicy() {
            @Override
            public Component getComponentAfter(Container container, Component component) {
                int index = indexOf(component);
                if (index < 0) return getFirstComponent(container);
                return components.get((index + 1) % components.size());
            }

            @Override
            public Component getComponentBefore(Container container, Component component) {
                int index = indexOf(component);
                if (index < 0) return getLastComponent(container);
                return components.get((index - 1 + components.size()) % components.size());
            }

            @Override
            public Component getFirstComponent(Container container) {
                return components.isEmpty() ? null : components.get(0);
            }

            @Override
            public Component getLastComponent(Container container) {
                return components.isEmpty() ? null : components.get(components.size() - 1);
            }

            @Override
            public Component getDefaultComponent(Container container) {
                return getFirstComponent(container);
            }

            private int indexOf(Component comp) {
                for (int i = 0; i < components.size(); i++) {
                    if (components.get(i) == comp || isDescendant(components.get(i), comp)) {
                        return i;
                    }
                }
                return -1;
            }
        };
    }
}
