package labyrinth.client.enums;

/**
 * Enumeration of panel names used in CardLayout navigation.
 * Eliminates magic strings for panel identification.
 */
public enum PanelName {
    MAIN_MENU("mainmenu"),
    SERVER_BROWSER("serverbrowser"),
    LOBBY("lobby"),
    BOARD("game"),
    OPTIONS("options"),
    GAME_OVER("gameover");

    private final String cardName;

    PanelName(String cardName) {
        this.cardName = cardName;
    }

    /**
     * Gets the CardLayout identifier for this panel.
     *
     * @return the card name string
     */
    public String getCardName() {
        return cardName;
    }
}
