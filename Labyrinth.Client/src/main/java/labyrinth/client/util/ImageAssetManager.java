package labyrinth.client.util;

import labyrinth.client.ui.theme.ThemeManager;
import labyrinth.client.util.Logger;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Centralized manager for loading and caching image assets.
 * Thread-safe singleton that provides efficient image loading with caching.
 */
public final class ImageAssetManager {

    private static final Logger log = Logger.getLogger(ImageAssetManager.class);
    private static final ImageAssetManager INSTANCE = new ImageAssetManager();

    private final Map<String, Image> backgroundCache = new ConcurrentHashMap<>();
    private final Map<Integer, BufferedImage> treasureCache = new ConcurrentHashMap<>();
    private final Map<String, BufferedImage> tileCache = new ConcurrentHashMap<>();
    private final Map<Integer, BufferedImage> playerIconCache = new ConcurrentHashMap<>();
    private final Map<String, Image> miscCache = new ConcurrentHashMap<>();

    private static final Map<Integer, String> TREASURE_FILE_MAPPING = Map.ofEntries(
            Map.entry(1, "Ghost"),
            Map.entry(2, "Dragon"),
            Map.entry(3, "Witch"),
            Map.entry(4, "Owl"),
            Map.entry(5, "Rat"),
            Map.entry(6, "Bug"),
            Map.entry(7, "Spider"),
            Map.entry(8, "Snake"),
            Map.entry(9, "Bat"),
            Map.entry(10, "Crown"),
            Map.entry(11, "Key"),
            Map.entry(12, "Treasure"),
            Map.entry(13, "Helmet"),
            Map.entry(14, "Book"),
            Map.entry(15, "Candle"),
            Map.entry(16, "Ring"),
            Map.entry(17, "Bag"),
            Map.entry(18, "Skull"),
            Map.entry(19, "Map"),
            Map.entry(20, "Sword"),
            Map.entry(21, "chalice"),
            Map.entry(22, "Diamond"),
            Map.entry(23, "Jug"),
            Map.entry(24, "Mouse")
    );

    private ImageAssetManager() {
        ThemeManager.getInstance().addThemeChangeListener(this::clearBackgroundCache);
    }

    public static ImageAssetManager getInstance() {
        return INSTANCE;
    }

    /**
     * Gets the current theme's background image.
     *
     * @return the background image, or null if not found
     */
    public Image getBackgroundImage() {
        String imagePath = ThemeManager.getInstance().getBackgroundImagePath();
        return backgroundCache.computeIfAbsent(imagePath, this::loadImage);
    }

    /**
     * Gets a treasure image by ID.
     *
     * @param treasureId the treasure ID (1-24)
     * @return the treasure image, or null if not found
     */
    public BufferedImage getTreasureImage(int treasureId) {
        return treasureCache.computeIfAbsent(treasureId, id -> {
            String fileName = TREASURE_FILE_MAPPING.get(id);
            if (fileName == null) {
                return null;
            }
            return loadBufferedImage("/images/tiles/" + fileName + ".png");
        });
    }

    /**
     * Gets a tile image by type.
     *
     * @param tileType the tile type (I, L, T)
     * @return the tile image, or null if not found
     */
    public BufferedImage getTileImage(String tileType) {
        return tileCache.computeIfAbsent(tileType,
                type -> loadBufferedImage("/images/tiles/" + type + "_tile.png"));
    }

    /**
     * Gets a player icon by index.
     *
     * @param playerIndex the player index (0-3)
     * @return the player icon, or null if not found
     */
    public BufferedImage getPlayerIcon(int playerIndex) {
        return playerIconCache.computeIfAbsent(playerIndex,
                idx -> loadBufferedImage("/images/players/player" + (idx + 1) + ".png"));
    }

    /**
     * Gets the logo image.
     *
     * @return the logo image, or null if not found
     */
    public Image getLogoImage() {
        return miscCache.computeIfAbsent("logo", k -> loadImage("/images/ui/logo.png"));
    }

    /**
     * Gets the application icon.
     *
     * @return the icon image, or null if not found
     */
    public Image getAppIcon() {
        return miscCache.computeIfAbsent("icon", k -> loadImage("/images/ui/icon.png"));
    }

    /**
     * Gets the bonus bag image.
     *
     * @return the bonus bag image, or null if not found
     */
    public BufferedImage getBonusBagImage() {
        return (BufferedImage) miscCache.computeIfAbsent("bonusBag",
                k -> loadBufferedImage("/images/tiles/BonusBag.png"));
    }

    /**
     * Preloads all images in background thread for faster access later.
     */
    public void preloadAll() {
        Thread preloadThread = new Thread(() -> {
            getBackgroundImage();
            getLogoImage();
            getAppIcon();
            getBonusBagImage();

            for (int i = 1; i <= 24; i++) {
                getTreasureImage(i);
            }

            for (String type : new String[]{"I", "L", "T"}) {
                getTileImage(type);
            }

            for (int i = 0; i < 4; i++) {
                getPlayerIcon(i);
            }

            log.info("[ImageAssetManager] Preloaded all images");
        }, "ImagePreloader");
        preloadThread.setDaemon(true);
        preloadThread.start();
    }

    /**
     * Clears the background cache (called on theme change).
     */
    public void clearBackgroundCache() {
        backgroundCache.clear();
    }

    /**
     * Clears all caches.
     */
    public void clearAllCaches() {
        backgroundCache.clear();
        treasureCache.clear();
        tileCache.clear();
        playerIconCache.clear();
        miscCache.clear();
    }

    private Image loadImage(String path) {
        try {
            var url = getClass().getResource(path);
            if (url != null) {
                return new ImageIcon(url).getImage();
            } else {
                log.error("[ImageAssetManager] Image not found: " + path);
                return null;
            }
        } catch (Exception e) {
            log.error("[ImageAssetManager] Error loading image: " + path + " -> " + e.getMessage());
            return null;
        }
    }

    private BufferedImage loadBufferedImage(String path) {
        try {
            var url = getClass().getResource(path);
            if (url == null) {
                log.error("[ImageAssetManager] Image not found: " + path);
                return null;
            }
            return ImageIO.read(url);
        } catch (Exception e) {
            log.error("[ImageAssetManager] Error loading image: " + path + " -> " + e.getMessage());
            return null;
        }
    }
}
