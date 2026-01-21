package labyrinth.client.ui.utils;

import labyrinth.client.ui.theme.ThemeManager;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Utility for loading and caching resources (Images, Fonts).
 */
public class ResourceLoader {

    private static final Map<String, Font> fontCache = new HashMap<>();
    private static final Map<String, BufferedImage> imageCache = new HashMap<>();

    // Specific caches
    private final Map<String, BufferedImage> treasureImages = new HashMap<>();
    private final Map<String, BufferedImage> tileImages = new HashMap<>();
    private final List<BufferedImage> playerIcons = new ArrayList<>();
    private BufferedImage bonusBagImage;
    private Image backgroundImage;

    // Singleton instance for the "game session" resources
    private static ResourceLoader instance;

    public static synchronized ResourceLoader getInstance() {
        if (instance == null) {
            instance = new ResourceLoader();
        }
        return instance;
    }

    private ResourceLoader() {
        loadResources();
    }

    private void loadResources() {
        loadTileImages();
        loadTreasureImages();
        loadPlayerIcons();
        loadBackgroundImage();
    }

    public void reloadThemeResources() {
        loadBackgroundImage();
    }

    // --- Font Caching ---

    public static Font getCachedFont(String family, int style, int size) {
        String key = family + "_" + style + "_" + size;
        return fontCache.computeIfAbsent(key, k -> new Font(family, style, size));
    }

    // --- Image Loading (Basics) ---

    private BufferedImage loadImage(String path) {
        if (imageCache.containsKey(path)) {
            return imageCache.get(path);
        }
        try {
            var url = getClass().getResource(path);
            if (url == null) {
                System.err.println("Image not found on classpath: " + path);
                return null;
            }
            BufferedImage img = ImageIO.read(url);
            imageCache.put(path, img);
            return img;
        } catch (Exception e) {
            System.err.println("Error loading image: " + path + " -> " + e.getMessage());
            return null;
        }
    }

    // --- Specific Loaders ---

    private void loadBackgroundImage() {
        try {
            String imagePath = ThemeManager.getInstance().getBackgroundImagePath();
            var url = getClass().getResource(imagePath);
            if (url != null) {
                backgroundImage = new ImageIcon(url).getImage();
                System.out.println("[ResourceLoader] Loaded background: " + imagePath);
            } else {
                System.err.println("Background image not found: " + imagePath);
            }
        } catch (Exception e) {
            System.err.println("Error loading background image: " + e.getMessage());
        }
    }

    private void loadTreasureImages() {
        // MAPPING: Deutsche Server-Namen -> Englische Dateinamen
        Map<String, String> treasureFileMapping = new HashMap<>();

        treasureFileMapping.put("Geist", "Ghost"); // id: 1
        treasureFileMapping.put("Drache", "Dragon"); // id: 2
        treasureFileMapping.put("Hexe", "Witch"); // id: 3
        treasureFileMapping.put("Eule", "Owl"); // id: 4
        treasureFileMapping.put("Ratte", "Rat"); // id: 5
        treasureFileMapping.put("Käfer", "Bug"); // id: 6
        treasureFileMapping.put("Spinne", "Spider"); // id: 7
        treasureFileMapping.put("Schlange", "Snake"); // id: 8
        treasureFileMapping.put("Fledermaus", "Bat"); // id: 9
        treasureFileMapping.put("Krone", "Crown"); // id: 10
        treasureFileMapping.put("Schlüssel", "Key"); // id: 11
        treasureFileMapping.put("Schatztruhe", "Treasure"); // id: 12
        treasureFileMapping.put("Helm", "Helmet"); // id: 13
        treasureFileMapping.put("Buch", "Book"); // id: 14
        treasureFileMapping.put("Kerze", "Candle"); // id: 15
        treasureFileMapping.put("Ring", "Ring"); // id: 16
        treasureFileMapping.put("Beutel", "Bag"); // id: 17
        treasureFileMapping.put("Totenkopf", "Skull"); // id: 18
        treasureFileMapping.put("Karte", "Map"); // id: 19
        treasureFileMapping.put("Schwert", "Sword"); // id: 20
        treasureFileMapping.put("Kelch", "chalice"); // id: 21
        treasureFileMapping.put("Edelstein", "Diamond"); // id: 22
        treasureFileMapping.put("Krug", "Jug"); // id: 23
        treasureFileMapping.put("Maus", "Mouse"); // id: 24

        // Load each treasure image
        for (Map.Entry<String, String> entry : treasureFileMapping.entrySet()) {
            String serverName = entry.getKey();
            String fileName = entry.getValue();

            BufferedImage img = loadImage("/images/tiles/" + fileName + ".png");

            if (img != null) {
                treasureImages.put(serverName, img);
            } else {
                System.err.println("❌ Failed to load treasure: " + serverName + " (file: " + fileName + ")");
            }
        }
    }

    private void loadTileImages() {
        tileImages.put("I", loadImage("/images/tiles/I_tile.png"));
        tileImages.put("L", loadImage("/images/tiles/L_tile.png"));
        tileImages.put("T", loadImage("/images/tiles/T_tile.png"));

        bonusBagImage = loadImage("/images/tiles/BonusBag.png");
    }

    private void loadPlayerIcons() {
        playerIcons.clear();
        for (int i = 1; i <= 4; i++) {
            BufferedImage img = loadImage("/images/players/player" + i + ".png");
            playerIcons.add(img);
        }
    }

    // --- Accessors ---

    public Image getBackgroundImage() {
        return backgroundImage;
    }

    public BufferedImage getTreasureImage(String name) {
        return treasureImages.get(name);
    }

    public BufferedImage getTileImage(String type) {
        return tileImages.get(type);
    }

    public BufferedImage getBonusBagImage() {
        return bonusBagImage;
    }

    public BufferedImage getPlayerIcon(int index) {
        if (index >= 0 && index < playerIcons.size()) {
            return playerIcons.get(index);
        }
        return null;
    }
}
