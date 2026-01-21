package labyrinth.client.models.extensions;

public class TreasureUtils {
    public static String getLocalName(int treasureId) {
        return switch (treasureId) {
            case 1 -> "Geist";
            case 2 -> "Drache";
            case 3 -> "Hexe";
            case 4 -> "Eule";
            case 5 -> "Ratte";
            case 6 -> "Käfer";
            case 7 -> "Spinne";
            case 8 -> "Schlange";
            case 9 -> "Fledermaus";
            case 10 -> "Krone";
            case 11 -> "Schlüssel";
            case 12 -> "Schatztruhe";
            case 13 -> "Helm";
            case 14 -> "Buch";
            case 15 -> "Kerze";
            case 16 -> "Ring";
            case 17 -> "Beutel";
            case 18 -> "Totenkopf";
            case 19 -> "Karte";
            case 20 -> "Schwert";
            case 21 -> "Kelch";
            case 22 -> "Edelstein";
            case 23 -> "Krug";
            case 24 -> "Maus";
            default -> "Unknown Treasure";
        };

    }
}
