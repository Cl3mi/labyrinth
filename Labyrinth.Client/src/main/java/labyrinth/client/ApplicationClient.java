package labyrinth.client;
import labyrinth.client.models.LabyrinthApplication;


public class ApplicationClient {
    public static void main(String[] args) throws Exception {
        // Initialize theme and fonts before creating/starting the UI
        labyrinth.client.ui.theme.ThemeApplier.initFontsAndResources();
        labyrinth.client.ui.theme.ThemeApplier.applyUIDefaults();

        startOnline();
    }

    private static void startOnline() throws Exception {
        LabyrinthApplication app = new LabyrinthApplication();
        app.start();
    }

}
