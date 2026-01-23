package labyrinth.client;
import labyrinth.client.models.LabyrinthApplication;

public class ApplicationClient {
    static void main() throws Exception {
        startLabyrinth();
    }

    private static void startLabyrinth() throws Exception {
        LabyrinthApplication app = new LabyrinthApplication();
        app.start();
    }
}
