package labyrinth.client;
import labyrinth.client.models.LabyrinthApplication;


public class ApplicationClient {
    public static void main(String[] args) throws Exception {
        startOnline();
    }

    private static void startOnline() throws Exception {
        LabyrinthApplication app = new LabyrinthApplication();
        app.start();
    }

}
