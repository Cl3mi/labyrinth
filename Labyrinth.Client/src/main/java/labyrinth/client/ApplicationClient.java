package labyrinth.client;

import labyrinth.client.abstractions.IBoardFactory;
import labyrinth.client.abstractions.ITreasureCardFactory;
import labyrinth.client.factories.BoardFactory;
import labyrinth.client.factories.TreasureCardFactory;
import labyrinth.client.models.Game;
import labyrinth.client.models.LabyrinthApplication;
import labyrinth.client.models.Player;

public class ApplicationClient {
    public static void main(String[] args) throws Exception {
        startOnline();
    }

    private static void startOnline() throws Exception {
        LabyrinthApplication app = new LabyrinthApplication();
        app.start();
    }

}
