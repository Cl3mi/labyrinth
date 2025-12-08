package labyrinth.server.messaging.commands.handler;

import labyrinth.contracts.models.CommandType;
import labyrinth.contracts.models.StartGameCommandPayload;
import labyrinth.server.messaging.commands.ICommandHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

@Component
public class StartGameCommandHandler implements ICommandHandler<StartGameCommandPayload> {

    // GAME LOGIK HIER

    @Override
    public CommandType type() {
        return CommandType.START_GAME;
    }

    @Override
    public void handle(WebSocketSession session,
                       StartGameCommandPayload payload) throws Exception {

        // TODO: Hier deine echte Spielstart-Logik integrieren
        System.out.println("START_GAME received: " +
                payload.getBoardSize().getRows() + "x" + payload.getBoardSize().getCols() +
                ", treasures=" + payload.getTreasureCardCount());

        // Beispiel – je nach Architektur:
        // gameService.startGameForSession(session, payload);
    }
}