package labyrinth.server;


import labyrinth.contracts.models.*;
import labyrinth.server.exceptions.ActionErrorException;
import labyrinth.server.messaging.OutboundMessageSender;
import labyrinth.server.messaging.SessionManager;
import org.springframework.stereotype.Component;

@Component
public class GameEngine {

    public GameEngine(OutboundMessageSender sender, SessionManager sessionManager) {

    }


    public void handleConnectCommand(ConnectCommandPayload payload) throws ActionErrorException {

    }

    public void handleStartGameCommand(StartGameCommandPayload payload) throws ActionErrorException {

    }

    public void handlePushTileCommand(PushTileCommandPayload payload) throws ActionErrorException {

    }

    public void handleMovePawnCommand(MovePawnCommandPayload payload) throws ActionErrorException {

    }

    public void handleToggleAiCommand(ToggleAiCommandPayload payload) throws ActionErrorException {

    }

    public void handleUseBeamCommand(UseBeamCommandPayload payload) throws ActionErrorException {

    }

    public void handleUseSwapCommand(UseSwapCommandPayload payload) throws ActionErrorException {

    }

    public void handleUsePushFixedCommand(UsePushFixedCommandPayload payload) throws ActionErrorException {

    }

    public void handleUsePushTwiceCommand(UsePushTwiceCommandPayload payload) throws ActionErrorException {

    }



}
