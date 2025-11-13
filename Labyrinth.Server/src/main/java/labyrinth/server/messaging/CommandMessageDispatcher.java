package labyrinth.server.messaging;

import labyrinth.contracts.models.*;
import labyrinth.server.GameEngine;
import labyrinth.server.exceptions.ActionErrorException;
import org.springframework.stereotype.Component;

@Component
public class CommandMessageDispatcher {

    private final GameEngine gameEngine;

    public CommandMessageDispatcher(GameEngine gameEngine) {
        this.gameEngine = gameEngine;
    }

    public void dispatch(CommandType commandType, Object payload) throws ActionErrorException {
        switch (commandType) {
            case CONNECT -> gameEngine.handleConnectCommand((ConnectCommandPayload) payload);
            case START_GAME -> gameEngine.handleStartGameCommand((StartGameCommandPayload) payload);
            case PUSH_TILE -> gameEngine.handlePushTileCommand((PushTileCommandPayload) payload);
            case MOVE_PAWN -> gameEngine.handleMovePawnCommand((MovePawnCommandPayload) payload);
            case TOGGLE_AI -> gameEngine.handleToggleAiCommand((ToggleAiCommandPayload) payload);
            case USE_BEAM -> gameEngine.handleUseBeamCommand((UseBeamCommandPayload) payload);
            case USE_SWAP -> gameEngine.handleUseSwapCommand((UseSwapCommandPayload) payload);
            case USE_PUSH_FIXED -> gameEngine.handleUsePushFixedCommand((UsePushFixedCommandPayload) payload);
            case USE_PUSH_TWICE -> gameEngine.handleUsePushTwiceCommand((UsePushTwiceCommandPayload) payload);
        }
    }
}
