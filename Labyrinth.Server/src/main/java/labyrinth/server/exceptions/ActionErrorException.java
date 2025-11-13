package labyrinth.server.exceptions;

import labyrinth.contracts.models.ErrorCode;
import lombok.Getter;

@Getter
public class ActionErrorException extends Exception {
    private ErrorCode errorCode;
}
