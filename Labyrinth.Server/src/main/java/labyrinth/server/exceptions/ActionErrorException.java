package labyrinth.server.exceptions;

import labyrinth.contracts.models.ErrorCode;
import lombok.Getter;

@Getter
public class ActionErrorException extends Exception {
    private final ErrorCode errorCode;

    public ActionErrorException(String message, ErrorCode errorCode) {
        super(message);
        this.errorCode = errorCode;
    }
}
