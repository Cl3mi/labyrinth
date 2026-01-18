package labyrinth.client.util;

import java.net.URI;

public class UriHelper {
    public static URI getGameUri(String serverAddress) {
        String uriString = serverAddress;
        if (!uriString.startsWith("ws://") && !uriString.startsWith("wss://")) {
            uriString = "ws://" + uriString;
        }
        if (!uriString.endsWith("/game")) {
            if (uriString.endsWith("/")) {
                uriString += "game";
            } else {
                uriString += "/game";
            }
        }
        return URI.create(uriString);
    }
}
