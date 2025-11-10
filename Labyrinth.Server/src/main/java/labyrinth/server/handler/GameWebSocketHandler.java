package labyrinth.server.handler;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.WebSocketMessage;
import org.springframework.web.reactive.socket.WebSocketSession;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

@Component
@RequiredArgsConstructor
@Slf4j
public class GameWebSocketHandler implements WebSocketHandler {

    private final ObjectMapper objectMapper;

    @NonNull
    @Override
    public Mono<Void> handle(WebSocketSession session) {
        log.info("Neuer Client verbunden: {}", session.getId());

        Mono<Void> input = session.receive()
                .map(WebSocketMessage::getPayloadAsText)
                .doOnNext(msg -> log.info("Nachricht von {}: {}", session.getId(), msg))
                .then();

        Mono<Void> output = session.send(Flux.never());

        return Mono.zip(input, output).then();
    }
}
