package labyrinth.server.messaging.events;

import labyrinth.server.messaging.events.abstractions.IEvent;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class EventPublisher  {

    private static final Logger log = LoggerFactory.getLogger(EventPublisher.class);
    private final ApplicationEventPublisher applicationEventPublisher;

    public void publish(IEvent IEvent) {
        try {
            applicationEventPublisher.publishEvent(IEvent);
        } catch (Exception ex) {
            log.error("Failed to publish event async: {}", IEvent, ex);
        }
    }
}

