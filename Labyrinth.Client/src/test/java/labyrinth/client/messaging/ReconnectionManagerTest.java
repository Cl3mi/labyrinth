package labyrinth.client.messaging;

import labyrinth.client.models.LabyrinthApplication;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ReconnectionManagerTest {

    private ReconnectionManager reconnectionManager;

    @Mock
    private GameClient mockClient;

    @Mock
    private LabyrinthApplication mockApplication;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        reconnectionManager = new ReconnectionManager(mockClient, mockApplication);
    }

    @AfterEach
    void tearDown() {
        reconnectionManager.shutdown();
    }

    @Test
    void testConstructor() {
        assertNotNull(reconnectionManager);
    }

    @Test
    void testReset() {
        // Should not throw exception
        assertDoesNotThrow(() -> reconnectionManager.reset());
    }

    @Test
    void testCancelReconnection() {
        // Should not throw exception
        assertDoesNotThrow(() -> reconnectionManager.cancelReconnection());
    }

    @Test
    void testShutdown() {
        // Should not throw exception
        assertDoesNotThrow(() -> reconnectionManager.shutdown());
    }

    @Test
    void testCalculateDelay_Attempt1() throws Exception {
        // Use reflection to test private method
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        int delay = (int) method.invoke(reconnectionManager, 1);
        assertEquals(1000, delay); // 1 second
    }

    @Test
    void testCalculateDelay_Attempt2() throws Exception {
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        int delay = (int) method.invoke(reconnectionManager, 2);
        assertEquals(2000, delay); // 2 seconds
    }

    @Test
    void testCalculateDelay_Attempt3() throws Exception {
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        int delay = (int) method.invoke(reconnectionManager, 3);
        assertEquals(4000, delay); // 4 seconds
    }

    @Test
    void testCalculateDelay_Attempt4() throws Exception {
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        int delay = (int) method.invoke(reconnectionManager, 4);
        assertEquals(8000, delay); // 8 seconds
    }

    @Test
    void testCalculateDelay_Attempt5() throws Exception {
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        int delay = (int) method.invoke(reconnectionManager, 5);
        assertEquals(16000, delay); // 16 seconds (max)
    }

    @Test
    void testCalculateDelay_AttemptZero() throws Exception {
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        int delay = (int) method.invoke(reconnectionManager, 0);
        assertEquals(1000, delay); // Base delay
    }

    @Test
    void testCalculateDelay_AttemptNegative() throws Exception {
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        int delay = (int) method.invoke(reconnectionManager, -1);
        assertEquals(1000, delay); // Base delay
    }

    @Test
    void testCalculateDelay_MaxCap() throws Exception {
        Method method = ReconnectionManager.class.getDeclaredMethod("calculateDelay", int.class);
        method.setAccessible(true);

        // Very high attempt number should be capped at MAX_DELAY_MS (16000)
        int delay = (int) method.invoke(reconnectionManager, 10);
        assertEquals(16000, delay); // Capped at max
    }

    @Test
    void testStartAutoReconnect_DoesNotThrow() {
        // Basic test to ensure startAutoReconnect can be called without exception
        // Note: Full async behavior testing would require complex setup with mocked scheduler
        assertDoesNotThrow(() -> reconnectionManager.startAutoReconnect());

        // Give scheduler time to process
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    @Test
    void testMultipleCancelCalls() {
        // Should handle multiple cancel calls gracefully
        assertDoesNotThrow(() -> {
            reconnectionManager.cancelReconnection();
            reconnectionManager.cancelReconnection();
            reconnectionManager.cancelReconnection();
        });
    }

    @Test
    void testResetAfterStart() {
        // Start reconnection
        reconnectionManager.startAutoReconnect();

        // Reset should cancel and clear state
        assertDoesNotThrow(() -> reconnectionManager.reset());

        // Give scheduler time to process
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
