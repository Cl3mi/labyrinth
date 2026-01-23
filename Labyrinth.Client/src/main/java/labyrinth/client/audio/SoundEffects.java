package labyrinth.client.audio;

import lombok.Getter;

import javax.sound.sampled.*;

/**
 * Simple sound effects system for game actions.
 * Uses programmatically generated tones for feedback when sound files aren't available.
 */
@Getter
public class SoundEffects {

    private static final float SAMPLE_RATE = 8000f;

    private boolean enabled = true;
    private float volume = 0.7f;
    public void setVolume(float volume) {
        this.volume = Math.max(0f, Math.min(1f, volume));
    }

    /**
     * Play a sound effect for tile push action
     */
    public void playPush() {
        if (!enabled) return;
        playToneAsync(300, 120); // Lower tone, slightly longer duration
    }

    /**
     * Play a sound effect for pawn movement
     */
    public void playMove() {
        if (!enabled) return;
        playToneAsync(500, 80); // Mid tone, short duration
    }

    /**
     * Play a sound effect for tile rotation
     */
    public void playRotate() {
        if (!enabled) return;
        playToneAsync(700, 60); // Higher tone, very short duration
    }

    /**
     * Play a sound effect for UI interactions
     */
    public void playClick() {
        if (!enabled) return;
        playToneAsync(600, 40); // Quick click sound
    }

    /**
     * Play a sound effect for errors
     */
    public void playError() {
        if (!enabled) return;
        playToneAsync(200, 200); // Low, longer tone
    }

    /**
     * Play a sound effect for success/achievement
     */
    public void playSuccess() {
        if (!enabled) return;
        // Play ascending tones for success
        new Thread(() -> {
            playTone(523, 100); // C5
            try { Thread.sleep(50); } catch (InterruptedException ignored) {}
            playTone(659, 100); // E5
            try { Thread.sleep(50); } catch (InterruptedException ignored) {}
            playTone(784, 150); // G5
        }).start();
    }


    /**
     * Play a tone asynchronously (non-blocking)
     */
    private void playToneAsync(int frequency, int durationMs) {
        if (!enabled) return;
        new Thread(() -> playTone(frequency, durationMs)).start();
    }

    /**
     * Play a simple tone at the specified frequency and duration
     */
    private void playTone(int frequency, int durationMs) {
        try {
            byte[] buffer = new byte[(int) (SAMPLE_RATE * durationMs / 1000)];


            for (int i = 0; i < buffer.length; i++) {
                double angle = 2.0 * Math.PI * i * frequency / SAMPLE_RATE;
                buffer[i] = (byte) (Math.sin(angle) * 127 * volume); // Apply volume
            }


            AudioFormat audioFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, false);
            DataLine.Info info = new DataLine.Info(Clip.class, audioFormat);

            if (!AudioSystem.isLineSupported(info)) {
                java.awt.Toolkit.getDefaultToolkit().beep();
                return;
            }

            Clip clip = (Clip) AudioSystem.getLine(info);
            clip.open(audioFormat, buffer, 0, buffer.length);
            clip.start();
            
            clip.addLineListener(event -> {
                if (event.getType() == LineEvent.Type.STOP) {
                    clip.close();
                }
            });

        } catch (Exception e) {
            try {
                java.awt.Toolkit.getDefaultToolkit().beep();
            } catch (Exception ignored) {}
        }
    }
}