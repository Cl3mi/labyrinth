package labyrinth.client.audio;

import javax.sound.sampled.*;
import java.io.IOException;
import java.net.URL;

public class AudioPlayer {

    private Clip clip;

    public AudioPlayer(String path) {
        try {
            URL url = getClass().getResource(path);
            if (url == null) {
                System.err.println("Audio file not found: " + path);
                return;
            }

            AudioInputStream audioStream = AudioSystem.getAudioInputStream(url);
            clip = AudioSystem.getClip();
            clip.open(audioStream);
            setVolume(0.8f);

        } catch (UnsupportedAudioFileException | IOException | LineUnavailableException e) {
            e.printStackTrace();
        }
    }

    /** Spielt den Sound einmal ab */
    public void play() {
        if (clip == null) return;
        clip.setFramePosition(0);
        clip.start();
    }

    /** Spielt die Musik in einer Endlosschleife */
    public void loop() {
        if (clip == null) return;
        clip.loop(Clip.LOOP_CONTINUOUSLY);
        clip.start();
    }

    /** Stoppt Musik */
    public void stop() {
        if (clip == null) return;
        clip.stop();
    }

    /** Lautstärke setzen (0.0 – 1.0) */
    public void setVolume(float volume) {
        if (clip == null) return;

        FloatControl gain = (FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN);
        float min = gain.getMinimum();
        float max = gain.getMaximum();

        float gainValue = min + (max - min) * volume;
        gain.setValue(gainValue);
    }
}
