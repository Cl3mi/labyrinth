package labyrinth.client.audio;

import javax.sound.sampled.*;
import java.io.BufferedInputStream;
import java.net.URL;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Zentraler Audio-Manager (Singleton) für Musik & Soundeffekte.
 *
 * Anforderungen:
 * - Globale Lautstärken (Music/SFX)
 * - Hintergrundmusik im MainMenu (06-Kokiri-Forest)
 * - Beim GameOver: Applaus einmal abspielen, danach wieder Kokiri Forest
 * - Alle Audio-Logik ist in dieser Klasse gekapselt
 */
public final class AudioPlayer {

    public static final String MENU_MUSIC_PATH = "/sounds/06-Kokiri-Forest.wav";
    public static final String GAMEOVER_APPLAUSE_PATH = "/sounds/Applause.wav";

    private static AudioPlayer instance;

    // Background music
    private Clip musicClip;
    private String currentMusicPath;
    private boolean currentMusicLoop;

    // Global volumes (0..1)
    private float musicVolume = 1.0f;
    private float sfxVolume = 1.0f;

    // Used to cancel delayed resumes (e.g., if stopAll() is called while a SFX is playing)
    private final AtomicLong resumeToken = new AtomicLong(0);

    private AudioPlayer() {}

    public static synchronized AudioPlayer getInstance() {
        if (instance == null) instance = new AudioPlayer();
        return instance;
    }

    // ========================= HIGH LEVEL =========================

    /** Starts the menu background music (Kokiri Forest) in a loop. */
    public void playMenuMusic() {
        playMusic(MENU_MUSIC_PATH, true);
    }

    /**
     * GameOver behaviour:
     * - stop current music (if any)
     * - play applause once
     * - after applause finishes, start menu music (Kokiri Forest) looping
     */
    public void playGameOverSequence() {
        final long token = resumeToken.incrementAndGet();

        // Always stop music for a clean applause moment.
        stopMusic();

        playSfxOnceInternal(GAMEOVER_APPLAUSE_PATH, () -> {
            // Only resume if nothing newer cancelled/overrode this sequence.
            if (resumeToken.get() != token) return;
            playMenuMusic();
        });
    }

    // ========================= MUSIC =========================

    public synchronized void playMusic(String path, boolean loop) {
        System.out.println("[AudioPlayer] playMusic called: " + path + ", loop=" + loop);

        // No-op if already playing the same track with same loop mode.
        if (musicClip != null && musicClip.isOpen()
                && path != null && path.equals(currentMusicPath)
                && loop == currentMusicLoop) {
            // Ensure it is actually running (could have been stopped externally)
            if (!musicClip.isRunning()) {
                System.out.println("[AudioPlayer] Resuming existing clip");
                if (loop) musicClip.loop(Clip.LOOP_CONTINUOUSLY);
                else musicClip.start();
            } else {
                System.out.println("[AudioPlayer] Already playing, skipping");
            }
            return;
        }

        stopMusic();

        currentMusicPath = path;
        currentMusicLoop = loop;

        try {
            URL url = getClass().getResource(path);
            if (url == null) {
                System.err.println("[AudioPlayer] Music not found: " + path);
                return;
            }
            System.out.println("[AudioPlayer] Loading music from: " + url);

            AudioInputStream in = AudioSystem.getAudioInputStream(new BufferedInputStream(url.openStream()));
            AudioInputStream pcm = toPlayablePcm(in);

            musicClip = AudioSystem.getClip();
            musicClip.open(pcm);

            System.out.println("[AudioPlayer] Setting volume to: " + musicVolume);
            applyVolume(musicClip, musicVolume);
            unmute(musicClip);

            if (loop) {
                System.out.println("[AudioPlayer] Starting loop playback");
                musicClip.loop(Clip.LOOP_CONTINUOUSLY);
            } else {
                System.out.println("[AudioPlayer] Starting single playback");
                musicClip.start();
            }

            System.out.println("[AudioPlayer] Music started successfully!");

        } catch (Exception e) {
            System.err.println("[AudioPlayer] Error playing music: " + e.getMessage());
            e.printStackTrace();
        }
    }

    public synchronized void stopMusic() {
        if (musicClip != null) {
            try { musicClip.stop(); } catch (Exception ignored) {}
            try { musicClip.close(); } catch (Exception ignored) {}
            musicClip = null;
        }
        // Reset path so playMusic will reload even the same track
        currentMusicPath = null;
        currentMusicLoop = false;
        System.out.println("[AudioPlayer] Music stopped");
    }

    /** Stops music and cancels any pending "resume after SFX" operations. */
    public void stopAll() {
        resumeToken.incrementAndGet();
        stopMusic();
        // Note: SFX clips are short-lived and self-closing; they are not tracked globally here.
    }

    public synchronized void setMusicVolume(float v) {
        musicVolume = clamp(v);
        applyVolume(musicClip, musicVolume);
    }

    public synchronized float getMusicVolume() {
        return musicVolume;
    }

    // ========================= SFX =========================

    public synchronized void setSfxVolume(float v) {
        sfxVolume = clamp(v);
    }

    public synchronized float getSfxVolume() {
        return sfxVolume;
    }

    public void playSfxOnce(String path) {
        playSfxOnceInternal(path, null);
    }

    private void playSfxOnceInternal(String path, Runnable onFinished) {
        new Thread(() -> {
            Clip c = null;
            try {
                URL url = getClass().getResource(path);
                if (url == null) {
                    System.err.println("SFX not found: " + path);
                    return;
                }

                AudioInputStream in = AudioSystem.getAudioInputStream(new BufferedInputStream(url.openStream()));
                AudioInputStream pcm = toPlayablePcm(in);

                c = AudioSystem.getClip();
                c.open(pcm);

                final Clip clipRef = c;
                applyVolume(clipRef, sfxVolume);
                unmute(clipRef);

                // Close + callback after the sound finished.
                c.addLineListener(ev -> {
                    if (ev.getType() == LineEvent.Type.STOP) {
                        try { clipRef.close(); } catch (Exception ignored) {}
                        if (onFinished != null) {
                            try { onFinished.run(); } catch (Exception ignored) {}
                        }
                    }
                });

                c.start();
            } catch (Exception e) {
                if (c != null) {
                    try { c.close(); } catch (Exception ignored) {}
                }
                e.printStackTrace();
            }
        }, "SfxThread").start();
    }

    // ========================= INTERNAL =========================

    private AudioInputStream toPlayablePcm(AudioInputStream in) {
        AudioFormat base = in.getFormat();
        AudioFormat target = new AudioFormat(
                AudioFormat.Encoding.PCM_SIGNED,
                base.getSampleRate(),
                16,
                base.getChannels(),
                base.getChannels() * 2,
                base.getSampleRate(),
                false
        );

        if (AudioSystem.isConversionSupported(target, base)) {
            return AudioSystem.getAudioInputStream(target, in);
        }
        return in;
    }

    private void applyVolume(Clip clip, float volume) {
        if (clip == null) return;
        float v = clamp(volume);

        // MASTER_GAIN is the most widely supported control in Java Sound.
        if (clip.isControlSupported(FloatControl.Type.MASTER_GAIN)) {
            FloatControl g = (FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN);

            // Convert linear volume (0..1) to dB. 1.0 => 0 dB.
            if (v == 0f) {
                g.setValue(g.getMinimum());
            } else {
                float db = (float) (20.0 * Math.log10(v));
                db = Math.max(g.getMinimum(), Math.min(g.getMaximum(), db));
                g.setValue(db);
            }
        } else if (clip.isControlSupported(FloatControl.Type.VOLUME)) {
            ((FloatControl) clip.getControl(FloatControl.Type.VOLUME)).setValue(v);
        }
    }

    private void unmute(Clip clip) {
        if (clip.isControlSupported(BooleanControl.Type.MUTE)) {
            ((BooleanControl) clip.getControl(BooleanControl.Type.MUTE)).setValue(false);
        }
    }

    private float clamp(float v) {
        return Math.max(0f, Math.min(1f, v));
    }
}