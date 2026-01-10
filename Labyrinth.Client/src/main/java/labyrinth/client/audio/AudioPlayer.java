package labyrinth.client.audio;

import javax.sound.sampled.*;
import java.io.BufferedInputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Zentraler Audio-Manager (Singleton) fuer Musik & Soundeffekte.
 *
 * Anforderungen:
 * - Globale Lautstaerken (Music/SFX)
 * - Hintergrundmusik im MainMenu (Playlist: RiddleSound1, RiddleSound2, ...)
 * - Beim GameOver: Applaus einmal abspielen, danach wieder MainMenu-Playlist
 * - Alle Audio-Logik ist in dieser Klasse gekapselt
 */
public final class AudioPlayer {

    /** First track of the menu playlist (used as fallback). */
    public static final String MENU_MUSIC_PATH = "/sounds/BackgroundSound1.wav";
    public static final String GAMEOVER_APPLAUSE_PATH = "/sounds/Applause.wav";

    /**
     * Automatically built playlist: /sounds/BackgroundSound1.wav, /sounds/BackgroundSound2.wav, ...
     * Stops at the first missing resource. (Max 99 tracks to avoid endless loops.)
     */
    private static final String[] MENU_PLAYLIST =
            buildSequentialPlaylist("/sounds/BackgroundSound", 1, ".wav", 99);

    private static AudioPlayer instance;

    // Background music
    private Clip musicClip;

    // Music state
    private enum MusicMode { NONE, SINGLE, PLAYLIST }
    private MusicMode musicMode = MusicMode.NONE;

    // Single-track state
    private String currentMusicPath;
    private boolean currentMusicLoop;

    // Playlist state
    private String[] currentPlaylist;
    private int playlistIndex;
    private boolean playlistLoop;

    /**
     * Music token used to invalidate LineListener callbacks when music is stopped/overridden.
     * Every stop/replace increments the token; listeners only act if their token is still current.
     */
    private final AtomicLong musicToken = new AtomicLong(0);

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

    /** Starts the menu background music playlist (RiddleSound1, RiddleSound2, ...) in a loop. */
    public void playMenuMusic() {
        if (MENU_PLAYLIST.length > 0) {
            playMusicPlaylist(MENU_PLAYLIST, true);
        } else {
            // Fallback if no sequential resources were found at runtime.
            playMusic(MENU_MUSIC_PATH, true);
        }
    }

    /**
     * GameOver behaviour:
     * - stop current music (if any)
     * - play applause once
     * - after applause finishes, start menu music playlist looping
     */
    public void playGameOverSequence() {
        final long token = resumeToken.incrementAndGet();

        // Always stop music for a clean applause moment.
        stopMusic();

        playSfxOnceInternal(GAMEOVER_APPLAUSE_PATH,getSfxVolume(), () -> {
            // Only resume if nothing newer cancelled/overrode this sequence.
            if (resumeToken.get() != token) return;
            playMenuMusic();
        });
    }

    // ========================= MUSIC =========================

    /**
     * Plays a single music track. If {@code loop} is true, it loops continuously.
     */
    public synchronized void playMusic(String path, boolean loop) {
        System.out.println("[AudioPlayer] playMusic called: " + path + ", loop=" + loop);

        // No-op if already playing the same track with same loop mode.
        if (musicMode == MusicMode.SINGLE
                && musicClip != null && musicClip.isOpen()
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

        // Stop any current music + invalidate old listeners.
        invalidateAndStopMusicLocked();

        // Establish state for single-track mode.
        musicMode = MusicMode.SINGLE;
        currentMusicPath = path;
        currentMusicLoop = loop;

        try {
            openAndStartMusicClipLocked(path, loop, null, -1);
            System.out.println("[AudioPlayer] Music started successfully!");
        } catch (Exception e) {
            System.err.println("[AudioPlayer] Error playing music: " + e.getMessage());
            e.printStackTrace();
            // Make sure state is clean after failure.
            invalidateAndStopMusicLocked();
        }
    }

    /**
     * Plays a playlist sequentially (track 0, then 1, ...).
     * When {@code loopPlaylist} is true, it continues from the beginning after the last track.
     */
    public synchronized void playMusicPlaylist(String[] paths, boolean loopPlaylist) {
        System.out.println("[AudioPlayer] playMusicPlaylist called: "
                + (paths == null ? "null" : Arrays.toString(paths))
                + ", loopPlaylist=" + loopPlaylist);

        if (paths == null || paths.length == 0) {
            System.err.println("[AudioPlayer] Playlist is empty; nothing to play.");
            return;
        }

        // No-op if already playing the same playlist with same loop mode.
        if (musicMode == MusicMode.PLAYLIST
                && currentPlaylist != null
                && Arrays.equals(currentPlaylist, paths)
                && playlistLoop == loopPlaylist
                && musicClip != null && musicClip.isOpen()) {

            if (!musicClip.isRunning()) {
                System.out.println("[AudioPlayer] Resuming playlist at index " + playlistIndex);
                musicClip.start();
            } else {
                System.out.println("[AudioPlayer] Playlist already playing, skipping");
            }
            return;
        }

        // Stop any current music + invalidate old listeners.
        invalidateAndStopMusicLocked();

        // Establish playlist state.
        musicMode = MusicMode.PLAYLIST;
        currentPlaylist = Arrays.copyOf(paths, paths.length);
        playlistLoop = loopPlaylist;
        playlistIndex = 0;

        try {
            openAndStartMusicClipLocked(currentPlaylist[playlistIndex], false, musicToken.get(), playlistIndex);
            System.out.println("[AudioPlayer] Playlist started successfully!");
        } catch (Exception e) {
            System.err.println("[AudioPlayer] Error starting playlist: " + e.getMessage());
            e.printStackTrace();
            invalidateAndStopMusicLocked();
        }
    }

    public synchronized void stopMusic() {
        invalidateAndStopMusicLocked();
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

    private void playSfxOnceInternal(String path,float volume, Runnable onFinished) {

        if (volume <= 0) {
            if (onFinished != null) onFinished.run();
            return;
        }

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
                applyVolume(clipRef, volume);
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

    private synchronized void invalidateAndStopMusicLocked() {
        // Invalidate all existing music listeners immediately.
        musicToken.incrementAndGet();

        if (musicClip != null) {
            try { musicClip.stop(); } catch (Exception ignored) {}
            try { musicClip.close(); } catch (Exception ignored) {}
            musicClip = null;
        }

        musicMode = MusicMode.NONE;
        currentMusicPath = null;
        currentMusicLoop = false;

        currentPlaylist = null;
        playlistIndex = 0;
        playlistLoop = false;
    }

    private void openAndStartMusicClipLocked(String path, boolean loop, Long playlistToken, int playlistTrackIndex) throws Exception {
        if (path == null) throw new IllegalArgumentException("Music path is null.");

        URL url = getClass().getResource(path);
        if (url == null) {
            throw new IllegalArgumentException("Music not found: " + path);
        }

        AudioInputStream in = AudioSystem.getAudioInputStream(new BufferedInputStream(url.openStream()));
        AudioInputStream pcm = toPlayablePcm(in);

        musicClip = AudioSystem.getClip();
        musicClip.open(pcm);

        applyVolume(musicClip, musicVolume);
        unmute(musicClip);

        // For playlist mode, attach a listener to advance to the next track after natural end.
        if (playlistToken != null) {
            final long token = playlistToken;
            final int finishedIndex = playlistTrackIndex;
            final Clip clipRef = musicClip;

            clipRef.addLineListener(ev -> {
                if (ev.getType() != LineEvent.Type.STOP) return;

                // Only act if this callback still belongs to the active music session.
                if (musicToken.get() != token) return;

                // Ignore STOP events that are not the natural end (e.g., manual stop or interruptions).
                if (clipRef.getFramePosition() < clipRef.getFrameLength()) return;

                // Advance in a separate thread to avoid doing heavy work on the audio event thread.
                new Thread(() -> advancePlaylistAfterTrackEnd(token, finishedIndex), "MusicPlaylistThread").start();
            });
        }

        if (loop) {
            musicClip.loop(Clip.LOOP_CONTINUOUSLY);
        } else {
            musicClip.start();
        }
    }

    private void advancePlaylistAfterTrackEnd(long token, int finishedIndex) {
        synchronized (this) {
            if (musicToken.get() != token) return;
            if (musicMode != MusicMode.PLAYLIST) return;
            if (currentPlaylist == null || currentPlaylist.length == 0) return;

            int nextIndex = finishedIndex + 1;
            if (nextIndex >= currentPlaylist.length) {
                if (playlistLoop) nextIndex = 0;
                else {
                    // Playlist finished; stop cleanly.
                    invalidateAndStopMusicLocked();
                    return;
                }
            }

            playlistIndex = nextIndex;

            // Close the finished clip without invalidating the token (same session).
            if (musicClip != null) {
                try { musicClip.close(); } catch (Exception ignored) {}
                musicClip = null;
            }

            try {
                openAndStartMusicClipLocked(currentPlaylist[playlistIndex], false, token, playlistIndex);
            } catch (Exception e) {
                System.err.println("[AudioPlayer] Error advancing playlist: " + e.getMessage());
                e.printStackTrace();
                invalidateAndStopMusicLocked();
            }
        }
    }

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

    private static String[] buildSequentialPlaylist(String prefix, int startIndex, String ext, int maxCount) {
        List<String> list = new ArrayList<>();
        for (int i = startIndex; i < startIndex + maxCount; i++) {
            String candidate = prefix + i + ext;
            URL url = AudioPlayer.class.getResource(candidate);
            if (url == null) break;
            list.add(candidate);
        }
        return list.toArray(new String[0]);
    }
}
