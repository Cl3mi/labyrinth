package labyrinth.client.audio;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.sound.sampled.*;
import java.io.BufferedInputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;


public final class AudioPlayer {

    private static final Logger log = LoggerFactory.getLogger(AudioPlayer.class);

    public static final String MENU_MUSIC_PATH = "/sounds/BackgroundSound1.wav";
    public static final String GAMEOVER_APPLAUSE_PATH = "/sounds/Applause.wav";


    private static final String[] MENU_PLAYLIST =
            buildSequentialPlaylist("/sounds/BackgroundSound", 1, ".wav", 99);

    private static AudioPlayer instance;


    private Clip musicClip;


    private enum MusicMode { NONE, SINGLE, PLAYLIST }
    private MusicMode musicMode = MusicMode.NONE;

    private String currentMusicPath;
    private boolean currentMusicLoop;

    private String[] currentPlaylist;
    private int playlistIndex;
    private boolean playlistLoop;


    private final AtomicLong musicToken = new AtomicLong(0);


    private float musicVolume = 1.0f;
    private float sfxVolume = 1.0f;

    private final AtomicLong resumeToken = new AtomicLong(0);

    private AudioPlayer() {}

    public static synchronized AudioPlayer getInstance() {
        if (instance == null) instance = new AudioPlayer();
        return instance;
    }

    public void playMenuMusic() {
        if (MENU_PLAYLIST.length > 0) {
            playMusicPlaylist(MENU_PLAYLIST, true);
        } else {
            // Fallback if no sequential resources were found at runtime.
            playMusic(MENU_MUSIC_PATH, true);
        }
    }

    public void playGameOverSequence() {
        final long token = resumeToken.incrementAndGet();
        stopMusic();

        playSfxOnceInternal(GAMEOVER_APPLAUSE_PATH,getSfxVolume(), () -> {
            if (resumeToken.get() != token) return;
            playMenuMusic();
        });
    }

    public synchronized void playMusic(String path, boolean loop) {
        log.info("[AudioPlayer] playMusic called: " + path + ", loop=" + loop);

        if (musicMode == MusicMode.SINGLE
                && musicClip != null && musicClip.isOpen()
                && path != null && path.equals(currentMusicPath)
                && loop == currentMusicLoop) {

            if (!musicClip.isRunning()) {
                log.info("[AudioPlayer] Resuming existing clip");
                if (loop) musicClip.loop(Clip.LOOP_CONTINUOUSLY);
                else musicClip.start();
            } else {
                log.info("[AudioPlayer] Already playing, skipping");
            }
            return;
        }

        invalidateAndStopMusicLocked();

        musicMode = MusicMode.SINGLE;
        currentMusicPath = path;
        currentMusicLoop = loop;

        try {
            openAndStartMusicClipLocked(path, loop, null, -1);
            log.info("[AudioPlayer] Music started successfully!");
        } catch (Exception e) {
            log.error("[AudioPlayer] Error playing music: " + e.getMessage());
            e.printStackTrace();
            // Make sure state is clean after failure.
            invalidateAndStopMusicLocked();
        }
    }

    public synchronized void playMusicPlaylist(String[] paths, boolean loopPlaylist) {
        log.info("[AudioPlayer] playMusicPlaylist called: "
                + (paths == null ? "null" : Arrays.toString(paths))
                + ", loopPlaylist=" + loopPlaylist);

        if (paths == null || paths.length == 0) {
            log.error("[AudioPlayer] Playlist is empty; nothing to play.");
            return;
        }

        if (musicMode == MusicMode.PLAYLIST
                && currentPlaylist != null
                && Arrays.equals(currentPlaylist, paths)
                && playlistLoop == loopPlaylist
                && musicClip != null && musicClip.isOpen()) {

            if (!musicClip.isRunning()) {
                log.info("[AudioPlayer] Resuming playlist at index " + playlistIndex);
                musicClip.start();
            } else {
                log.info("[AudioPlayer] Playlist already playing, skipping");
            }
            return;
        }
        invalidateAndStopMusicLocked();

        musicMode = MusicMode.PLAYLIST;
        currentPlaylist = Arrays.copyOf(paths, paths.length);
        playlistLoop = loopPlaylist;
        playlistIndex = 0;

        try {
            openAndStartMusicClipLocked(currentPlaylist[playlistIndex], false, musicToken.get(), playlistIndex);
            log.info("[AudioPlayer] Playlist started successfully!");
        } catch (Exception e) {
            log.error("[AudioPlayer] Error starting playlist: " + e.getMessage());
            e.printStackTrace();
            invalidateAndStopMusicLocked();
        }
    }

    public synchronized void stopMusic() {
        invalidateAndStopMusicLocked();
        log.info("[AudioPlayer] Music stopped");
    }


    public synchronized void setMusicVolume(float v) {
        musicVolume = clamp(v);
        applyVolume(musicClip, musicVolume);
    }


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
                    log.error("SFX not found: {}", path);
                    return;
                }

                AudioInputStream in = AudioSystem.getAudioInputStream(new BufferedInputStream(url.openStream()));
                AudioInputStream pcm = toPlayablePcm(in);

                c = AudioSystem.getClip();
                c.open(pcm);

                final Clip clipRef = c;
                applyVolume(clipRef, volume);
                unmute(clipRef);

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



    private synchronized void invalidateAndStopMusicLocked() {
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

        if (playlistToken != null) {
            final long token = playlistToken;
            final int finishedIndex = playlistTrackIndex;
            final Clip clipRef = musicClip;

            clipRef.addLineListener(ev -> {
                if (ev.getType() != LineEvent.Type.STOP) return;

                if (musicToken.get() != token) return;

                if (clipRef.getFramePosition() < clipRef.getFrameLength()) return;

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

            if (musicClip != null) {
                try { musicClip.close(); } catch (Exception ignored) {}
                musicClip = null;
            }

            try {
                openAndStartMusicClipLocked(currentPlaylist[playlistIndex], false, token, playlistIndex);
            } catch (Exception e) {
                log.error("[AudioPlayer] Error advancing playlist: " + e.getMessage());
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


        if (clip.isControlSupported(FloatControl.Type.MASTER_GAIN)) {
            FloatControl g = (FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN);

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
