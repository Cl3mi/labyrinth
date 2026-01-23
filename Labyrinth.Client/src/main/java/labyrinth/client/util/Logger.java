package labyrinth.client.util;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Simple logger for the Labyrinth client.
 * Configure the OUTPUT_MODE below to change where logs are written.
 */
public final class Logger {

    // ========== CONFIGURATION ==========
    // Change this to control where logs are written:
    // CONSOLE - only System.out
    // FILE    - only to file
    // BOTH    - console and file
    private static final OutputMode OUTPUT_MODE = OutputMode.CONSOLE;

    // Log file path (used when OUTPUT_MODE is FILE or BOTH)
    private static final String LOG_FILE = "labyrinth-client.log";
    // ===================================

    public enum OutputMode {
        CONSOLE,
        FILE,
        BOTH
    }

    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static PrintWriter fileWriter;

    static {
        if (OUTPUT_MODE == OutputMode.FILE || OUTPUT_MODE == OutputMode.BOTH) {
            try {
                fileWriter = new PrintWriter(new FileWriter(LOG_FILE, true), true);
            } catch (IOException e) {
                System.err.println("Could not open log file: " + e.getMessage());
            }
        }
    }

    private final String className;

    private Logger(String className) {
        this.className = className;
    }

    public static Logger getLogger(Class<?> clazz) {
        return new Logger(clazz.getSimpleName());
    }

    public void info(String message) {
        log("INFO", message);
    }

    public void info(String format, Object... args) {
        log("INFO", String.format(format, args));
    }

    public void error(String message) {
        log("ERROR", message);
    }

    public void error(String format, Object... args) {
        log("ERROR", String.format(format, args));
    }

    public void error(String message, Throwable t) {
        log("ERROR", message + " - " + t.getMessage());
        if (OUTPUT_MODE == OutputMode.CONSOLE || OUTPUT_MODE == OutputMode.BOTH) {
            t.printStackTrace(System.err);
        }
        if ((OUTPUT_MODE == OutputMode.FILE || OUTPUT_MODE == OutputMode.BOTH) && fileWriter != null) {
            t.printStackTrace(fileWriter);
        }
    }

    private void log(String level, String message) {
        String timestamp = LocalDateTime.now().format(FORMATTER);
        String logLine = String.format("[%s] [%s] [%s] %s", timestamp, level, className, message);

        if (OUTPUT_MODE == OutputMode.CONSOLE || OUTPUT_MODE == OutputMode.BOTH) {
            System.out.println(logLine);
        }

        if ((OUTPUT_MODE == OutputMode.FILE || OUTPUT_MODE == OutputMode.BOTH) && fileWriter != null) {
            fileWriter.println(logLine);
        }
    }
}
