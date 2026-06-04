package org.kissweb.restServer;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;

import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletContextListener;

/**
 * Author: Blake McBride
 * Date: 4/17/20
 */
public class StartupListener implements ServletContextListener {

    /**
     * Constructs a new StartupListener instance.
     */
    public StartupListener() {
    }

    private static final Logger logger = LogManager.getLogger(StartupListener.class);

    /**
     * Strong references to every java.util.logging logger we reconfigure.
     *
     * <p>{@link java.util.logging.LogManager} holds loggers through weak
     * references, so a {@code Logger.getLogger(name).setLevel(...)} whose
     * result is not retained can be garbage-collected at any time.  When
     * that happens the level we set is lost and the logger is recreated at
     * its default level, so the noise we silenced at startup reappears
     * hours or days later.  Holding the references here for the lifetime
     * of the JVM keeps the levels in force.
     */
    private static final java.util.List<java.util.logging.Logger> HELD_LOGGERS = new java.util.ArrayList<>();

    /**
     * Set {@code name}'s java.util.logging level and retain a strong
     * reference to the logger so the setting cannot be lost to GC (see
     * {@link #HELD_LOGGERS}).
     */
    private static java.util.logging.Logger quiet(String name, java.util.logging.Level level) {
        java.util.logging.Logger l = java.util.logging.Logger.getLogger(name);
        l.setLevel(level);
        HELD_LOGGERS.add(l);
        return l;
    }

    /**
     * Drops a small, fixed set of Tomcat connector/parser log records that
     * are produced by hostile or broken HTTP clients --- port scanners, TLS
     * spoken to the plaintext port, malformed request lines, abusive HTTP/2
     * frames, invalid cookies, client aborts --- and that carry no action
     * for a Kiss application.
     *
     * <p>This is intentionally a <em>content</em> filter, not a level cut:
     * it drops only records whose message (or thrown-exception chain)
     * matches one of the known-junk signatures, at <em>any</em> level.
     * Every other record passes through unchanged --- including genuine
     * Tomcat warnings from the very same components, and all application
     * logging (which uses Log4j2, not java.util.logging, so it never
     * reaches this filter at all).
     *
     * <p>Matching is by substring against messages that are stable across
     * Tomcat 11.x.  If Tomcat ever rewords a message the worst case is that
     * one noise line reappears --- never that a meaningful message is lost.
     */
    private static final class TomcatNoiseFilter implements java.util.logging.Filter {

        // Any filter already present on the handler; consulted so we add to,
        // rather than replace, existing filtering.
        private final java.util.logging.Filter delegate;

        TomcatNoiseFilter(java.util.logging.Filter delegate) {
            this.delegate = delegate;
        }

        private static final String[] NOISE = {
            "Invalid character found in method name",       // TLS / junk bytes to :80
            "Invalid character found in the request target", // scanner URLs
            "Invalid character found in the request line",
            "Invalid character found in the HTTP protocol",  // RTSP and similar probes
            "Error parsing HTTP request header",
            "Client sent more data than stream window allowed",
            "Total header size too big",
            "Closed due to error",                           // HTTP/2 stream/connection abort
            "that contained an invalid cookie",              // Cookie.logInvalidHeader
            "was present in the Cookie value",               // invalid cookie character
            "ClientAbortException",                          // client hung up mid-response
            "Broken pipe",
            "Connection reset by peer",
        };

        @Override
        public boolean isLoggable(java.util.logging.LogRecord record) {
            if (isNoise(record)) {
                return false;
            }
            return delegate == null || delegate.isLoggable(record);
        }

        private static boolean isNoise(java.util.logging.LogRecord record) {
            if (contains(record.getMessage())) {
                return true;
            }
            for (Throwable t = record.getThrown(); t != null; t = t.getCause()) {
                if (contains(t.getMessage())) {
                    return true;
                }
            }
            return false;
        }

        private static boolean contains(String s) {
            if (s == null) {
                return false;
            }
            for (String n : NOISE) {
                if (s.contains(n)) {
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * Install {@link TomcatNoiseFilter} on a handler, chaining any filter
     * already present so existing filtering is preserved.  Idempotent.
     */
    private static void installNoiseFilter(java.util.logging.Handler handler) {
        java.util.logging.Filter existing = handler.getFilter();
        if (existing instanceof TomcatNoiseFilter) {
            return;
        }
        handler.setFilter(new TomcatNoiseFilter(existing));
    }

    /**
     * This is the first method that gets called.  It is where the whole process starts.
     */
    @Override
    public void contextInitialized(ServletContextEvent sce) {
        configTomcatLogger();
        configC3P0Logger();
        Configurator.setLevel(logger, Level.ALL);
        logger.info("* * * INITIALIZING APPLICATION * * *");
        MainServlet.initializeSystem(sce.getServletContext().getRealPath("/"));
        UserCache.startAutoPurge();
        logger.info("* * * APPLICATION STARTED * * *");
    }

    private static void configTomcatLogger() {
        // ========================================
        // Configure Tomcat Logging Levels
        // ========================================
        // This section programmatically configures java.util.logging levels for Tomcat
        // to reduce log noise while preserving important startup and error messages.
        // Parent loggers must be configured before child loggers to ensure proper inheritance.

        // NOTE: every logger configured below is routed through quiet(),
        // which retains a strong reference so the level survives GC (see
        // HELD_LOGGERS).  Plain Logger.getLogger(name).setLevel(...) calls
        // here are unreliable: the logger is weakly held, gets collected,
        // and the noise comes back hours later.

        // 1. Root logger + handlers.  Install the noise filter on every root
        //    handler so the specific meaningless records (see TomcatNoiseFilter)
        //    are dropped from both the console (catalina.out) and the file
        //    logs --- regardless of their level --- while every other record,
        //    including real Tomcat warnings and all application logging,
        //    passes through untouched.
        java.util.logging.Logger rootLogger = quiet("", java.util.logging.Level.INFO);
        for (java.util.logging.Handler handler : rootLogger.getHandlers()) {
            if (handler instanceof java.util.logging.ConsoleHandler) {
                handler.setLevel(java.util.logging.Level.INFO);
            }
            installNoiseFilter(handler);
        }

        // 2. Keep startup and lifecycle loggers at INFO for normal startup messages
        quiet("org.apache.catalina.startup", java.util.logging.Level.INFO);
        quiet("org.apache.catalina.startup.Catalina", java.util.logging.Level.INFO);
        quiet("org.apache.catalina.startup.VersionLoggerListener", java.util.logging.Level.INFO);
        quiet("org.apache.catalina.util.LifecycleBase", java.util.logging.Level.INFO);
        quiet("org.apache.catalina.core.StandardService", java.util.logging.Level.INFO);
        quiet("org.apache.catalina.core.StandardEngine", java.util.logging.Level.INFO);
        quiet("org.apache.catalina.core.AprLifecycleListener", java.util.logging.Level.INFO);

        // 3. Reduce noise from Catalina and Coyote - set parent loggers to WARNING
        quiet("org.apache.catalina", java.util.logging.Level.WARNING);
        quiet("org.apache.coyote", java.util.logging.Level.WARNING);
        quiet("org.apache.catalina.core", java.util.logging.Level.WARNING);
        quiet("org.apache.coyote.http2", java.util.logging.Level.WARNING);

        // 4. The specific, meaningless connector/parser records (malformed
        //    request lines, TLS spoken to the plaintext port, abusive HTTP/2
        //    frames, invalid cookies from bots, client aborts) are dropped by
        //    CONTENT via the TomcatNoiseFilter installed on the root handlers
        //    in step 1 --- NOT by cranking whole loggers to SEVERE.  A
        //    level-based cut would also hide any genuine warning these
        //    components raise; the filter drops only the known-junk records
        //    and lets every real warning through.  The loggers are therefore
        //    left at their default levels here.

        // 5. Network endpoint loggers - set to WARNING
        quiet("org.apache.tomcat.util.net.NioEndpoint", java.util.logging.Level.WARNING);
        quiet("org.apache.tomcat.util.net.Nio2Endpoint", java.util.logging.Level.WARNING);
    }

    private static void configC3P0Logger() {
        // ========================================
        // Configure C3P0 Connection Pool Logging Levels
        // ========================================
        // This section programmatically configures SLF4J logging levels for C3P0
        // to suppress verbose INFO messages about connection pool initialization while
        // preserving important WARNING and ERROR messages.
        //
        // C3P0 uses SLF4J for logging (as indicated by "MLog clients using slf4j logging"),
        // so we configure it using System properties which SLF4J Simple Logger uses.
        //
        // Suppressed INFO messages:
        //   - "MLog clients using slf4j logging"
        //   - "Initializing c3p0-0.11.2..."
        //   - "Initializing c3p0 pool... [long pool config output]"
        //
        // Note: This must be called early in startup, before C3P0 initialization.
        // SLF4J Simple Logger reads these properties on first use.

        System.setProperty("org.slf4j.simpleLogger.log.com.mchange", "warn");
        System.setProperty("com.mchange.v2.c3p0.management.ManagementCoordinator", "com.mchange.v2.c3p0.management.NullManagementCoordinator");
    }

    /**
     * This method gets called when the server is shut down.  It is the last method to be run.
     */
    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        UserCache.stopAutoPurge();
        MainServlet.stopCron();
        MainServlet.cleanupDatabaseResources();
        logger.info("* * * APPLICATION STOPPED * * *");
    }
}
