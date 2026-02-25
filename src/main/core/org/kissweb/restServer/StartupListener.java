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

        // 1. Root logger and console handler configuration
        java.util.logging.Logger rootLogger = java.util.logging.Logger.getLogger("");
        rootLogger.setLevel(java.util.logging.Level.INFO);

        // Set ConsoleHandler level to INFO
        for (java.util.logging.Handler handler : rootLogger.getHandlers()) {
            if (handler instanceof java.util.logging.ConsoleHandler) {
                handler.setLevel(java.util.logging.Level.INFO);
            }
        }

        // 2. Keep startup and lifecycle loggers at INFO for normal startup messages
        java.util.logging.Logger.getLogger("org.apache.catalina.startup").setLevel(java.util.logging.Level.INFO);
        java.util.logging.Logger.getLogger("org.apache.catalina.startup.Catalina").setLevel(java.util.logging.Level.INFO);
        java.util.logging.Logger.getLogger("org.apache.catalina.startup.VersionLoggerListener").setLevel(java.util.logging.Level.INFO);
        java.util.logging.Logger.getLogger("org.apache.catalina.util.LifecycleBase").setLevel(java.util.logging.Level.INFO);
        java.util.logging.Logger.getLogger("org.apache.catalina.core.StandardService").setLevel(java.util.logging.Level.INFO);
        java.util.logging.Logger.getLogger("org.apache.catalina.core.StandardEngine").setLevel(java.util.logging.Level.INFO);
        java.util.logging.Logger.getLogger("org.apache.catalina.core.AprLifecycleListener").setLevel(java.util.logging.Level.INFO);

        // 3. Reduce noise from Catalina and Coyote - set parent loggers to WARNING
        java.util.logging.Logger.getLogger("org.apache.catalina").setLevel(java.util.logging.Level.WARNING);
        java.util.logging.Logger.getLogger("org.apache.coyote").setLevel(java.util.logging.Level.WARNING);
        java.util.logging.Logger.getLogger("org.apache.catalina.core").setLevel(java.util.logging.Level.WARNING);
        java.util.logging.Logger.getLogger("org.apache.coyote.http2").setLevel(java.util.logging.Level.WARNING);

        // 4. Suppress common client aborted/bad header/reset noise - set to SEVERE
        // These log messages are typically client-side issues and create unnecessary log clutter
        java.util.logging.Logger.getLogger("org.apache.coyote.http11.Http11Processor").setLevel(java.util.logging.Level.SEVERE);
        java.util.logging.Logger.getLogger("org.apache.coyote.http11.Http11InputBuffer").setLevel(java.util.logging.Level.SEVERE);
        java.util.logging.Logger.getLogger("org.apache.coyote.http2.Http2UpgradeHandler").setLevel(java.util.logging.Level.SEVERE);
        java.util.logging.Logger.getLogger("org.apache.coyote.http2.Http2AsyncUpgradeHandler").setLevel(java.util.logging.Level.SEVERE);
        java.util.logging.Logger.getLogger("org.apache.catalina.core.StandardWrapperValve").setLevel(java.util.logging.Level.SEVERE);
        java.util.logging.Logger.getLogger("org.apache.coyote.http2.Stream").setLevel(java.util.logging.Level.SEVERE);
        java.util.logging.Logger.getLogger("org.apache.catalina.connector").setLevel(java.util.logging.Level.SEVERE);
        java.util.logging.Logger.getLogger("com.arahant.servlets.REST").setLevel(java.util.logging.Level.SEVERE);

        // 5. Suppress HTTP/2 parser warnings and disable parent handler inheritance
        // This completely silences HTTP/2 protocol validation warnings from malformed client requests
        java.util.logging.Logger http2ParserLogger = java.util.logging.Logger.getLogger("org.apache.coyote.http2.Http2Parser");
        http2ParserLogger.setLevel(java.util.logging.Level.SEVERE);
        http2ParserLogger.setUseParentHandlers(false);

        // 6. Network endpoint loggers - set to WARNING
        java.util.logging.Logger.getLogger("org.apache.tomcat.util.net.NioEndpoint").setLevel(java.util.logging.Level.WARNING);
        java.util.logging.Logger.getLogger("org.apache.tomcat.util.net.Nio2Endpoint").setLevel(java.util.logging.Level.WARNING);
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
