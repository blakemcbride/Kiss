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
        Configurator.setLevel(logger, Level.ALL);
        logger.info("* * * INITIALIZING APPLICATION * * *");
        MainServlet.initializeSystem(sce.getServletContext().getRealPath("/"));
        UserCache.startAutoPurge();
        logger.info("* * * APPLICATION STARTED * * *");
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
