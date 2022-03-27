package org.kissweb.restServer;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

/**
 * Author: Blake McBride
 * Date: 4/17/20
 */
public class StartupListener implements ServletContextListener {

    private static final Logger logger = Logger.getLogger(StartupListener.class);

    /**
     * This is the first method that gets called.  It is where the whole process starts.
     */
    @Override
    public void contextInitialized(ServletContextEvent sce) {
        logger.setLevel(Level.ALL);
        logger.info("* * * INITIALIZING APPLICATION * * *");
        MainServlet.initializeSystem(sce.getServletContext().getRealPath("/"));
        logger.info("* * * APPLICATION STARTED * * *");
    }

    /**
     * This method gets called when the server is shut down.  It is the last method to be run.
     */
    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        logger.info("* * * APPLICATION STOPPED * * *");
        MainServlet.stopCron();
        System.exit(0);
    }
}
