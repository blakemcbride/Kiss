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

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        logger.setLevel(Level.ALL);
        logger.info("* * * START CONTEXT INITIALIZED * * *");
        MainServlet.initializeSystem(sce.getServletContext().getRealPath("/"));
        logger.info("* * * END CONTEXT INITIALIZED * * *");
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        logger.info("* * * CONTEXT DESTROYED * * *");
        MainServlet.stopCron();
        System.exit(0);
    }
}
