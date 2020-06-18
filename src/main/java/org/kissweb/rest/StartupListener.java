package org.kissweb.rest;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

/**
 * Author: Blake McBride
 * Date: 4/17/20
 */
public class StartupListener implements ServletContextListener {

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        System.out.println("* * * CONTEXT INITIALIZED * * *");
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        System.out.println("* * * CONTEXT DESTROYED * * *");
        System.exit(0);
    }
}
