package org.kissweb.rest;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;


/**
 * Author: Blake McBride
 * Date: 5/4/18
 */
@WebServlet(urlPatterns="/rest", asyncSupported = true)
@MultipartConfig
public class MainServlet extends HttpServlet {

    private static final Logger logger = Logger.getLogger(MainServlet.class);

    /**
     * Max number of HTTP requests that can be processed at one time.
     * Any additional ones get put on a queue.
     */
    private static final int MAX_THREADS = 40;

    private QueueManager queueManager;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        logger.setLevel(Level.ALL);
        logger.trace("doPost 1");

        if (queueManager == null) {
            logger.trace("doPost 2");
            queueManager = new QueueManager(MAX_THREADS);
            ServiceBase.setApplicationPath(request);
        }

        queueManager.add(request, response);
        logger.trace("doPost 4");
    }


}
