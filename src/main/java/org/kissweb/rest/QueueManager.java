package org.kissweb.rest;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.servlet.AsyncContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;

/**
 * Author: Blake McBride
 * Date: 11/24/19
 *
 * This class processes the incoming REST event queue.
 */
class QueueManager {
    private static final Logger logger = Logger.getLogger(QueueManager.class);
    private final BlockingDeque<Packet> queue;
    private final ExecutorService pool;

    QueueManager(int maxThreads) {
        logger.setLevel(Level.ALL);
        logger.trace("new 1");
        queue = new LinkedBlockingDeque<>();
        pool = Executors.newFixedThreadPool(maxThreads);
        logger.trace("new 2");
        Dispatcher dispatcher = new Dispatcher();
        Thread thread = new Thread(dispatcher);
        thread.start();
        logger.trace("new 3");
    }

    void add(HttpServletRequest request, HttpServletResponse response) {
        queue.addLast(new Packet(request, response));
    }

    private class Dispatcher implements Runnable {

        @Override
        public void run() {
            while (true) {
                try {
                    logger.trace("run 1");
                    Packet packet = queue.takeFirst();
                    logger.trace("run 2");
                    Runnable r = new ProcessServlet(packet);
                    logger.trace("run 3");
                    pool.execute(r);
                    logger.trace("run 4");
                } catch (InterruptedException e) {
                    break;
                }
            }
        }
    }

    static class Packet {
        AsyncContext asyncContext;

        Packet(HttpServletRequest request, HttpServletResponse response) {
            asyncContext = request.startAsync(request, response);
        }
    }
}
