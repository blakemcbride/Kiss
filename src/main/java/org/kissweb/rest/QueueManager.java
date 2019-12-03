package org.kissweb.rest;


import org.apache.log4j.Logger;

import javax.servlet.AsyncContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
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
        queue = new LinkedBlockingDeque<>();
        pool = Executors.newFixedThreadPool(maxThreads);
        Dispatcher dispatcher = new Dispatcher();
        Thread thread = new Thread(dispatcher);
        thread.start();
    }

    void add(HttpServletRequest request, HttpServletResponse response, PrintWriter out) {
        queue.addLast(new Packet(request, response, out));
    }

    private class Dispatcher implements Runnable {

        @Override
        public void run() {
            while (true) {
                try {
                    Packet packet = queue.takeFirst();
                    Runnable r = new ProcessServlet(packet);
                    pool.execute(r);
                } catch (InterruptedException e) {
                    break;
                }
            }
        }
    }

    static class Packet {
        AsyncContext asyncContext;
        PrintWriter out;

        Packet(HttpServletRequest request, HttpServletResponse response, PrintWriter out) {
            this.out = out;
            asyncContext = request.startAsync(request, response);
        }
    }
}
