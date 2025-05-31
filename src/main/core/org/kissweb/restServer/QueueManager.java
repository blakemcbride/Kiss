package org.kissweb.restServer;

import jakarta.servlet.AsyncContext;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Author: Blake McBride
 * Date: 11/24/19
 *
 * This class processes the incoming REST event queue.
 */
class QueueManager {
    private final ExecutorService pool;

    QueueManager(int maxThreads) {
        pool = Executors.newFixedThreadPool(maxThreads);
    }

    void add(HttpServletRequest request, HttpServletResponse response, ServletOutputStream out) {
        pool.execute(new ProcessServlet(new Packet(request, response, out)));
    }

    static class Packet {
        AsyncContext asyncContext;
        ServletOutputStream out;

        Packet(HttpServletRequest request, HttpServletResponse response, ServletOutputStream out) {
            this.out = out;
            asyncContext = request.startAsync(request, response);
        }
    }
}
