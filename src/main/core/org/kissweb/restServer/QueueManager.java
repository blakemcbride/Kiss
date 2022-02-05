package org.kissweb.restServer;

import javax.servlet.AsyncContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
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

    void add(HttpServletRequest request, HttpServletResponse response, PrintWriter out) {
        pool.execute(new ProcessServlet(new Packet(request, response, out)));
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
