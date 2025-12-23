package org.kissweb.restServer;

import jakarta.servlet.AsyncContext;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Author: Blake McBride
 * Date: 11/24/19
 *
 * This class processes the incoming REST event queue.
 */
class QueueManager {
    private static final Logger logger = LogManager.getLogger(QueueManager.class);
    private final ExecutorService pool;

    QueueManager(int maxThreads) {
        pool = Executors.newFixedThreadPool(maxThreads);
    }

    void add(HttpServletRequest request, HttpServletResponse response, ServletOutputStream out) {
        pool.execute(new ProcessServlet(new Packet(request, response, out)));
    }

    /**
     * Gracefully shutdown the thread pool executor.
     * This method should be called when the servlet is being destroyed.
     */
    void shutdown() {
        if (pool != null && !pool.isShutdown()) {
            try {
                logger.info("Shutting down QueueManager thread pool...");
                pool.shutdown(); // Disable new tasks from being submitted
                
                // Wait for existing tasks to terminate
                if (!pool.awaitTermination(5, TimeUnit.SECONDS)) {
                    logger.warn("Thread pool did not terminate gracefully, forcing shutdown...");
                    pool.shutdownNow(); // Cancel currently executing tasks
                    
                    // Wait a bit for tasks to respond to being cancelled
                    if (!pool.awaitTermination(2, TimeUnit.SECONDS)) {
                        logger.error("Thread pool did not terminate after forced shutdown");
                    }
                }
                logger.info("QueueManager thread pool shutdown complete");
            } catch (InterruptedException ie) {
                logger.error("Thread pool shutdown interrupted", ie);
                pool.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
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
