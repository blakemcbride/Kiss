package org.kissweb.restServer;

import org.kissweb.database.Connection;

/**
 * Application-registered hook that prepares the per-request database connection after
 * the request is authenticated and before the web service method executes.
 * <br><br>
 * Register an implementation with {@link MainServlet#setRequestConnectionPreparer(RequestConnectionPreparer)}
 * (typically from <code>KissInit.groovy</code>).  When registered, {@link #prepare} is called for
 * every REST request that carries a database connection — with the authenticated user's
 * {@link UserData}, or <code>null</code> when the method is allowed without authentication and no
 * valid session accompanies the request.  If <code>prepare</code> throws, the request is aborted
 * with an error response and the service method never runs (fail-closed).
 * <br><br>
 * {@link #release} is called when a request connection is closed, before the underlying
 * connection returns to the pool.  Use it to clear any per-request connection state set by
 * <code>prepare</code> (for example, resetting the schema search path) so pooled connections never
 * carry state between consumers.
 * <br><br>
 * Typical use: multi-tenant applications that select a per-tenant database schema (for
 * example, the PostgreSQL <code>search_path</code>) from the authenticated user, keeping every web
 * service tenant-neutral.  Note that connections obtained outside the request path (cron
 * tasks, {@link ProcessServlet#getConnection()}) are not passed to <code>prepare</code>; such
 * consumers must establish any needed connection state themselves.
 */
public interface RequestConnectionPreparer {

    /**
     * Prepare the request's database connection.  Called after authentication and before
     * the web service method runs.
     *
     * @param db the request's database connection (never null)
     * @param userData the authenticated user, or null when the method is allowed without
     *                 authentication and no valid session accompanies the request
     * @throws Exception to abort the request without running the service method
     */
    void prepare(Connection db, UserData userData) throws Exception;

    /**
     * Clear per-request connection state before the underlying connection returns to the
     * pool.  Default: no action.
     *
     * @param db the connection being released
     */
    default void release(Connection db) {
    }
}
