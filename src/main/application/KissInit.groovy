import org.kissweb.database.Connection
import org.kissweb.rest.MainServlet
import org.kissweb.rest.UserCache

class KissInit {

    static void init() {
        MainServlet.setConnectionType Connection.ConnectionType.PostgreSQL
        MainServlet.setHost "localhost"
        MainServlet.setDatabase ""
        MainServlet.setUser "postgres"            // database user (not application user login)
        MainServlet.setPassword "postgres"        // database password (not application user password)
        MainServlet.setMaxWorkerThreads 30        // max number of simultaneous REST services (any additional are put on a queue)

        UserCache.setInactiveUserMaxSeconds 900   // seconds user is allowed to be idle before auto-logoff

        MainServlet.setDebug true                // if true print debug info
    }
}
